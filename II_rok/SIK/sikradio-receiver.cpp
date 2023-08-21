#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <cstdint>
#include <thread>
#include <set>
#include <vector>
#include <sys/poll.h>


#include "err.h"
#include "utility.h"
#include "buffor.h"
#include "Radio_station.h"
#include "Discovered_stations.h"

#define D_DISCOVER_ADDR "255.255.255.255"


#define D_UI_PORT 18842
#define D_CTRL_PORT 38842

#define D_BSIZE 65536
#define D_RTIME 250
#define D_NAME "Nienazwany Nadajnik"

#define MAX_REPLY_SIZE 150
#define QUEUE_LENGTH 20
#define TIMEOUT 2500
#define BUF_SIZE 4
#define KEY_SIZE 4

char const *key_mode = "\xFF\xFD\x22";
char key_up[KEY_SIZE] = {27, 91, 65, 0};
char key_down[KEY_SIZE] = {27, 91, 66, 0};
char const *clear = "\033[2J\033[H";

using namespace std;


typedef struct {
    char *DISCOVER_ADDR;

    uint16_t UI_PORT;
    uint16_t CTRL_PORT;

    uint64_t BSIZE;

    uint64_t RTIME;
    string NAME;
} Options;

Options new_options()
{
    Options a;
    a.DISCOVER_ADDR = (char *) D_DISCOVER_ADDR;

    a.CTRL_PORT = D_CTRL_PORT;
    a.UI_PORT = D_UI_PORT;

    a.BSIZE = D_BSIZE;

    a.RTIME = D_RTIME;
    a.NAME = "";
    return a;
}

Options pars_options(int argc, char *argv[])
{
    Options options = new_options();
    int opt;

    static char
            usage[] = "Usage: program [-d DISCOVER_ADDR] [-C CTRL_PORT] [-U UI_PORT] [-b BSIZE] [-R RTIME] [-n NAZWA]\n"
                      "Options:\n"
                      "  -d DISCOVER_ADDR   adres używany do wykrywania aktywnych nadajników. - opcjonalny\n"
                      "  -C CTRL_PORT       port UDP używany do transmisji pakietów kontrolnych - opcjonalny.\n"
                      "  -U UI_PORT         port TCP, na którym udostępniany jest prosty interfejs tekstowy do przełączania się między stacjami - opcjonalny.\n"
                      "  -b BSIZE           rozmiar w bajtach bufora - opcjonalny.\n"
                      "  -R RTIME           czas pomiędzy wysłaniem kolejnych raportów o brakujących paczkach - opcjonalny.\n"
                      "  -n NAZWA           nazwa nadajnika - opcjonalny.\n";

    static char invalid[] = "invalid options\n";

    static char argument[] = "Need argument\n";

    while ((opt = getopt(argc, argv, ":d:C:U:b:R:n:")) != -1)
    {

        switch (opt)
        {
            case 'd':
                options.DISCOVER_ADDR = optarg;
                struct sockaddr_in sa;
                if (!inet_pton(AF_INET, options.DISCOVER_ADDR, &(sa.sin_addr)))
                {
                    exit(EXIT_FAILURE);
                }
                break;
            case 'C':
                is_port_number(optarg);
                options.CTRL_PORT = strtoul(optarg, nullptr, 10);
                break;

            case 'U':
                is_port_number(optarg);
                options.UI_PORT = strtoul(optarg, nullptr, 10) > UINT16_MAX;
                break;


            case 'b':
                options.BSIZE = strtoul(optarg, nullptr, 10);

                if (!is_number(optarg))
                {
                    fprintf(stderr, "parametr b musi buć liczbą\n");
                    exit(EXIT_FAILURE);
                }
                if (!options.BSIZE)
                {
                    fprintf(stderr, "parametr b nie może być równy 0\n");
                    exit(EXIT_FAILURE);
                }
                break;


            case 'R':
                options.RTIME = strtoul(optarg, nullptr, 10);

                if (!is_number(optarg))
                {
                    fprintf(stderr, "parametr R musi buć liczbą\n");
                    exit(EXIT_FAILURE);
                }
                if (!is_number(optarg) || !options.RTIME)
                {
                    fprintf(stderr, "parametr R nie może być równy 0\n");
                    exit(EXIT_FAILURE);
                }
                break;

            case 'n':
                options.NAME = optarg;
                if (!is_valid_name(options.NAME))
                {
                    fprintf(stderr, "Nie poprawna nazwa\n");
                    exit(EXIT_FAILURE);
                }
                break;

            case '?':

                fprintf(stderr, "%s", invalid);
                fprintf(stderr, "%s", usage);
                exit(EXIT_FAILURE);
            case ':':
                fprintf(stderr, "%s", argument);
                fprintf(stderr, "%s", usage);
                exit(EXIT_FAILURE);

            default:

                fprintf(stderr, "%s", usage);
                exit(EXIT_FAILURE);
        }
    }
    return options;
}


/**
 * Funkcja, którą wykonuje wątek wypisujący zawartość bufora. Pobiera on paczkę z bufora, a następnie wypisuje ją.
 */
void play_music(Buffer &buffer, const bool &wypisuj)
{
    auto *buff = new uint8_t[buffer.get_messege_size()];
    while (wypisuj)
    {
        buffer.print_buffer(buff, wypisuj);
        size_t left = buffer.get_messege_size();
        uint8_t *tmp = buff;

        while (left != 0 && wypisuj)
        {
            ssize_t result = write(STDOUT_FILENO, tmp, left);
            if (result < 0)
            {
                exit(1);
            }
            else
            {
                left -= result;
                tmp += result;
            }
        }
    }
    delete[] buff;
}

/**
 * Funkcja, którą wykonuje wątek wysyłający komunikaty LOUDER_PLEASE. Cyklicznie co @param RTIME pobiera z bufora
 * wektor zawierajacy brakujace paczki i o nie prosi.
 */
void send_replays(Buffer &buf, Options &options)
{
    int socket_fd = open_socket_UDP();
    while (true)
    {
        auto missing_packs = buf.get_missing();
        if (!missing_packs.empty())
        {
            string message = "LOUDER_PLEASE ";

            message += to_string(missing_packs[0]);
            for (size_t i = 1; i < missing_packs.size(); ++i)
            {
                message += "," + to_string(missing_packs[i]);
            }
            message += "\n";
            struct sockaddr_in send_address = get_send_address(options.DISCOVER_ADDR, options.CTRL_PORT);
            send_message(socket_fd, &send_address, (char *) message.c_str(), message.size());
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(options.RTIME));
    }
}

/**
 * Funkcja, którą wykonuje wątek szukający aktywnych nadajników. Wysyła komunikat ZERO_SEVEN_COME_IN na adress
 * DISCOVER_ADDR i oczekuje odpowiedzi. Po 5 sekundach aktualizuje listę dostępnych nadjników.
 */
void control_port(Discovered_stations &discovered_stations, Options &options)
{
    char reply[MAX_REPLY_SIZE];
    ssize_t received;
    struct sockaddr_in client_address;
    int socket_fd = open_socket_UDP();
    set_timeout(socket_fd, 0, options.RTIME);


    struct sockaddr_in send_address = get_send_address(options.DISCOVER_ADDR, options.CTRL_PORT);
    while (true)
    {
        string message = "ZERO_SEVEN_COME_IN\n";
        send_message(socket_fd, &send_address, (char *) message.c_str(), message.size());

        received = read_message(socket_fd, &client_address, reply, MAX_REPLY_SIZE);

        while (received > 0)
        {
            auto split = split_string(reply);

            if (pars_replay(split))
            {
                discovered_stations.add_station(split[1], strtoul(split[2], nullptr, 10), string(split[3]));
            }

            free_split_string(split);
            received = read_message(socket_fd, &client_address, reply, MAX_REPLY_SIZE);
        }
        sleep(5);
        discovered_stations.update_station();
    }

}

/**
 * Funkcjia pomocnicza wysyła do każdego klienta podłączonego przez telnet aktualną listę nadajników.
 */
void poll_clear_all(std::vector<pollfd> &poll_descriptors, Discovered_stations &discovered_stations)
{
    char *new_stations = discovered_stations.to_char();
    for (size_t i = 1; i < poll_descriptors.size(); ++i)
    {
        if (poll_descriptors[i].fd != -1)
        {
            send(poll_descriptors[i].fd, clear, strlen(clear), 0);
            send(poll_descriptors[i].fd, new_stations, strlen(new_stations), 0);
        }
    }
    delete[] new_stations;
}

/**
 * Poll obługujacy UI.
 */
void ui_thread(Discovered_stations &discovered_stations, uint16_t ui_port)
{
    std::vector<pollfd> poll_descriptors(1);

    poll_descriptors[0].fd = bind_socket_to_any_TCP(ui_port);

    poll_descriptors[0].events = POLLIN;
    poll_descriptors[0].revents = 0;

    start_listening(poll_descriptors[0].fd, QUEUE_LENGTH);
    char buf[BUF_SIZE];
    while (true)
    {
        if (discovered_stations.has_change_control())
        {
            poll_clear_all(poll_descriptors, discovered_stations);
        }
        for (auto &x: poll_descriptors)
        {
            x.revents = 0;
        }
        int poll_status = poll(poll_descriptors.data(), poll_descriptors.size(), TIMEOUT);
        if (poll_status == -1)
        {
            if (errno == EINTR)
            {
                fprintf(stderr, "Interrupted system call\n");
            }
            else
                PRINT_ERRNO();
        }
        else if (poll_status > 0)
        {
            if (poll_descriptors[0].revents & POLLIN)
            {
                int client_fd = accept_connection(poll_descriptors[0].fd, nullptr);
                CHECK_ERRNO(fcntl(client_fd, F_SETFL, O_NONBLOCK));

                pollfd tmp{};
                tmp.fd = client_fd;
                tmp.events = POLLIN | POLLOUT;
                try
                {
                    poll_descriptors.push_back(tmp);
                }
                catch (exception &e)
                {
                    close(client_fd);
                }
                send_all(client_fd, (char *) key_mode, strlen(key_mode));
                send_all(client_fd, (char *) clear, strlen(clear));


                char *new_stations = discovered_stations.to_char();
                send_all(client_fd, (char *) new_stations, strlen(new_stations));
                delete[] new_stations;

            }

            for (auto it = poll_descriptors.begin() + 1; it != poll_descriptors.end(); ++it)
            {
                if (it->fd != -1 && (it->revents & POLLIN))
                {
                    ssize_t received_bytes = read(it->fd, buf, BUF_SIZE);
                    if (received_bytes < 0)
                    {
                        CHECK_ERRNO(close(it->fd));
                        it = poll_descriptors.erase(it);
                        if (it == poll_descriptors.end())
                        {
                            break;
                        }
                    }
                    else if (received_bytes == 0)
                    {
                        CHECK_ERRNO(close(it->fd));
                        it = poll_descriptors.erase(it);
                        if (it == poll_descriptors.end())
                        {
                            break;
                        }
                    }
                    else
                    {
                        if (strncmp(reinterpret_cast<const char *>(buf), key_up, sizeof(buf)) == 0)
                        {
                            discovered_stations.switch_station(1);
                        }
                        else if (strncmp(reinterpret_cast<const char *>(buf), key_down, sizeof(buf)) == 0)
                        {
                            discovered_stations.switch_station(-1);
                        }
                        poll_clear_all(poll_descriptors, discovered_stations);
                        break;
                    }
                }
            }
        }
    }
}


int main(int argc, char *argv[])
{
    setbuf(stdout, nullptr);
    Options options = pars_options(argc, argv);
    uint64_t current_session_id = 0;
    bool print = false;


    auto *pack = static_cast<char *>(malloc(options.BSIZE));
    if (pack == nullptr)
    {
        return 1;
    }

    ssize_t pack_size;
    Buffer buf;
    Discovered_stations discovered_stations(options.NAME);

    std::thread print_thread;
    std::thread control_sender_thread = std::thread([&]()
                                                    { control_port(discovered_stations, options); });
    std::thread control_ui_thread = std::thread([&]()
                                                { ui_thread(discovered_stations, options.UI_PORT); });

    std::thread louder_thread = std::thread([&]()
                                            { send_replays(buf, options); });


    int socket_fd = -1;
    struct ip_mreq multicast_request{};




    while (true)
    {
        /**
         * Sprawdzma czy zmieniła się stacja nadawcza. Jeżeli się zmieniłą to przełączam lub czek aż pokaże sie stacja z
         * odpowiednia nazwa.
         */
        if (discovered_stations.has_change_reciver())
        {
            discovered_stations.wait_for_station();
            Radio_station current_station = discovered_stations.get_currently_playing();
            if (socket_fd != -1)
            {
                close(socket_fd);
            }

            current_session_id = 0;
            socket_fd = open_socket_UDP();
            set_timeout(socket_fd, 0, options.RTIME);

            multicast_request.imr_interface.s_addr = htonl(INADDR_ANY);
            if (inet_aton(current_station.mcast_addr.c_str(), &multicast_request.imr_multiaddr) == 0)
            {
                fatal("inet_aton - invalid multicast address\n");
            }

            CHECK_ERRNO(setsockopt(socket_fd, IPPROTO_IP, IP_ADD_MEMBERSHIP, (void *) &multicast_request,
                                   sizeof multicast_request));

            bind_socket_to_any(current_station.data_port, socket_fd);
        }

        /**
         * Odbieram paczkę. Zapisuję ją do bufora lub zaczynam grać od początku.
         */
        pack_size = recv(socket_fd, pack, options.BSIZE, 0);

        if (pack_size > static_cast<ssize_t>(2 * sizeof(uint64_t)))
        {
            size_t mes_size = pack_size - 2 * sizeof(uint64_t);

            uint64_t session_id = get_session_id(pack);
            uint64_t current_byte_num = get_byte_num(pack);
            if (session_id > current_session_id)
            {
                if (print)
                {
                    print = false;
                    buf.wake_me();
                    print_thread.join();
                }
                current_session_id = session_id;
                buf = Buffer(current_byte_num, (options.BSIZE / mes_size) * mes_size, mes_size);
                buf.add_buffer(pack);
                print = true;
                print_thread = std::thread([&buf, &print]()
                                           { play_music(buf, print); });
            }
            else if (session_id == current_session_id)
            {
                buf.add_buffer(pack);
            }
        }
    }
}
