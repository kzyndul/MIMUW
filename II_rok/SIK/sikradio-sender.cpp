#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <cstdint>
#include <iostream>

#include <ctime>
#include <sys/types.h>
#include <sys/socket.h>
#include <cstring>
#include <thread>

#include <arpa/inet.h>
#include <string>

#include "err.h"
#include "Fifo.h"
#include "Re_sent.h"


#define UDP_MAX 65507
#define D_PSIZE 512
#define D_DATA_PORT 28842
#define D_NAME "Nienazwany Nadajnik"


#define D_CTRL_PORT 38842
#define D_FSIZE 131072
#define D_RTIME 250


using namespace std;

typedef struct {
    char *MCAST_ADDR;

    uint16_t DATA_PORT;
    uint16_t CTRL_PORT;

    uint64_t PSIZE;
    uint64_t FSIZE;


    uint64_t RTIME;
    string NAZWA;
} Options;

Options new_options()
{
    Options a;
    a.MCAST_ADDR = nullptr;
    a.DATA_PORT = D_DATA_PORT;
    a.CTRL_PORT = D_CTRL_PORT;

    a.FSIZE = D_FSIZE;
    a.PSIZE = D_PSIZE;

    a.RTIME = D_RTIME;
    a.NAZWA = D_NAME;
    return a;
}


Options pars_options(int argc, char *argv[])
{
    Options options = new_options();
    int opt;

    static char usage[] =
            "Usage: program [-a MCAST_ADDR] [-P DATA_PORT] [-C CTRL_PORT] [-p PSIZE] [-f FSIZE] [-R RTIME] [-n NAZWA]\n"
            "Options:\n"
            "  -a MCAST_ADDR  adres rozgłaszania ukierunkowanego - obowiązkowym.\n"
            "  -P DATA_PORT   port UDP używany do przesyłania danych - opcjonalny.\n"
            "  -C CTRL_PORT   port UDP używany do transmisji pakietów kontrolnych - opcjonalny.\n"
            "  -p PSIZE       rozmiar w bajtach pola audio_data paczki - opcjonalny.\n"
            "  -f FSIZE       rozmiar w bajtach kolejki FIFO nadajnika - opcjonalny.\n"
            "  -R RTIME       czas pomiędzy kolejnymi retransmisjami paczek - opcjonalny.\n"
            "  -n NAZWA       nazwa nadajnika - opcjonalny.\n";

    static char invalid[] = "invalid options\n";
    static char argument[] = "Need argument\n";

    while ((opt = getopt(argc, argv, ":a:P:p:n:C:f:R:")) != -1)
    {

        switch (opt)
        {
            case 'a':

                if (!is_multicast_address(optarg))
                {
                    fprintf(stderr, "Nie poprawny adres\n");
                    exit(EXIT_FAILURE);
                }
                options.MCAST_ADDR = optarg;

                break;
            case 'P':
                is_port_number(optarg);
                options.DATA_PORT = strtoul(optarg, nullptr, 10);
                break;

            case 'C':
                is_port_number(optarg);
                options.CTRL_PORT = strtoul(optarg, nullptr, 10);
                break;
            case 'p':
                options.PSIZE = strtoul(optarg, nullptr, 10);

                if (!is_number(optarg))
                {
                    fprintf(stderr, "parametr p musi byc liczba\n");
                    exit(EXIT_FAILURE);
                }
                if (!options.PSIZE)
                {
                    fprintf(stderr, "parametr p nie może być równy 0\n");
                    exit(EXIT_FAILURE);
                }
                if (options.PSIZE > UDP_MAX)
                {
                    fprintf(stderr, "przekroczono rozmiar\n");
                    exit(EXIT_FAILURE);
                }
                break;


            case 'f':
                options.FSIZE = strtoul(optarg, nullptr, 10);
                if (!is_number(optarg))
                {
                    fprintf(stderr, "parametr f musi byc liczba\n");
                    exit(EXIT_FAILURE);
                }


                if (!options.FSIZE)
                {
                    fprintf(stderr, "parametr f nie może być równy 0\n");
                    exit(EXIT_FAILURE);
                }
                break;
            case 'R':
                options.RTIME = strtoul(optarg, nullptr, 10);

                if (!is_number(optarg))
                {
                    fprintf(stderr, "parametr R musi byc liczba\n");
                    exit(EXIT_FAILURE);
                }
                if (!options.RTIME)
                {
                    fprintf(stderr, "%s", usage);
                    fprintf(stderr, "parametr R nie może być równy 0\n");
                    exit(EXIT_FAILURE);
                }
                break;
            case 'n':
                options.NAZWA = optarg;
                if (!is_valid_name(options.NAZWA))
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

    if (options.MCAST_ADDR == nullptr)
    {
        fprintf(stderr, "%s", "missing -a option\n");
        fprintf(stderr, "%s", usage);
        exit(EXIT_FAILURE);
    }
    return options;
}

/**
 * Funkcjia wykonywana przez wątek obsługujący port kontrolny. Dopóki program się nie zakończy wątek nasłuchuje na
 *      adresie 0.0.0.0 na porcie options.CTRL_PORT jeżeli otrzyma komunikat "LOUDER_PLEASE" zapisze numery paczek,
 *      które należy wysłać ponownie. Gdy otrzyma komunikat ZERO_SEVEN_COME_IN odpowie na adres nadawcy komunikatem
 *      ustalonym w treści zadania.
 *
 * @param end           zmienna informująca czy program się zakończył.
 * @param re_sent       struktura danych przechowywująca paczki, ktore zostaną ponownie nadane.
 * @param options       opcje z którymi program został wywołany.
 */
void control_port(bool &end, Re_sent &re_sent, Options &options)
{
    int socket_fd = open_socket_UDP();

    int enable = 1;
    if (setsockopt(socket_fd, SOL_SOCKET, SO_BROADCAST, &enable, sizeof(enable)) < 0)
    {
        std::cerr << "Failed to set socket options." << std::endl;
        exit(EXIT_FAILURE);
    }

    set_timeout(socket_fd, 0, options.RTIME);
    bind_socket_to_any(options.CTRL_PORT, socket_fd);

    struct sockaddr_in client_address;


    char buf[UDP_MAX];
    const char *louder_please = "LOUDER_PLEASE";
    const char *zero_seven_come_in = "ZERO_SEVEN_COME_IN";
    auto data_port = to_string(options.DATA_PORT);
    string response =
            "BOREWICZ_HERE" + string(" ") + string(options.MCAST_ADDR) + " " + data_port + " " + options.NAZWA + "\n";
    const char *reply_response = response.c_str();

    while (!end)
    {
        ssize_t read = read_message(socket_fd, &client_address, buf, UDP_MAX);

        if (read > 0)
        {
            if (strncmp(reinterpret_cast<const char *>(buf), louder_please, strlen(louder_please)) == 0)
            {
                string input = string(buf + 13);
                try
                {
                    auto numbers = string_to_ints(input);
                    re_sent.add_to_copy(numbers);
                }
                catch (const std::exception &e)
                {
                    cerr << "ERROR" << endl;
                }
            }
            else if (strncmp(reinterpret_cast<const char *>(buf), zero_seven_come_in, strlen(zero_seven_come_in)) == 0)
            {
                send_message(socket_fd, &client_address, (char *) reply_response, response.length() + 1);
            }
        }
    }
    re_sent.wake_me();
}

/**
 * Funkcjia wykonywana przez wątek kopiujący z kolejki Fifo do re_sent. Jeżeli nadajnik poprosił o ponowne nadesłanie
 *      jakiś paczek, funkcjia próbuje je skopiować z kolejki FIFO.
 *
 * @param end           zmienna informująca czy program się zakończył.
 * @param fifo          kolejka przechowywyjąca ostatnie FSIZE wysłanych paczek.
 * @param re_sent       przechowuje paczki do ponownego nadania.
 */
void copy_from_fifo(bool &end, Fifo &fifo, Re_sent &re_sent)
{
    char *buf;
    try
    {
        buf = new char[fifo.get_pack_size()];
    }
    catch (exception &e)
    {
        exit(EXIT_FAILURE);
    }
    while (!end)
    {
        auto byte_num = re_sent.pop_to_copy(end);
        if (!end && fifo.copy(buf, byte_num))
        {
            re_sent.save(buf);
        }
    }
    delete[] buf;
}
//Funkcjia pełni funkcjie budzika, co określony czas ustawia zmienną na true.

/**
 * Funkcjia wykonywana przez wątek będący zegarem. Co RTIME zmienia wartość zmiennej @param retransmit na true.
 *
 * @param end           zmienna informująca czy program się zakończył.
 * @param retransmit    zmienna którą zegar obsługuje.
 * @param options       opcje z którymi program został wywołany.
 */
void my_clock(bool &end, bool &retransmit, Options &options)
{
    while (!end)
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(options.RTIME));
        retransmit = true;
    }
}


int main(int argc, char *argv[])
{

    Options options = pars_options(argc, argv);


    uint64_t session_id = htobe64(time(nullptr));
    ssize_t iteration = 0;
    uint64_t first_byte_num;

    bool end = false, retransmit = false;


    size_t pack_size = (2 * sizeof(uint64_t) + options.PSIZE);

    Fifo fifo = Fifo(options.PSIZE, options.FSIZE / options.PSIZE, session_id);
    Re_sent re_sent = Re_sent(pack_size);


    // Tworzenie wątków pomocniczych
    std::thread clock_thread = std::thread([&]()
                                           { my_clock(end, retransmit, options); });
    std::thread copy_thread = std::thread([&]()
                                          { copy_from_fifo(end, fifo, re_sent); });
    std::thread control_thread = std::thread([&]()
                                             { control_port(end, re_sent, options); });


    struct sockaddr_in send_address = get_send_address(options.MCAST_ADDR, options.DATA_PORT);
    int socket_fd = open_socket_UDP();


    char *buffer;
    try
    {
        buffer = new char[pack_size];
    }
    catch (exception &e)
    {
        return 1;
    }


    memcpy(buffer, &session_id, sizeof(uint64_t));

    while (!end)
    {

        // Sprawdzam czy mineło RTIME, czy muszę retransmitować paczki.
        if (retransmit)
        {
            std::vector<char> &x = re_sent.swap();

            for (size_t i = 0; i < x.size(); i += pack_size)
            {
                send_message(socket_fd, &send_address, &x[i], 2 * sizeof(uint64_t) + options.PSIZE * sizeof(*buffer));
            }
            x.resize(0);
        }

        char *buff = buffer + 2 * sizeof(uint64_t);
        size_t left = options.PSIZE;
        while (left != 0)
        {
            ssize_t result = read(STDIN_FILENO, buff, left);
            if (result < 0)
            {
                free(buffer);
                return 1;
            }
            else if (result == 0)
            {
                end = true;
                left -= result;
                break;
            }
            else
            {
                left -= result;
                buff += result;
            }
        }

        if (left == 0)
        {

            first_byte_num = htobe64(options.PSIZE * iteration);
            memcpy(buffer + sizeof(uint64_t), &first_byte_num, sizeof(uint64_t));

            send_message(socket_fd, &send_address, buffer, 2 * sizeof(uint64_t) + options.PSIZE * sizeof(*buffer));
            fifo.save(buffer);
        }

        ++iteration;
    }
    delete[] buffer;

    clock_thread.join();
    control_thread.join();
    copy_thread.join();


    return 0;
}