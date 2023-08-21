#ifndef UTILITY_H
#define UTILITY_H


#include <arpa/inet.h>
#include <cstdint>
#include <netinet/in.h>
#include "err.h"
#include <netdb.h>
#include <fcntl.h>
#include <vector>
#include <exception>

#include <iostream>
#include <sstream>


struct My_exception : public std::exception {
    [[nodiscard]] const char * what () const noexcept override {
        return "";
    }
};


uint64_t get_session_id(const char *pack)
{
    return be64toh(*(uint64_t *) pack);
}

uint64_t get_byte_num(const char *pack)
{
    return be64toh(*(uint64_t *) (pack + sizeof(uint64_t)));
}

bool is_empty_pack(const char *pack, size_t size)
{
    size_t i = 0;
    while (i < size && pack[i] == 0)
    {
        ++i;
    }
    return i == size;
}


inline static void start_listening(int socket_fd, size_t queue_length)
{
    CHECK_ERRNO(listen(socket_fd, queue_length));
}


inline static int accept_connection(int socket_fd, struct sockaddr_in *client_address)
{
    socklen_t client_address_length = (socklen_t) sizeof(*client_address);

    int client_fd = accept(socket_fd, (struct sockaddr *) client_address, &client_address_length);
    if (client_fd < 0)
    {
        PRINT_ERRNO();
    }
    return client_fd;
}


inline static int open_socket_UDP()
{
    int socket_fd = socket(PF_INET, SOCK_DGRAM, 0);
    if (socket_fd < 0)
    {
        PRINT_ERRNO();
    }
    return socket_fd;
}

void bind_socket_to_any(uint16_t port, int socket_fd)
{
    struct sockaddr_in server_address;
    server_address.sin_family = AF_INET;
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);
    server_address.sin_port = htons(port);

    CHECK_ERRNO(bind(socket_fd, (struct sockaddr *) &server_address, (socklen_t) sizeof(server_address)));
}

int bind_socket_to_any_TCP(uint16_t port)
{
    int socket_fd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (socket_fd < 0)
    {
        PRINT_ERRNO();
    }
    bind_socket_to_any(port, socket_fd);
    return socket_fd;
}

void set_timeout(int socket_fd, uint64_t sec, uint64_t usec)
{
    struct timeval timeout;
    timeout.tv_sec = sec;
    timeout.tv_usec = usec;

    if (setsockopt(socket_fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout)) < 0)
    {
        perror("Error setting socket timeout");
        exit(EXIT_FAILURE);
    }
}

struct sockaddr_in get_send_address(char *host, uint16_t port)
{
    struct addrinfo hints;
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;

    struct addrinfo *address_result;
    CHECK(getaddrinfo(host, nullptr, &hints, &address_result));

    struct sockaddr_in send_address;
    send_address.sin_family = AF_INET;
    send_address.sin_addr.s_addr = ((struct sockaddr_in *) (address_result->ai_addr))->sin_addr.s_addr;
    send_address.sin_port = htons(port);

    freeaddrinfo(address_result);

    return send_address;
}


ssize_t read_message(int socket_fd, struct sockaddr_in *client_address, char *pack, size_t max_length)
{
    auto address_length = (socklen_t) sizeof(*client_address);
    int flags = 0;
    errno = 0;
    ssize_t len = recvfrom(socket_fd, pack, max_length, flags, (struct sockaddr *) client_address, &address_length);
    return len;
}

void send_message(int socket_fd, const struct sockaddr_in *send_address, char *pack, size_t length)
{
    int send_flags = 0;
    socklen_t address_length = (socklen_t) sizeof(*send_address);
    errno = 0;
    ssize_t sent_length = sendto(socket_fd, pack, length, send_flags, (struct sockaddr *) send_address, address_length);
    if (sent_length < 0)
    {
        PRINT_ERRNO();
    }
}

/**
 * Odczytuje ciąg liczb poprzedzilanych przecinkami, lub rzucam wyjątek keżeli jest nie poprawny.
 * @param string_input
 * @return
 */
std::vector<uint64_t> string_to_ints(const std::string &string_input)
{
    std::vector<uint64_t> numbers;
    size_t i = 0;
    while (i < string_input.size() && string_input[i] == ' ')
    {
        ++i;
    }

    uint64_t num = 0;
    while (i < string_input.size())
    {
        num = 0;
        while (string_input[i] >= '0' && string_input[i] <= '9')
        {
            num *= 10;
            num += string_input[i] - '0';
            ++i;
        }

        if (i < string_input.size() && string_input[i] != ',' && string_input[i] != '\n')
        {
            throw My_exception();
        }
        else
        {
            numbers.push_back(num);
        }

        ++i;
    }

    return numbers;
}


char **split_string(char *s)
{

    size_t len = strlen(s);
    char **parts = static_cast<char **>(calloc(5, sizeof(char *)));

    int end = 0;
    size_t i = 0;
    int p = 0;
    while (s[i] == ' ')
    {
        ++i;
        ++end;
    }

    for (; i < len && p < 3; ++i)
    {
        if (s[i] == ' ')
        {
            parts[p++] = strndup(s + end, i - end);
            end = i;

            while (s[i] == ' ')
            {
                ++i;
                ++end;
            }
        }
    }
    if (p == 3)
    {
        parts[p] = strndup(s + end, len - i + 1);

        if (parts[p][strlen(parts[p]) - 1] == '\n')
        {
            parts[p][strlen(parts[p]) - 1] = '\000';
        }
    }
    return parts;
}


void free_split_string(char **parts)
{
    for (int i = 0; parts[i] != nullptr; ++i)
    {
        free(parts[i]);
    }
    free(parts);
}

void send_all(int client_fd, char *message, size_t size)
{
    size_t i = 0;
    while (size != i)
    {
        auto tmp = send(client_fd, message, size - i, 0);
        if (tmp > 0)
        {
            i += tmp;
        }
        else
        {
            break;
        }
    }
}


#endif
