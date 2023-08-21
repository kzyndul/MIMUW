#ifndef ERR_
#define ERR_

#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cerrno>
#include <unistd.h>
#include <cstring>
#include <cctype>
#include <iostream>

// Evaluate `x`: if non-zero, describe it as a standard error code and exit with an error.
#define CHECK(x)                                                          \
    do {                                                                  \
        int err = (x);                                                    \
        if (err != 0) {                                                   \
            fprintf(stderr, "Error: %s returned %d in %s at %s:%d\n%s\n", \
                #x, err, __func__, __FILE__, __LINE__, strerror(err));    \
            exit(EXIT_FAILURE);                                           \
        }                                                                 \
    } while (0)

// Evaluate `x`: if false, print an error message and exit with an error.
#define ENSURE(x)                                                         \
    do {                                                                  \
        bool result = (x);                                                \
        if (!result) {                                                    \
            fprintf(stderr, "Error: %s was false in %s at %s:%d\n",       \
                #x, __func__, __FILE__, __LINE__);                        \
            exit(EXIT_FAILURE);                                           \
        }                                                                 \
    } while (0)

// Check if errno is non-zero, and if so, print an error message and exit with an error.
#define PRINT_ERRNO()                                                  \
    do {                                                               \
        if (errno != 0) {                                              \
            fprintf(stderr, "Error: errno %d in %s at %s:%d\n%s\n",    \
              errno, __func__, __FILE__, __LINE__, strerror(errno));   \
            exit(EXIT_FAILURE);                                        \
        }                                                              \
    } while (0)


// Set `errno` to 0 and evaluate `x`. If `errno` changed, describe it and exit.
#define CHECK_ERRNO(x)                                                             \
    do {                                                                           \
        errno = 0;                                                                 \
        (void) (x);                                                                \
        PRINT_ERRNO();                                                             \
    } while (0)

// Note: the while loop above wraps the statements so that the macro can be used with a semicolon
// for example: if (a) CHECK(x); else CHECK(y);


// Print an error message and exit with an error.
void fatal(const char *fmt, ...)
{
    va_list fmt_args;

    fprintf(stderr, "Error: ");
    va_start(fmt_args, fmt);
    vfprintf(stderr, fmt, fmt_args);
    va_end(fmt_args);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

bool is_number(char *str)
{
    for (size_t i = 0; i < strlen(str); ++i)
    {
        if (!isdigit(str[i]))
        {
            return false;
        }
    }
    return true;
}

bool is_valid_name(std::string &str)
{
    if (str.empty() || str[0] == ' ')
    {
        return false;
    }

    for (auto x: str)
    {
        if (x < 32 || x > 127)
        {
            return false;
        }
    }
    return str.length() <= 64;
}

bool is_multicast_address(char *address)
{
    struct sockaddr_in sa;
    int result = inet_pton(AF_INET, address, &(sa.sin_addr));
    return (result == 1 && (sa.sin_addr.s_addr & htonl(0xF0000000)) == htonl(0xE0000000));
}

void is_port_number(char *str)
{
    auto num = strtoul(optarg, nullptr, 10);
    if (!is_number(optarg) || num > UINT16_MAX)
    {
        fatal("%u is not a valid port number", num);
    }
}


bool pars_replay(char **s)
{
    const char *borewicz_here = "BOREWICZ_HERE";
    int i = 0;
    for (; s[i] != nullptr; ++i)
    {}
    if (i != 4)
    {
        return false;
    }

    if (strncmp(borewicz_here, s[0], strlen(s[0])) != 0)
    {
        return false;
    }

    if (!is_multicast_address(s[1]))
    {
        return false;
    }

    if (!is_number(s[2]) || strtoul(s[2], nullptr, 10) > UINT16_MAX)
    {
        return false;
    }

    std::string tmp = s[3];
    if (!is_valid_name(tmp))
    {
        return false;
    }


    return true;
}






#endif
