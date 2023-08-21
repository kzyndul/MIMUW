#ifndef BUFFOR_H
#define BUFFOR_H

#include <cstdint>
#include <mutex>
#include "utility.h"
#include <cstring>
#include <condition_variable>
#include <set>

/**
 * Bufora zachowuję się na zasadzie producentów i konsumerów.
 */
class Buffer {
    std::size_t buffer_size;
    std::size_t message_size;
    std::size_t byte0;
    std::size_t end_num;
    std::size_t last_played;

    mutable std::mutex mutex_p;
    mutable std::condition_variable wait_for_pack;
    mutable std::condition_variable wait_for_buffer;

    mutable std::mutex mutex_m;
    std::vector<uint64_t> packs_lost;

    bool czekam;
    bool force_play;
    bool play;
    char *buffer = nullptr;


    void clear(size_t i)
    {
        std::memset(buffer + i, 0, message_size);
    }

    void clear_buffer(size_t to_play)
    {
        size_t tmp = 0;
        while (tmp != to_play)
        {
            clear((last_played + tmp) % buffer_size);
            tmp += message_size;
        }
    }

    /**
     * Zapisuje numery paczek, które brakują.
     */
    void missing(size_t byte_num)
    {
        if (byte_num > last_played)
        {
            mutex_m.lock();
            size_t help = last_played;
            while (help != byte_num)
            {
                packs_lost = std::vector<uint64_t>();
                if (is_empty_pack(buffer + (help % buffer_size), message_size))
                {
                    packs_lost.push_back(help);
                }
                help += message_size;
            }
            mutex_m.unlock();
        }
    }




public:
    Buffer(std::size_t byte0, std::size_t buffer_size, std::size_t message_size) : buffer_size(
            (buffer_size / message_size) * message_size), message_size(message_size), byte0(byte0), end_num(byte0),
            last_played(byte0), mutex_p(), wait_for_pack(), wait_for_buffer(), czekam(false), force_play(false),
            play(false)

    {
        buffer = new char[this->buffer_size];
    }

    Buffer()
    {}

    Buffer &operator=(const Buffer &other)
    {
        message_size = other.message_size;
        buffer_size = other.buffer_size;
        byte0 = other.byte0;
        last_played = other.last_played;
        end_num = other.end_num;

        force_play = false;
        play = false;
        czekam = false;

        buffer = new char[this->buffer_size];
        return *this;
    }

    Buffer(Buffer const &other) : mutex_p(), wait_for_pack(), wait_for_buffer()
    {
        message_size = other.message_size;
        buffer_size = other.buffer_size;
        byte0 = other.byte0;
        play = false;
        czekam = false;
        force_play = false;
        end_num = other.end_num;
        last_played = other.last_played;

        buffer = new char[this->buffer_size];
    }

    /*
     * Pobieram paczkę z bufora. Sprzwdzam czy bufor jest nie pusty jeżeli jest nie pusty do kopiuję paczkę z buffora,
     * wpp zaczynam grę od nowa.
     */
    void print_buffer(uint8_t *buf, const bool &end_playing)
    {
        std::unique_lock<std::mutex> lock(mutex_p);
        while ((!play || last_played == end_num || is_empty_pack(buffer + (last_played % buffer_size), message_size)) &&
               !force_play && end_playing)
        {
            if (is_empty_pack(buffer + (last_played % buffer_size), message_size) && end_playing)
            {
                play = false;
                byte0 = last_played;
            }
            czekam = true;
            wait_for_pack.wait(lock);
        }
        if (end_playing)
        {
            czekam = false;

            memcpy(buf, buffer + (last_played % buffer_size), message_size);
            memset(buffer + (last_played % buffer_size), 0, message_size);
            last_played += message_size;
            if (force_play)
            {
                force_play = false;
                wait_for_buffer.notify_one();
            }
            else
            {
                lock.unlock();
            }
        }
    }

    size_t get_messege_size() const
    {
        return message_size;
    }

    /*
     * Zapisuje paczkę do bufora.
     */
    void add_buffer(char *pack)
    {
        size_t byte_num = get_byte_num(pack);
        if (byte_num >= last_played)
        {
            play = play || byte_num >= byte0 + 3 * buffer_size / 4;
            size_t to_play = byte_num - last_played;
            std::unique_lock<std::mutex> lock(mutex_p);

            if (byte_num == end_num && to_play == buffer_size)
            {
                force_play = true;
                wait_for_pack.notify_one();
                wait_for_buffer.wait(lock);
            }
            else if (to_play >= buffer_size)
            {
                size_t tmp = std::min(buffer_size, to_play - buffer_size);
                clear_buffer(tmp);
                last_played = byte_num + message_size - buffer_size;
            }
            end_num = std::max(end_num, byte_num + message_size);
            memcpy(buffer + (byte_num % buffer_size), pack + 2 * sizeof(uint64_t), message_size);

            missing(byte_num);
            if (czekam)
            {
                wait_for_pack.notify_one();
            }
            else
            {
                lock.unlock();
            }

        }
    }

    std::vector<uint64_t> get_missing()
    {
        const std::lock_guard<std::mutex> lock(mutex_m);
        return std::vector<uint64_t>({packs_lost});
    }


    void wake_me()
    {
        wait_for_pack.notify_one();
    }

    ~Buffer()
    {
        delete[] buffer;
    }
};

#endif
