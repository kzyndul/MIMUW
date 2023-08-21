#ifndef DISCOVERD_STATIONS_H
#define DISCOVERD_STATIONS_H

#include <set>
#include "Radio_station.h"
#include <iostream>
#include <atomic>
#include <utility>

class Discovered_stations {
    std::set<Radio_station> stations;                                       // Lista stacji.
    std::string name;
    std::set<Radio_station>::iterator current_station = stations.end();     // Iterator na obecnie słuchaną stację.


    std::mutex mutex;
    std::condition_variable wait_for_first;


    std::atomic<bool> change_station_recive{true};      // Informuję czy musze zmienić słuchaną stację.
    std::atomic<bool> change_station_control{false};    // Informuję czy musze odświeżyć UI.


public:

    /**
     * Jeżeli stacja o tych samych parametrach istnieje to aktualizuje informacje kiedy dostalem od nije ostatni raz
     * wiadomość, wpp dodaję stacje do listy.
     */
    void add_station(char *mcast_addr, uint16_t data_port, std::string n)
    {
        mutex.lock();
        auto new_statio = Radio_station(mcast_addr, data_port, n);
        if (stations.find(new_statio) == stations.end())
        {
            auto where = stations.insert(new_statio);
            if (where.second)
            {
                change_station_control = true;
                if (n == name)
                {
                    current_station = where.first;
                    change_station_recive = true;
                }
                if (current_station == stations.end() && name.empty())
                {
                    current_station = stations.begin();
                    change_station_recive = true;
                }
                wait_for_first.notify_one();
            }
        }
        else
        {
            auto it = stations.find(new_statio);
            it->last_message = 0;
        }
        mutex.unlock();
    }


    /**
     * Ubsługa przełączania stacji przez UI.
     */
    void switch_station(int i)
    {
        mutex.lock();

        if (current_station == stations.end())
        {
            current_station = stations.begin();
            change_station_recive = true;
            change_station_control = true;
            wait_for_first.notify_one();
            mutex.unlock();
            return;
        }

        if (stations.size() == 1 && current_station == stations.end())
        {
            current_station = stations.begin();
            change_station_recive = true;
            change_station_control = true;
            wait_for_first.notify_one();
            mutex.unlock();
            return;
        }


        if (i > 0)
        {

            if (current_station != stations.begin())
            {
                current_station = std::prev(current_station);
                change_station_recive = true;
                change_station_control = true;
                wait_for_first.notify_one();
            }
        }
        else
        {
            if (current_station != std::prev(stations.end()))
            {
                current_station = std::next(current_station);
                change_station_recive = true;
                change_station_control = true;
                wait_for_first.notify_one();

            }
        }
        mutex.unlock();
    }

    /**
     * Usuwam stację, które nie odzywały się dłużej niż 20s.
     */
    void update_station()
    {
        mutex.lock();
        bool erased = false;
        auto now = current_station;
        for (auto it = stations.begin(); it != stations.end(); ++it)
        {
            it->last_message++;
            if (it->last_message == 4)
            {
                if (it == now)
                {
                    change_station_recive = true;
                    erased = true;
                }
                it = stations.erase(it);
                change_station_control = true;

                if (it == stations.end())
                {
                    break;
                }
            }

        }

        if (erased)
        {
            current_station = stations.end();
            if (name.empty())
            {
                current_station = stations.begin();
                change_station_recive = true;
                change_station_control = true;
            }
            for (auto it = stations.begin(); it != stations.end(); ++it)
            {
                if (it->name == name)
                {
                    change_station_recive = true;
                    change_station_control = true;
                    current_station = it;
                    break;
                }
            }
        }
        mutex.unlock();
    }

    char *to_char()
    {
        std::string result;
        std::string _break = "--------------------------------------------------------------------------------\n";
        std::string  new_line = "\n";
        std::string  header = "SIK RADIO\n";

        result += _break + new_line + header + new_line + _break + new_line;
        mutex.lock();
        for (auto x: stations)
        {
            if (current_station != stations.end() && *current_station == x)
            {
                result += " > ";
            }

            result += x.to_string();
            result += new_line;
        }
        mutex.unlock();
        result += _break;
        char* tmp = new char[result.size() + 1];
        std::strcpy(tmp, result.c_str());
        return tmp;
    }

    bool has_change_control()
    {
        if (change_station_control)
        {
            change_station_control = false;
            return true;
        }
        return false;
    }

    bool has_change_reciver()
    {
        if (change_station_recive)
        {
            change_station_recive = false;
            return true;
        }
        return false;
    }

    Radio_station get_currently_playing()
    {
        return *current_station;
    }

    void wait_for_station()
    {
        std::unique_lock<std::mutex> lk(mutex);
        wait_for_first.wait(lk, [&]{return current_station != stations.end();});
    }


    explicit Discovered_stations(std::string name) : name(std::move(name)) {}
};


#endif
