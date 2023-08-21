#ifndef RADIO_STATION_H
#define RADIO_STATION_H

#include <cstring>
#include <iostream>
#include <utility>


class Radio_station {
public:

    std::string name;
    std::string mcast_addr;
    uint16_t data_port;

    mutable uint32_t last_message = 0;


    bool operator==(const Radio_station &other) const
    {
        return name == other.name && mcast_addr == other.mcast_addr && data_port == other.data_port;
    }

    bool operator<(const Radio_station &other) const
    {
        if (name < other.name)
        {
            return true;
        }
        else if (name == other.name)
        {
            if (mcast_addr < other.mcast_addr)
            {
                return true;
            }
            else if (mcast_addr == other.mcast_addr)
            {
                return data_port < other.data_port;
            }
        }
        return false;
    }

    std::string to_string()
    {
        return "RADIO \"" + name + "\"\n";
    }


    Radio_station(char *mcast_addr, uint16_t data_port, std::string name) :
            name(std::move(name)),
            mcast_addr(mcast_addr),
            data_port(data_port)

    {};
};


#endif
