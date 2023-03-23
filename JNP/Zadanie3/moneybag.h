#ifndef MONEYBAG_H
#define MONEYBAG_H

#include <iostream>
#include <cstdint>
#include <sstream>
#include <compare>
#include <boost/multiprecision/cpp_int.hpp>

namespace {
#define LIVR_TO_DENAR 240
#define SOLIDUS_TO_DENAR 12
}

class Moneybag {
public:

    using coin_number_t = uint64_t;

    friend std::ostream &operator<<(std::ostream &os, const Moneybag &x);

    constexpr bool operator==(const Moneybag &x) const {
        return ((this->livr == x.livr) && (this->denier == x.denier) && (this->solidus == x.solidus));
    }

    constexpr Moneybag &operator+=(const Moneybag &m) {
        if (livr > UINT64_MAX - m.livr || denier > UINT64_MAX - m.denier || solidus > UINT64_MAX - m.solidus) {
            throw std::out_of_range("Nie da sie dodac");
        }

        livr += m.livr;
        solidus += m.solidus;
        denier += m.denier;

        return *this;
    }

    constexpr const Moneybag operator+(const Moneybag &m) const {
        return Moneybag(*this) += m;
    }


    constexpr Moneybag &operator-=(const Moneybag &x) {
        if (denier < x.denier || livr < x.livr || solidus < x.solidus) {
            throw std::out_of_range("Nie da sie odjac");
        }

        this->denier = (this->denier - x.denier);
        this->livr = (this->livr - x.livr);
        this->solidus = (this->solidus - x.solidus);
        return *this;
    }

    constexpr Moneybag operator-(const Moneybag &x) const {
        return Moneybag(*this) -= x;
    }

    constexpr Moneybag &operator*=(coin_number_t x) {
        coin_number_t temp1 = denier * x;
        coin_number_t temp2 = solidus * x;
        coin_number_t temp3 = livr * x;
        if (x != 0 && (temp1 / x != denier || temp2 / x != solidus || temp3 / x != livr)) {
            throw std::out_of_range("Nie da sie pomnozyc");
        }

        this->denier = (this->denier * x);
        this->livr = (this->livr * x);
        this->solidus = (this->solidus * x);
        return *this;
    }

    constexpr Moneybag operator*(coin_number_t x) const {
        return Moneybag(*this) *= x;
    }

    constexpr friend const Moneybag operator*(coin_number_t x, const Moneybag &m);

    constexpr std::partial_ordering operator<=>(const Moneybag &x) const {
        if (livr == x.livr && solidus == x.solidus && denier == x.denier) { return std::partial_ordering::equivalent; }
        if (livr >= x.livr && solidus >= x.solidus && denier >= x.denier) { return std::partial_ordering::greater; }
        if (livr <= x.livr && solidus <= x.solidus && denier <= x.denier) { return std::partial_ordering::less; }
        return std::partial_ordering::unordered;
    }

    constexpr explicit operator bool() const {
        return !(livr == 0 && solidus == 0 && denier == 0);
    }

    constexpr Moneybag(coin_number_t a, coin_number_t b, coin_number_t c) : livr(a), solidus(b), denier(c) {};

    constexpr coin_number_t livre_number() {
        return livr;
    }

    constexpr coin_number_t solidus_number() {
        return solidus;
    }

    constexpr coin_number_t denier_number() {
        return denier;
    }

private:
    coin_number_t livr;
    coin_number_t solidus;
    coin_number_t denier;
};

constexpr const Moneybag operator*(const uint64_t x, const Moneybag &m) {
    return m * x;
}

inline std::ostream &operator<<(std::ostream &os, const Moneybag &x) {
    return os << "(" << x.livr << " livr" << (x.livr == 1 ? "" : "es") <<
              ", " << x.solidus << " solidus" << (x.solidus == 1 ? "" : "es") <<
              ", " << x.denier << " denier" << (x.denier == 1 ? "" : "s") << ")";
}

constinit const Moneybag Livre(1, 0, 0);

constinit const Moneybag Solidus(0, 1, 0);

constinit const Moneybag Denier(0, 0, 1);

class Value {
private:
    boost::multiprecision::uint128_t denier;
public:
    constexpr Value(Moneybag x) {
        boost::multiprecision::uint128_t wynik = x.denier_number();
        boost::multiprecision::uint128_t temp = x.livre_number();
        temp *= LIVR_TO_DENAR;
        wynik += temp;
        temp = x.solidus_number();
        temp *= SOLIDUS_TO_DENAR;
        wynik += temp;
        denier = wynik;
    }

    constexpr Value(uint64_t x) : denier(x) {};

    constexpr Value() : denier(0) {};

    inline explicit operator std::string() const {
        std::stringstream ss;
        ss << denier;
        return ss.str();
    }

    inline bool operator==(const Value &x) const {
        return (denier == x.denier);
    }

    inline std::weak_ordering operator<=>(const Value &v) const {
        if (denier > v.denier) { return std::weak_ordering::greater; }
        if (denier == v.denier) { return std::weak_ordering::equivalent; }
        return std::weak_ordering::less;
    }

};

#endif //MONEYBAG_H