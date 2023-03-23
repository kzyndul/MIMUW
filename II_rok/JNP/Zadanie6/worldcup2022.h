#ifndef ZADANIE6_WORLDCUP2022_H
#define ZADANIE6_WORLDCUP2022_H

#include "worldcup.h"
#include <list>
#include <utility>
#include <vector>
#include <list>
#include <unordered_map>
#include<sstream>

class WorldCupException : public std::exception {
public:
    const char *what() const throw() {
        return nullptr;
    }
};

class TooManyDiceException : public WorldCupException {};
class TooFewDiceException : public WorldCupException {};
class TooManyPlayersException : public WorldCupException {};
class TooFewPlayersException : public WorldCupException {};


class Gracz {
    std::string imie;
    int pieniadze;
    int ile_czeka;
    int id;
public:
    explicit Gracz(std::string name) : imie(std::move(name)), pieniadze(1000), ile_czeka(0), id(0) {}

    void set_id(int new_id) {
        id = new_id;
    }

    int get_id() {
        return id;
    }

    void rozlicz(int ile) {
        pieniadze += ile;
    }

    void czekaj(int ile) {
        ile_czeka = ile;
    }

    bool czeka() {
        ile_czeka = std::max(ile_czeka - 1, 0);
        return ile_czeka;
    }

    std::string status() {
        if (bankrut()) {
            return "*** bankrut ***";
        } else if (ile_czeka) {
            std::stringstream ss;
            ss << ile_czeka;
            return "*** czekanie: " + ss.str() + " ***";
        } else {
            return "w grze";
        }
    }

    int wynik() const {
        return pieniadze;
    }

    bool bankrut() const {
        return (pieniadze < 0);
    }


    std::string get_imie() {
        return imie;
    }
};

class Pole {
protected:
    const std::string nazwa;
public:
    virtual void stan_na_polu(Gracz &zawodnik) {
        (void) zawodnik;
    }

    virtual void przejdz_przez_pole(Gracz &zawodnik) {
        (void) zawodnik;
    }

    std::string podaj_nazwe() {
        return nazwa;
    }

    explicit Pole(std::string nazwa) : nazwa(std::move(nazwa)) {}

    virtual ~Pole() = default;
};

class Pole_czekajace : public Pole {
    int ile;
public:
    void stan_na_polu(Gracz &zawodnik) override {
        zawodnik.czekaj(ile);
    }

    Pole_czekajace(std::string nazwa, int ile) : Pole(std::move(nazwa)), ile(ile) {}

    ~Pole_czekajace() override = default;

};

class Wolne : public Pole {
public:
    explicit Wolne(std::string nazwa) : Pole(std::move(nazwa)) {}

    ~Wolne() override = default;

};

class Pole_pienizne : public Pole {
protected:
    int ile;
public:
    Pole_pienizne(std::string nazwa, int ile) : Pole(std::move(nazwa)), ile(ile) {}

    ~Pole_pienizne() override = default;

};

class Przy_wejsciu : public Pole_pienizne {
public:
    void stan_na_polu(Gracz &zawodnik) override {
        zawodnik.rozlicz(ile);
    }

    Przy_wejsciu(std::string nazwa, int ile) : Pole_pienizne(std::move(nazwa), ile) {}

    ~Przy_wejsciu() override = default;
};

class Start : public Przy_wejsciu {
public:
    void stan_na_polu(Gracz &zawodnik) override {
        zawodnik.rozlicz(ile);
    }

    void przejdz_przez_pole(Gracz &zawodnik) override {
        zawodnik.rozlicz(ile);
    }

    Start(std::string nazwa, int ile) : Przy_wejsciu(std::move(nazwa), ile) {}


    ~Start() override = default;
};


class Mecz : public Pole_pienizne {
    int pula;
    float jaki_mecz;
public:
    void przejdz_przez_pole(Gracz &zawodnik) override {
        zawodnik.rozlicz(ile);
        pula += ile;
    }

    void stan_na_polu(Gracz &zawodnik) override {
        zawodnik.rozlicz(pula * jaki_mecz);
        pula = 0;
    }

    Mecz(std::string nazwa, int ile, float jaki_mecz) : Pole_pienizne(std::move(nazwa), ile), pula(0),
                                                        jaki_mecz(jaki_mecz) {}


};

class Okresowe : public Pole_pienizne {
protected:
    int co_ile;
    int ktory;
public:
    void stan_na_polu(Gracz &zawodnik) override {
        if (ktory == 0) {
            zawodnik.rozlicz(ile);
        } else {
            zawodnik.rozlicz(-ile);
        }
        ktory = (ktory + 1) % co_ile;
    }

    Okresowe(std::string nazwa, int ile, int co_ile) : Pole_pienizne(std::move(nazwa), ile), co_ile(co_ile), ktory(0) {}

    ~Okresowe() override = default;
};

class Plansza {
    std::vector<std::shared_ptr<Pole>> pola;
    int rozmiar_planszy;
    std::unordered_map<int, int> pozycja;

    int rzuc_koscmi(std::list<std::shared_ptr<Die>> &kostki) {
        int sum = 0;
        for (auto it = kostki.begin(); it != kostki.end(); ++it) {
            sum += it->get()->roll();
        }
        return sum;
    }

public:
    Plansza(Plansza &&other) noexcept: pola(std::move(other.pola)), rozmiar_planszy(other.rozmiar_planszy) {}

    Plansza() : rozmiar_planszy(0) {}

    Plansza &operator=(Plansza other) noexcept {
        pola = other.pola;
        rozmiar_planszy = other.rozmiar_planszy;
        return *this;
    }

    std::shared_ptr<Pole> podaj_pole(Gracz &g) {
        return pola[pozycja[g.get_id()]];
    }

    void dodaj_pole(std::shared_ptr<Pole> pole) {
        pola.push_back(pole);
        ++rozmiar_planszy;
    }


    void inicjuj(std::list<Gracz> &gracze) {
        for (Gracz g: gracze) {
            pozycja.insert({g.get_id(), 0});
        }
    }

    void usun_gracza(int id) {
        pozycja.erase(id);
    }

    bool wykonaj_ruch(Gracz &gracz, std::list<std::shared_ptr<Die>> &kostki) {
        if (!gracz.czeka()) {
            int obecna_pozycja = pozycja[gracz.get_id()];
            auto ile_oczek = rzuc_koscmi(kostki);

            for (int i = 0; i < ile_oczek - 1; ++i) {
                obecna_pozycja = (obecna_pozycja + 1) % rozmiar_planszy;
                pola[obecna_pozycja]->przejdz_przez_pole(gracz);
                if (gracz.bankrut()) {
                    obecna_pozycja =
                            (obecna_pozycja + ile_oczek - 1 - i) % rozmiar_planszy; // przesuwamy na daną pozycję
                    pozycja[gracz.get_id()] = obecna_pozycja;
                    return true;
                }
            }

            obecna_pozycja = (obecna_pozycja + 1) % rozmiar_planszy;
            pola[obecna_pozycja]->stan_na_polu(gracz);
            if (gracz.bankrut()) {
                pozycja[gracz.get_id()] = obecna_pozycja;
                return true;
            }
            pozycja[gracz.get_id()] = obecna_pozycja;
        }
        return false;
    }
};

class WorldCup2022 : public WorldCup {
    Plansza plansza;
    std::list<std::shared_ptr<Die>> kostki;
    std::list<Gracz> gracze;
    std::shared_ptr<ScoreBoard> tablica_wynikow;


    bool jedna_runda(int numer_rundy) {
        tablica_wynikow->onRound(numer_rundy);

        auto it = gracze.begin();
        while (it != gracze.end() && gracze.size() > 1) {
            auto czy_bankrut = plansza.wykonaj_ruch(*it, kostki);
            tablica_wynikow->onTurn(it->get_imie(), it->status(), plansza.podaj_pole(*it)->podaj_nazwe(),
                                    std::max(it->wynik(), 0));
            if (czy_bankrut) {
                plansza.usun_gracza(it->get_id());
                it = gracze.erase(it);
            } else {
                ++it;
            }
        }
        return (gracze.size() == 1);
    }


public:
    void addDie(std::shared_ptr<Die> die) override {
        kostki.push_back(die);
    }

    void addPlayer(std::string const &name) override {
        gracze.emplace_back(name);
        gracze.back().set_id(gracze.size());
    }

    void setScoreBoard(std::shared_ptr<ScoreBoard> scoreboard) override {
        tablica_wynikow = scoreboard;
    }

    void play(unsigned int rounds) override {
        if (gracze.size() < 2)
            throw TooFewPlayersException();
        if (gracze.size() > 11)
            throw TooManyPlayersException();
        if (kostki.size() < 2)
            throw TooFewDiceException();
        if (kostki.size() > 2)
            throw TooManyDiceException();


        plansza.inicjuj(gracze);
        bool stop = false;
        for (unsigned int i = 0; i < rounds && !stop; ++i) {
            stop = jedna_runda(i);
        }

        Gracz top_gracz = gracze.front();
        for (auto iter = gracze.begin(); iter != gracze.end(); ++iter) {
            if (iter->wynik() > top_gracz.wynik())
                top_gracz = *iter;
        }

        std::vector<Gracz> zwyciezcy;
        for (auto iter = gracze.begin(); iter != gracze.end(); ++iter) {
            if (iter->wynik() == top_gracz.wynik())
                zwyciezcy.push_back(*iter);
        }


        for (Gracz gracz : zwyciezcy) {
            tablica_wynikow->onWin(gracz.get_imie());
        }
    }

    WorldCup2022() {
        Plansza temp = Plansza();
        temp.dodaj_pole(std::make_shared<Start>("Początek sezonu", 50));
        temp.dodaj_pole(std::make_shared<Mecz>("Mecz z San Marino", -160, -1));
        temp.dodaj_pole(std::make_shared<Wolne>("Dzień wolny od treningu"));
        temp.dodaj_pole(std::make_shared<Mecz>("Mecz z Liechtensteinem", -220, -1));
        temp.dodaj_pole(std::make_shared<Pole_czekajace>("Żółta kartka", 3));
        temp.dodaj_pole(std::make_shared<Mecz>("Mecz z Meksykiem", -300, -2.5));
        temp.dodaj_pole(std::make_shared<Mecz>("Mecz z Arabią Saudyjską", -280, -2.5));
        temp.dodaj_pole(std::make_shared<Okresowe>("Bukmacher", 100, 3));
        temp.dodaj_pole(std::make_shared<Mecz>("Mecz z Argentyną", -250, -2.5));
        temp.dodaj_pole(std::make_shared<Przy_wejsciu>("Gol", 120));
        temp.dodaj_pole(std::make_shared<Mecz>("Mecz z Francją", -400, -4));
        temp.dodaj_pole(std::make_shared<Przy_wejsciu>("Rzut karny", -180));
        plansza = std::move(temp);
    }
};


#endif //ZADANIE6_WORLDCUP2022_H
