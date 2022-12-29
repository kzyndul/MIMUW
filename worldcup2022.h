#ifndef ZADANIE6_WORLDCUP2022_H
#define ZADANIE6_WORLDCUP2022_H

#include "worldcup.h"
#include <list>
#include <utility>
#include <vector>
#include <list>
#include <unordered_map>

class Gracz {
    std::string imie;
    int pieniadze;
    int ile_czeka;
public:
    explicit Gracz(std::string name) : imie(std::move(name)), pieniadze(1000), ile_czeka(0)
    {}

    void rozlicz(int ile)
    {
        pieniadze += ile;
    }

    void czekaj(int ile)
    {
        ile_czeka = ile;
    }

    bool czeka ()
    {
        return (ile_czeka > 0);
    }

    void pomin_kolejke ()
    {
        if (ile_czeka > 0)
        {
            --ile_czeka;
        }
    }

    bool bankrut ()
    {
        return (pieniadze < 0);
    }


    std::string get_imie ()
    {
        return imie;
    }
};

class Pole {
protected:
    const std::string nazwa;
public:
    virtual void stan_na_polu(Gracz &zawodnik) = 0;

    virtual void przejdz_przez_pole(Gracz &zawodnik)
    {}

    explicit Pole (std::string nazwa) : nazwa(std::move(nazwa)) {}

    virtual ~Pole() = default;
};

class Pole_czekajace : public Pole {
    int ile;
public:
    void stan_na_polu(Gracz &zawodnik) override
    {
        zawodnik.czekaj(ile);
    }
    Pole_czekajace (std::string nazwa, int ile) : Pole(std::move(nazwa)), ile(ile) {}

    ~Pole_czekajace() override = default;

};

class Wolne : public Pole {
public:
    explicit Wolne(std::string nazwa) : Pole(std::move(nazwa)) {}

    void stan_na_polu(Gracz &zawodnik) override {}

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
    void stan_na_polu(Gracz &zawodnik) override
    {
        zawodnik.rozlicz(ile);
    }
    Przy_wejsciu(std::string nazwa, int ile) : Pole_pienizne(std::move(nazwa), ile) {}

    ~Przy_wejsciu() override = default;
};

class Mecz : public Pole_pienizne {
    int pula;
    float jaki_mecz;
public:
    void przejdz_przez_pole(Gracz &zawodnik) override
    {
        zawodnik.rozlicz(ile);
        pula += ile;
    }

    void stan_na_polu(Gracz &zawodnik) override
    {
        zawodnik.rozlicz(pula * jaki_mecz);
        pula = 0;
    }

    Mecz(std::string nazwa, int ile, float jaki_mecz) : Pole_pienizne(std::move(nazwa), ile), pula(0), jaki_mecz(jaki_mecz) {}


};

class Start : public Przy_wejsciu {
public:
    void przejdz_przez_pole(Gracz &zawodnik) override
    {
        zawodnik.rozlicz(ile);
    }
    Start(std::string nazwa, int ile) : Przy_wejsciu(std::move(nazwa), ile) {}

    ~Start() override = default;
};




class Okresowe : public Pole_pienizne {
protected:
    int co_ile;
    int ktory;
public:
    void stan_na_polu(Gracz &zawodnik) override
    {
        if (ktory == 0)
        {
            zawodnik.rozlicz(ile);
        }
        else
        {
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
    std::unordered_map<std::string, int> pozycja;



    int rzuc_koscmi (std::list<std::shared_ptr<Die>> &kostki)
    {
        int sum = 0;
        for (auto it = kostki.begin(); it != kostki.end(); ++it)
        {
            sum += it->get()->roll();
        }
        return sum;
    }

public:
    Plansza(Plansza &&other)  noexcept : pola(std::move(other.pola)), rozmiar_planszy(other.rozmiar_planszy) {}
    Plansza() : rozmiar_planszy(0) {}
    Plansza &operator=(Plansza other) noexcept {
            pola = other.pola;
            rozmiar_planszy = other.rozmiar_planszy;
    }


    void dodaj_pole (std::shared_ptr<Pole> pole)
    {
        pola.push_back(pole);
        ++rozmiar_planszy;
    }

    void inicjuj (std::list<Gracz> &gracze)
    {
        for (Gracz g : gracze)
        {
            pozycja.insert({g.get_imie(), 0});
        }
    }

    bool wykonaj_ruch (Gracz &gracz, std::list<std::shared_ptr<Die>> &kostki)
    {
        gracz.pomin_kolejke();
        if (gracz.czeka())
        {
            return false;
        }

        int obecna_pozycja = pozycja[gracz.get_imie()];
        auto ile_oczek = rzuc_koscmi(kostki);
        for (int i = 0; i < ile_oczek - 1; ++i)
        {
            obecna_pozycja = (obecna_pozycja + 1) % rozmiar_planszy;
            pola[obecna_pozycja]->przejdz_przez_pole(gracz);
            if (gracz.bankrut())
            {
                pozycja.erase(gracz.get_imie());
                return true;
            }
        }
        obecna_pozycja = (obecna_pozycja + 1) % rozmiar_planszy;
        pola[obecna_pozycja]->stan_na_polu(gracz);
        if (gracz.bankrut())
        {
            pozycja.erase(gracz.get_imie());
            return true;
        }
        pozycja[gracz.get_imie()] = obecna_pozycja;
        return false;
    }
};

class WorldCup2022 : public WorldCup {
    Plansza plansza;
    std::list<std::shared_ptr<Die>> kostki;
    std::list<Gracz> gracze;
    std::shared_ptr<ScoreBoard> tablica_wynikow;


    bool jedna_runda ()
    {
        auto it = gracze.begin();
        while (it != gracze.end() && gracze.size() > 1)
        {
            auto czy_bankrut = plansza.wykonaj_ruch(*it, kostki);
            if (!czy_bankrut)
            {
                ++it;
            }
            else
            {
                it = gracze.erase(it);
            }
        }
        return (gracze.size() == 1);
    }



public:
    void addDie(std::shared_ptr<Die> die) override
    {
        kostki.push_back(die);
    }

    void addPlayer(std::string const &name) override
    {
        gracze.emplace_back(name);
    }

    void setScoreBoard(std::shared_ptr<ScoreBoard> scoreboard) override
    {
        tablica_wynikow = scoreboard;
    }

    void play(unsigned int rounds) override
    {
        plansza.inicjuj(gracze);
        for (int i = 0; i < rounds; ++i)
        {
            if (jedna_runda())
            {
                break;
            }
        }
        // znajdz zwyciesce
    }

    WorldCup2022()
    {
        Plansza temp = Plansza();
        temp.dodaj_pole(std::make_shared<Start>("początek sezonu", 50));
        temp.dodaj_pole(std::make_shared<Mecz>("mecz z San Marino", -160, -1));
        temp.dodaj_pole(std::make_shared<Wolne>("dzień wolny od treningu"));
        temp.dodaj_pole(std::make_shared<Mecz>("mecz z Liechtensteinem", -220, -1));
        temp.dodaj_pole(std::make_shared<Pole_czekajace>("żółta kartka", 3));
        temp.dodaj_pole(std::make_shared<Mecz>("mecz z Meksykiem", -300, -2.5));
        temp.dodaj_pole(std::make_shared<Mecz>("mecz z Arabią Saudyjską", -280, -2.5));
        temp.dodaj_pole(std::make_shared<Okresowe>("bukmacher", 100, 3));
        temp.dodaj_pole(std::make_shared<Mecz>("mecz z Argentyną", -250, -2.5));
        temp.dodaj_pole(std::make_shared<Przy_wejsciu>("gol", 120));
        temp.dodaj_pole(std::make_shared<Mecz>("mecz z Francją", -400, -4));
        temp.dodaj_pole(std::make_shared<Przy_wejsciu>("rzut karny", -180));
        plansza = std::move(temp);
    }
};


#endif //ZADANIE6_WORLDCUP2022_H
