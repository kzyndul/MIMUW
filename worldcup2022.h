#ifndef ZADANIE6_WORLDCUP2022_H
#define ZADANIE6_WORLDCUP2022_H

#include "worldcup.h"
#include <list>
#include <utility>
#include <vector>
#include <list>

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
};

class Pole {
protected:
    const std::string nazwa;
public:
    virtual void stan_na_polu(Gracz &zawodnik) = 0;

    virtual void przejdz_przez_pole(Gracz &zawodnik) const
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

class Start : public Przy_wejsciu {
public:
    void przejdz_przez_pole(Gracz &zawodnik) const override
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
    std::vector<Pole *> pola;
    int rozmiar_planszy;

public:
    Plansza(Plansza &&other)  noexcept : pola(std::move(other.pola)), rozmiar_planszy(other.rozmiar_planszy) {}

    Plansza &operator=(Plansza other) noexcept {
            pola = other.pola;
            rozmiar_planszy = other.rozmiar_planszy;
    }

        Plansza() : rozmiar_planszy(0) {}
    void dodaj_pole (Pole *pole)
    {
        pola.push_back(pole);
        ++rozmiar_planszy;
    }
};

class WorldCup2022 : public WorldCup {
    Plansza plansza;
    std::list<std::shared_ptr<Die>> kostki;
    std::list<Gracz> gracze;
    std::shared_ptr<ScoreBoard> tablica_wynikow;


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
        for (int i = 0; i < rounds; ++i)
        {
            // Graj runde
        }
        // sprawdz kto wygral, czy pozostali zbankrutowali, czy wszystkie rundy przeszly.
    }

    WorldCup2022()
    {
        Plansza temp = Plansza();
        temp.dodaj_pole(new Start("początek sezonu", 50));
        temp.dodaj_pole(new Przy_wejsciu("mecz z San Marino", -160));
        temp.dodaj_pole(new Wolne("dzień wolny od treningu"));
        temp.dodaj_pole(new Przy_wejsciu("mecz z Liechtensteinem", -220));
        temp.dodaj_pole(new Pole_czekajace("żółta kartka", 3));
        temp.dodaj_pole(new Przy_wejsciu("mecz z Meksykiem", -2.5 * 300));
        temp.dodaj_pole(new Przy_wejsciu("mecz z Arabią Saudyjską", -2.5 * 280));
        temp.dodaj_pole(new Okresowe("bukmacher", 300, 3));
        temp.dodaj_pole(new Przy_wejsciu("mecz z Argentyną", -2.5 * 250));
        temp.dodaj_pole(new Przy_wejsciu("gol", 120));
        temp.dodaj_pole(new Przy_wejsciu("mecz z Francją", -4 * 400));
        temp.dodaj_pole(new Przy_wejsciu("rzut karny", -180));
        plansza = std::move(temp);
    }
};


#endif //ZADANIE6_WORLDCUP2022_H
