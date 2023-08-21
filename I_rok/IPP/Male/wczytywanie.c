#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>

#include "labirynt.h"
#include "przydatne.h"
#include "obsluga_bledow.h"

#define DUZO 4294967296 / 8

static void pomin_biale_znaki(int *c)
{
    while (isspace(*c) && (*c) != '\n')
    {
        *c = getchar();
    }
}

static void pomin_zera(int *c)
{
    while (*c == '0')
    {
        *c = getchar();
    }
}

static size_t wczytaj_liczbe(int *c, int *blad)
{
    size_t liczba = 0;
    if (!isdigit(*c))
    {
        *blad = 1;
        return 1;
    }
    while (isdigit(*c))
    {
        if (__builtin_umull_overflow(liczba, 10, &liczba))
        {
            *blad = 1;
            return 1;
        }
        if (__builtin_uaddl_overflow(liczba, (*c) - '0', &liczba))
        {
            *blad = 1;
            return 1;
        }
        *c = getchar();
    }
    return liczba;
}

/**
 * Funkcja wczytuje jedna linie z wejscia.
 * a - tablica opisujaca wczytana linie.
 * n - rozmiar tej tablicy.
 * ktora_linia - informuje o tym ktora linjie obecje wczytujemy.
 * Jesli wystapil blad, powoduje zakonczenie programu z kodem 1 i wypisane
 * na stdout numer linii w ktorej nastapil blad.
 */
static int wczytaj_jedna_linie(size_t **a, size_t *n, int ktora_linia)
{
    int blad = 0, c = getchar();
    size_t *wynik = NULL;
    size_t rozmiar = 0, i = 0, liczba = 0;
    pomin_biale_znaki(&c);
    while (c != EOF && c != '\n')
    {
        if (i == rozmiar)
        {
            if (powieksz_tablice(&rozmiar, &wynik))
            {
                free(wynik);
                return 1;
            }
        }

        liczba = wczytaj_liczbe(&c, &blad);
        if (blad)
        {
            free(wynik);
            printf("ERROR %d\n", ktora_linia);
            return 1;
        }

        wynik[i] = liczba;
        liczba = 0;
        ++i;
        pomin_biale_znaki(&c);
    }
    *a = wynik;
    *n = i;
    return blad;
}

static bool czy_odpowiedni_znak_w_szesnastkowym(int c)
{
    c = toupper(c);
    return (c >= 'A' && c <= 'F');
}

static int zamiana_z_litery_na_liczb(int c)
{
    c = toupper(c);
    return c - 55;
}

/**
 * Wczytuje czwarta linie jezeli jest zapisana w systemie szesnastkowym.
 * Na jednym polu tablicy typu char zapiuje jedna liczbe z wejscia.
 * a - tablica opisujaca wczytana liczbe.
 * n - rozmiar tej tablicy.
 */
static int wczytaj_czwarta_szesnastkowo(unsigned char **wynik, size_t *rozmiar)
{
    unsigned char *tab = NULL;
    size_t wielkosc = 0, i = 0;
    int c = getchar();
    pomin_biale_znaki(&c);
    if (koniec_linii(c))
    {
        printf("ERROR 4\n");
        return 1;
    }
    pomin_zera(&c);
    while (c != EOF && c != '\n')
    {
        if (i == wielkosc)
        {
            powieksz_tablice_char(&wielkosc, &tab);
        }
        if (isdigit(c))
        {
            tab[i] = (c - '0');
        }
        else if (czy_odpowiedni_znak_w_szesnastkowym(c))
        {
            tab[i] = zamiana_z_litery_na_liczb(c);
        }
        else
        {
            break;
        }
        ++i;
        c = getchar();
    }
    pomin_biale_znaki(&c);
    if (!koniec_linii(c))
    {
        free(tab);
        printf("ERROR 4\n");
        return 1;
    }
    *wynik = tab;
    *rozmiar = i;
    return 0;
}

static void dodaj_do_tablicy_posortowanej(size_t *tab, size_t *rozmiar,
                                          size_t x)
{
    size_t i = 0, pom;
    while (i < *rozmiar && x > tab[i])
    {
        ++i;
    }
    if (i == *rozmiar)
    {
        tab[*rozmiar] = x;
    }
    else if (tab[i] != x)
    {
        while (i < *rozmiar)
        {
            pom = tab[i];
            tab[i] = x;
            x = pom;
            ++i;
        }
        tab[*rozmiar] = x;
    }
    else
    {
        *rozmiar = *rozmiar - 1;
    }
}

static int generuj_wi(size_t *tab, size_t **a, size_t *b, size_t max)
{
    size_t *wynik = NULL;
    size_t s = tab[4], w, i, rozmiar = 0, rozmiar_tablicy = 0;

    for (i = 0; i < tab[3]; ++i)
    {
        s = (tab[0] * s + tab[1]) % tab[2];
        w = s % max;
        if (rozmiar == rozmiar_tablicy)
        {
            if (powieksz_tablice(&rozmiar, &wynik))
            {
                free(wynik);
                return 1;
            }
        }
        dodaj_do_tablicy_posortowanej(wynik, &rozmiar_tablicy, w);
        ++rozmiar_tablicy;
    }
    *a = wynik;
    *b = rozmiar_tablicy;
    return 0;
}

static void inicjiuj_tablice_na_0(unsigned char *tab, size_t n)
{
    for (size_t i = 0; i < n; ++i)
    {
        tab[i] = 0;
    }
}

/**
 * Generuje rozwiniecie dwojkowe liczby opisujacej polozenie scian labiryntu.
 * a - tablica opisujaca lab.
 * rozmiar - jej rozmiar.
 * max - iloczyn n_1*n_2*...*n_k.
 * tab - tablica pomocnicza na ktorej sa zapisane liczby wczytane z czwartej
 *       linii po literze "R".
 */
static int zamien_R_na_bity(size_t *tab, size_t *rozmiar,
                            unsigned char **a, size_t max)
{
    size_t *tablica_wi = NULL;
    unsigned char *wynik = NULL;
    size_t rozmiar_wynik = 0, ktora_komorka, rozmiar_tablicy_wi;

    rozmiar_wynik = sufit(max, 8);
    if (mallokuj_tablie_char(&wynik, rozmiar_wynik))
    {
        return 1;
    }

    inicjiuj_tablice_na_0(wynik, rozmiar_wynik);

    if (generuj_wi(tab, &tablica_wi, &rozmiar_tablicy_wi, max))
    {
        return 1;
    }

    // Zapisuje na tablicy "wynik" rozwiniecie dwojkowe liczby opisujacej
    // polozenie scian labiryntu.
    for (size_t i = 0; i < rozmiar_tablicy_wi; ++i)
    {
        ktora_komorka = rozmiar_wynik - (tablica_wi[i] / 8) - 1;

        while (ktora_komorka + DUZO >= DUZO)
        {

            wynik[ktora_komorka] += zamiana_modulo(tablica_wi[i]);
            ktora_komorka = ktora_komorka - DUZO;
        }
    }
    free(tablica_wi);
    *a = wynik;
    *rozmiar = rozmiar_wynik;
    return 0;
}

/**
 * Kompresuje liczbe ktora opisuje polozenie scian labiryntu. Przepisuje
 * ja do 8 bitow przy zachowaniu tego samego rozwiniceia dwojkowego.
 * Jesli wystapil blad, powoduje zakonczenie programu z kodem 1.
 */
static int szesnastkowy_na_256(unsigned char **wynik, size_t *rozmiar,
                               unsigned char *tab)
{
    bool licznik = true;
    size_t rozmiar_wynik;
    unsigned char *pomocnicza = NULL;

    rozmiar_wynik = sufit(*rozmiar, 2);

    if (mallokuj_tablie_char(&pomocnicza, rozmiar_wynik))
    {
        return 1;
    }

    --rozmiar_wynik;
    for (size_t o = *rozmiar; o > 0; --o)
    {
        if (licznik)
        {
            pomocnicza[rozmiar_wynik] = tab[o - 1];
            licznik = false;
        }
        else
        {
            pomocnicza[rozmiar_wynik] += tab[o - 1] * 16;
            licznik = true;
            --rozmiar_wynik;
        }
    }
    // Nie zmieniam rozmiaru tablicy opisujacej labirynt. Robie to pozniej po
    // po sprawdzeniu czy nie miala zaduzo bitow.
    *wynik = pomocnicza;
    return 0;
}

static int wczytaj_czwarta_linje(unsigned char **tablica, size_t *wielkosc,
                                 bool *szesnastkowy, size_t max)
{
    int c = getchar();
    pomin_biale_znaki(&c);
    size_t *tab = NULL;
    size_t rozmiar = 0;
    unsigned char *wynik = NULL;
    if (c == 'R')
    {
        *szesnastkowy = false;
        if (wczytaj_jedna_linie(&tab, &rozmiar, 4))
        {
            free(tab);
            return 1;
        }
        if (czy_wczytalem_5_liczb_mniejszych_od_max32(rozmiar, tab))
        {
            free(tab);
            return 1;
        }

        if (zamien_R_na_bity(tab, &rozmiar, &wynik, max))
        {
            free(tab);
            return 1;
        }
        free(tab);
    }
    else if (c == '0' && (c = getchar()) == 'x')
    {
        *szesnastkowy = true;
        unsigned char *chwilowa = NULL;

        if (wczytaj_czwarta_szesnastkowo(&chwilowa, &rozmiar))
        {
            free(chwilowa);
            return 1;
        }
        if (szesnastkowy_na_256(&wynik, &rozmiar, chwilowa))
        {
            free(chwilowa);
            return 1;
        }
        free(chwilowa);
    }
    else
    {
        return 1;
    }
    *tablica = wynik;
    *wielkosc = rozmiar;
    return 0;
}

/**
 * Wczytuje i zapisuje cztery linie z wejscia.
 * Jesli wystapil blad, powoduje zakonczenie programu z kodem 1
 */
int inicjuj(Tlabirynt *lab, bool *szesnastkowy)
{
    size_t pomoc, max;
    size_t *tab_pomoc = NULL;
    unsigned char *tablica_pomocnicza = NULL;
    int c;

    if (wczytaj_jedna_linie(&tab_pomoc, &pomoc, 1))
    {
        return 1;
    }
    lab->pierwsza = tab_pomoc;
    lab->tablice_r = pomoc;
    if (czy_pierwsz_linia_poprawa(lab->pierwsza, lab->tablice_r))
    {
        return 1;
    }

    if (wczytaj_jedna_linie(&tab_pomoc, &pomoc, 2))
    {
        return 1;
    }
    lab->druga = tab_pomoc;
    if (czy_dane_niepoprawne(lab->pierwsza, lab->tablice_r, tab_pomoc, pomoc))
    {
        printf("ERROR 2\n");
        return 1;
    }

    if (wczytaj_jedna_linie(&tab_pomoc, &pomoc, 3))
    {
        return 1;
    }
    lab->trzecia = tab_pomoc;
    if (czy_dane_niepoprawne(lab->pierwsza, lab->tablice_r, tab_pomoc, pomoc))
    {
        printf("ERROR 3\n");
        return 1;
    }

    max = iloczyn(lab->pierwsza, lab->tablice_r);
    if (wczytaj_czwarta_linje(&tablica_pomocnicza, &pomoc, szesnastkowy, max))
    {
        return 1;
    }
    lab->czwarta = tablica_pomocnicza;
    lab->czwarta_r = pomoc;

    if ((c = getchar()) != EOF)
    {
        printf("ERROR 5\n");
        return 1;
    }
    return 0;
}