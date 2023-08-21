/** @file
 * Implementuje dynamiczny string i funkcjie zajmujące się pamięcią.
 *
 * @author Krzysztof Żyndul <kz438842@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include <stdlib.h>
#include <stdbool.h>
#include "przydatne.h"

/** @brief Zwiększa liczbę.
 * Zwraca liczbę trzy drugie raza większą oo @p a. Po podzieleniu przez 2
 * dodaję 1 żeby nie było problemu przy @p a = 0.
 * @param a - liczba którą mam zwiększyć.
 * @return Nowa wartość.
 */
static size_t wiecej(size_t a)
{
    return a * 3 / 2 + 2;
}

/** @brief Zmniejsza liczbę.
 * Zwraca liczbę o połowę mniejszą od @p n.
 * @param a - liczba którą mam zmniejszyć.
 * @return Nowa wartość.
 */
static size_t mniej(size_t a)
{
    return a / 2;
}

/** @brief Realokuje tablice char.
 * Realukuje tablice char o rozmiarze @p wielkosc i zapisuje ją na @p a.
 * @param a       - wskaźnik na wskaźnik na którym ma być nowa tablica.
 * @param wielkosc - rozmiar tablicy, którą chcemy zaalokować.
 * @return Wartość @p false jeżeli allokacja się powiodła w przeciwnym przypadku
 *         @p true jeżeli nie udało się przydzielić pamięci.
 */
static bool reallokuj_tablice_char(char **a, size_t wielkosc)
{
    *a = realloc(*a, wielkosc * sizeof (**a));
    return *a == NULL;
}

Tstring *inicjuj(void)
{
    Tstring *a = malloc(sizeof (*a));
    if (a)
    {
        a->string = NULL;
        a->wielkosc = 0;
        a->wolny_indeks = 0;
    }
    return a;
}

bool dodaj_element(Tstring *a, char b)
{
    if (a->wolny_indeks + 1 >= a->wielkosc)
    {
        a->wielkosc = wiecej(a->wielkosc);
        if (reallokuj_tablice_char(&(a->string), a->wielkosc))
        {
            return false;
        }
    }
    a->string[a->wolny_indeks] = b;
    ++a->wolny_indeks;
    a->string[a->wolny_indeks] = '\0';
    return true;
}

void usun_element(Tstring *a)
{
    if (a != NULL)
    {
        if (4 * a->wolny_indeks < a->wielkosc && a->wielkosc > 2)
        {
            a->wielkosc = mniej(a->wielkosc);
            reallokuj_tablice_char(&(a->string), a->wielkosc);
        }
        if (a->wolny_indeks != 0)
        {
            --a->wolny_indeks;
        }
        if (a->string)
        {
            a->string[a->wolny_indeks] = '\0';
        }
    }
}

void usun(Tstring *a)
{
    if(a != NULL)
    {
        free(a->string);
    }
    free((a));
}

int mallokuj_tablice_char(char **a, size_t rozmiar)
{
    *a = malloc(rozmiar * sizeof(**a));
    if (*a == NULL)
    {
        return 0;
    }
    return 1;
}

bool powieksz_tablice_numerow(size_t *wielkosc, const char ***a)
{
    int nowa_wielkosc = wiecej(*wielkosc);
    *wielkosc = wiecej(*wielkosc);
    const char **pomoc = NULL;
    pomoc = realloc(*a, nowa_wielkosc * sizeof (const char **));
    if (pomoc)
    {
        *a = pomoc;
        *wielkosc = nowa_wielkosc;
        return true;
    }
    return false;
}