#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>

#include "labirynt.h"

int czy_pierwsz_linia_poprawa(size_t *tab, size_t rozmiar)
{
    if (tab == NULL)
    {
        printf("ERROR 1\n");
        return 1;
    }
    size_t iloczy = 1;
    for (size_t i = 0; i < rozmiar; ++i)
    {
        // Sprawdzam czy iloczyn n_1*...*n_k nie przekracza SIZE_MAX
        if (__builtin_umull_overflow(iloczy, tab[i], &iloczy))
        {
            printf("ERROR 0\n");
            return 1;
        }
        if (tab[i] == 0)
        {
            printf("ERROR 1\n");
            return 1;
        }
    }
    return 0;
}

int czy_wczytalem_5_liczb_mniejszych_od_max32(size_t a, size_t *b)
{
    if (a != 5 || b[2] == 0)
    {
        printf("ERROR 4\n");
        return 1;
    }
    for (int i = 0; i < 5; ++i)
    {
        if (b[i] > UINT32_MAX)
        {
            printf("ERROR 4\n");
            return 1;
        }
    }
    return 0;
}
/**
 * Sprawdza czy dane wczytane z drugiel/trzeciej linii wejscia sa poprawne.
 * Czyli czy pozycja startowa/koncowa jest dodatnia, ma tyle samo wspolrzednych co
 * wymiarow ma labirynt i czy znajduje sie w nim.
 * Jesli wystapil blad konczy dzialanie i przekazuje kod 1;
 */
int czy_dane_niepoprawne(size_t *tab1, size_t rozmiar1,
                         size_t *tab2, size_t rozmiar2)
{
    if (rozmiar1 != rozmiar2)
    {
        return 1;
    }
    for (size_t i = 0; i < rozmiar1; ++i)
    {
        if (tab2[i] < 1 || tab1[i] < tab2[i])
        {
            return 1;
        }
    }
    return 0;
}

int koniec_linii(int c)
{
    if (c == EOF || c == '\n')
    {
        return 1;
    }
    return 0;
}