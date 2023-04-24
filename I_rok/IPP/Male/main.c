#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "labirynt.h"
#include "wczytywanie.h"
#include "przetwarzanie.h"
#include "lista.h"
#include "przydatne.h"
#include "obsluga_bledow.h"

static int czy_liczba_opisujaca_polozenie_ma_zaduzo_bitow(Tlabirynt labirynt)
{
    size_t max = iloczyn(labirynt.pierwsza, labirynt.tablice_r);
    if (max < labirynt.czwarta_r * 4)
    {
        printf("ERROR 4\n");
        return 1;
    }
    return 0;
}

static void rezultat(size_t wynik, bool czy_jest_droga)
{
    if (!czy_jest_droga)
    {
        printf("NO WAY\n");
    }
    else
    {
        printf("%zu\n", wynik);
    }
}

int main(void)
{
    bool szesnastkowy, czy_jest_droga = false;
    int blad = 0;
    Tlabirynt labirynt;
    size_t wynik;
    tworz(&labirynt);

    if (inicjuj(&labirynt, &szesnastkowy))
    {
        usun_labirynt(labirynt);
        return 0;
    }

    if (szesnastkowy)
    {
        if (czy_liczba_opisujaca_polozenie_ma_zaduzo_bitow(labirynt))
        {
            usun_labirynt(labirynt);
            return 0;
        }
        labirynt.czwarta_r = sufit(labirynt.czwarta_r, 2);
    }

    wynik = przeszukiwanie_wszerz(labirynt, &czy_jest_droga, &blad);
    if (blad)
    {
        usun_labirynt(labirynt);
        return 0;
    }

    rezultat(wynik, czy_jest_droga);
    usun_labirynt(labirynt);
    return 0;
}