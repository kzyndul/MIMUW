#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "kolejka.h"

bool Pusta(Tkolejka k)
{
    return (k.pocz == NULL);
}

void Tworz_pusta(Tkolejka *k)
{
    k->pocz = NULL;
}

int Wstaw(Tkolejka *k, Twezel *x)
{
    if (k->pocz == NULL)
    {
        if (malloc_lista(&k->pocz))
        {
            return 1;
        }
        k->pocz->wezel = x;
        k->kon = k->pocz;
    }
    else
    {
        if (wstaw_za(k->kon, x))
        {
            return 1;
        }
        k->kon = k->kon->nast;
    }
    return 0;
}

Twezel *Pobierz(Tkolejka *k)
{
    Tlista *pom = k->pocz;
    Twezel *x = k->pocz->wezel;
    if (k->pocz == k->kon)
    {
        k->pocz = NULL;
    }
    else
    {
        k->pocz = k->pocz->nast;
    }
    free(pom);
    return x;
}

void usun_kolejke(Tkolejka *k)
{
    while (!Pusta(*k))
    {
        Pobierz(k);
    }
}