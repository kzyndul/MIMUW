#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "lista.h"

int malloc_lista(Tlista **a)
{
    *a = malloc(sizeof(Tlista));
    if (*a == NULL)
    {
        printf("ERROR 0\n");
        return 1;
    }
    return 0;
}

static int mallokuj_wezel(Twezel **a)
{
    *a = malloc(sizeof(Twezel));
    if (*a == NULL)
    {
        return 1;
    }
    return 0;
}

int wstaw_za(Tlista *wsk, Twezel *x)
{
    Tlista *pom;
    if (malloc_lista(&pom))
    {
        return 1;
    }
    pom->nast = wsk->nast;
    pom->wezel = x;
    wsk->nast = pom;
    return 0;
}

int wstaw_na_koniec(Tlista **wsk, Twezel *x)
{
    Tlista *pom = *wsk;
    if (pom == NULL)
    {
        if (malloc_lista(&pom))
        {
            return 1;
        }
        pom->nast = NULL;
        pom->wezel = x;
        *wsk = pom;
    }
    else
    {
        while (pom->nast != NULL)
        {
            pom = pom->nast;
        }
        if (wstaw_za(pom, x))
        {
            return 1;
        }
    }
    return 0;
}

Twezel *nowy_wezel(size_t *a, int *blad)
{
    Twezel *wynik;
    if (mallokuj_wezel(&wynik))
    {
        printf("ERROR 0\n");
        *blad = 1;
        return NULL;
    }
    wynik->tab = a;
    wynik->sasiedzi = NULL;
    *blad = 0;
    return wynik;
}

void usun_wezel(Twezel *w)
{
    Twezel *pom;
    Tlista *pom2;
    while (w->sasiedzi != NULL)
    {
        pom = w->sasiedzi->wezel;
        usun_wezel(pom);
        pom2 = w->sasiedzi;
        w->sasiedzi = w->sasiedzi->nast;
        free(pom2);
    }
    free(w->tab);
    free(w);
}