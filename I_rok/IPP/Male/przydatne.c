#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#include "labirynt.h"
#include "lista.h"

int mallokuj_tablie_char(unsigned char **a, size_t rozmiar)
{
    *a = malloc(rozmiar * sizeof(**a));
    if (*a == NULL)
    {
        printf("ERROR 0\n");
        return 1;
    }
    return 0;
}

int mallokuj_tablice_size_t(size_t **a, size_t rozmiar)
{
    *a = malloc(rozmiar * sizeof(**a));
    if (*a == NULL)
    {
        printf("ERROR 0\n");
        return 1;
    }
    return 0;
}

static size_t wiecej(size_t n)
{
    return (n / 2 + 1) * 3;
}

int powieksz_tablice(size_t *a, size_t **tab)
{
    *a = wiecej(*a);
    *tab = realloc(*tab, (*a) * sizeof **tab);
    if (*tab == NULL)
    {
        printf("ERROR 0\n");
        return 1;
    }
    return 0;
}

int powieksz_tablice_char(size_t *a, unsigned char **tab)
{
    *a = wiecej(*a);
    *tab = realloc(*tab, (size_t)(*a) * sizeof **tab);
    if (*tab == NULL)
    {
        printf("ERROR 0\n");
        return 1;
    }
    return 0;
}

size_t iloczyn(size_t *tab, size_t n)
{
    size_t iloczyn = 1;
    for (size_t i = 0; i < n; ++i)
    {
        iloczyn *= tab[i];
    }
    return iloczyn;
}

static int potega(int a, int b)
{
    if (b == 0)
    {
        return 1;
    }
    else
    {
        return a * potega(a, b - 1);
    }
}

int zamiana_modulo(size_t a)
{
    return potega(2, a % 8);
}

size_t sufit(size_t a, int b)
{
    if (a % b == 0)
    {
        return (a / b);
    }
    else
    {
        return (a / b) + 1;
    }
}