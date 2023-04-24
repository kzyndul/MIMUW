#ifndef PRZYDATNE_H
#define PRZYDATNE_H
#include "lista.h"

int powieksz_tablice(size_t *a, size_t **tab);

int powieksz_tablice_char(size_t *a, unsigned char **tab);

size_t iloczyn(size_t *tab, size_t n);

int zamiana_modulo(size_t a);

size_t sufit(size_t a, int b);

int mallokuj_tablie_char(unsigned char **a, size_t rozmiar);

int mallokuj_tablice_size_t(size_t **a, size_t rozmiar);

#endif