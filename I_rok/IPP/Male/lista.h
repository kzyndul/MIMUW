#ifndef LISTA_H
#define LISTA_H

typedef struct lista Tlista;
typedef struct wezel Twezel;

struct lista
{
    struct wezel *wezel;
    struct lista *nast;
};

struct wezel
{
    size_t *tab;
    struct lista *sasiedzi;
};

Twezel *nowy_wezel(size_t *a, int *blad);

void usun_wezel(Twezel *w);

int wstaw_za(Tlista *wsk, Twezel *x);

int wstaw_na_koniec(Tlista **wsk, Twezel *x);

int malloc_lista(Tlista **a);

int mallokuj_tablice_size_t(size_t **a, size_t rozmiar);

#endif