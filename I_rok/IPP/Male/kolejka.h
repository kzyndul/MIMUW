#ifndef KOLEJKA_H
#define KOLEJKA_H
#include "lista.h"

struct kolejka
{
    Tlista *pocz;
    Tlista *kon;
};

typedef struct kolejka Tkolejka;

bool Pusta(Tkolejka k);

void Tworz_pusta(Tkolejka *k);

int Wstaw(Tkolejka *k, Twezel *x);

Twezel *Pobierz(Tkolejka *k);

void usun_kolejke(Tkolejka *k);

#endif