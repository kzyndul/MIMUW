#include <stdlib.h>
#include "labirynt.h"

void usun_labirynt(Tlabirynt l)
{
    free(l.czwarta);
    free(l.druga);
    free(l.pierwsza);
    free(l.trzecia);
}

void tworz(Tlabirynt *l)
{
    l->pierwsza = NULL;
    l->druga = NULL;
    l->trzecia = NULL;
    l->czwarta = NULL;
    l->czwarta_r = 0;
    l->tablice_r = 0;
}