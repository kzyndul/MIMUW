#ifndef LABIRYNT_H
#define LABIRYNT_H

struct labirynt
{
    size_t *pierwsza;
    size_t *druga;
    size_t *trzecia;
    size_t tablice_r;
    unsigned char *czwarta;
    size_t czwarta_r;
};

typedef struct labirynt Tlabirynt;

void usun_labirynt(Tlabirynt l);

void tworz(Tlabirynt *l);

#endif