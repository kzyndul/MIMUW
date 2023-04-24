#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "labirynt.h"
#include "kolejka.h"
#include "przydatne.h"

static bool czy_pole_koncowe(Twezel *w, Tlabirynt labirynt)
{
    for (size_t i = 0; i < labirynt.tablice_r; ++i)
    {
        if (w->tab[i] != labirynt.trzecia[i])
        {
            return false;
        }
    }
    return true;
}

static size_t *kopiuj_tablic(size_t *a, size_t n, int *blad)
{
    size_t *wynik;
    if (mallokuj_tablice_size_t(&wynik, n))
    {
        *blad = 1;
        return NULL;
    }
    for (size_t i = 0; i < n; ++i)
    {
        wynik[i] = a[i];
    }
    return wynik;
}
/**
 * Sprawdza czy dane pole miesci sie w zakresie labiryntu
 */
static bool czy_odpowiednie_pole(Tlabirynt labirynt, size_t i,
                                 int a, Twezel wezel)
{
    size_t sprawdzany = wezel.tab[i] + (size_t)a;
    return (sprawdzany > 0 && sprawdzany <= labirynt.pierwsza[i]);
}

/**
 * Wyliczaj jaki numer bedzie mial bit opisujacy kostke o danych wspolrzednych
 */
static size_t wylicznie_bitu(size_t *tablica, size_t i,
                             int odjemnik, Tlabirynt labirynt)
{
    tablica[i] += (size_t)odjemnik;
    size_t wynik = 0, iloczyn = 1;
    for (size_t j = 0; j < labirynt.tablice_r; ++j)
    {
        wynik += (tablica[j] - 1) * iloczyn;
        iloczyn *= labirynt.pierwsza[j];
    }
    tablica[i] -= odjemnik;
    return wynik;
}

static bool czy_puste_pole(size_t x, Tlabirynt labirynt)
{
    size_t nasz_indeks = labirynt.czwarta_r - 1 - (x / 8);
    return !(labirynt.czwarta[nasz_indeks] & zamiana_modulo(x));
}

static void zmien_bit_na_zero(size_t x, Tlabirynt *labirynt)
{
    size_t nasz_indeks = labirynt->czwarta_r - 1 - (x / 8);
    size_t pomoc = zamiana_modulo(x);
    labirynt->czwarta[nasz_indeks] = (labirynt->czwarta[nasz_indeks] | pomoc);
}

/**
 * Przetwarza jeden wierzcholek grafu. Sprawdza czy pola sasiadujace z kostak
 * sa odpowiedni. Czyli czy mieszcza sie one w labiryncie i czy sa puste.
 * Jesli program zakonczy sie bledem zwraca kod 1.
 * wezel - zawiera wspolrzedne sprawdzanej kostki
 * labirynt - zawiera wymiary labiryntu i inforamcje gdzie sa puste kostki
 * magazyn - kolejka na ktora dodaje "dobre kostki"
 */
static int przetwarzanie(Twezel *wezel, Tlabirynt labirynt, Tkolejka *magazyn)
{
    int blad = 0, odjemnik = -1;
    bool odpowiednie, puste;
    size_t bit;

    for (size_t i = 0; i < 2 * labirynt.tablice_r; ++i)
    {
        puste = false;
        odpowiednie = czy_odpowiednie_pole(labirynt, i / 2, odjemnik, *wezel);
        if (odpowiednie)
        {
            bit = wylicznie_bitu(wezel->tab, i / 2, odjemnik, labirynt);
            puste = czy_puste_pole(bit, labirynt);
        }
        if (puste)
        {
            // Jezeli kostak jest odpowiednia to tworze nowy wierzcholek grafu,
            // wstawiam go do kolejki
            size_t *tab = kopiuj_tablic(wezel->tab, labirynt.tablice_r, &blad);
            if (blad)
            {
                return 1;
            }
            tab[i / 2] += odjemnik;
            Twezel *sasiad = nowy_wezel(tab, &blad);
            if (blad)
            {
                free(tab);
                return 1;
            }
            if (wstaw_na_koniec(&(wezel->sasiedzi), sasiad))
            {
                usun_wezel(sasiad);
                return 1;
            }
            if (Wstaw(magazyn, sasiad))
            {
                usun_wezel(sasiad);
                return 1;
            }
            zmien_bit_na_zero(bit, &labirynt);
        }
        odjemnik *= -1;
    }
    return 0;
}
/**
 * Jezeli liczba opisujaca labirynt ma zamalo bitow to dodaje do niej
 * zera wiodace.
 * Jesli program zakonczy sie bledem zwraca kod 1.
 */
static int dodaj_zer_wiodacych(Tlabirynt *labirynt, bool *czy_zwiekszane)
{
    size_t ile, max = iloczyn(labirynt->pierwsza, labirynt->tablice_r);
    if (max > labirynt->czwarta_r * 8)
    {
        *czy_zwiekszane = true;
        ile = sufit(max - labirynt->czwarta_r * 8, 8);
        unsigned char *wynik = NULL;
        if (mallokuj_tablie_char(&wynik, ile + labirynt->czwarta_r))
        {
            return 1;
        }
        for (size_t i = 0; i < ile; ++i)
        {
            wynik[i] = 0;
        }
        for (size_t j = ile; j < ile + labirynt->czwarta_r; ++j)
        {
            wynik[j] = labirynt->czwarta[j - ile];
        }
        labirynt->czwarta = wynik;
        labirynt->czwarta_r = ile + labirynt->czwarta_r;
    }
    return 0;
}

static void wyczysc(Tkolejka *m, Tkolejka *n, Twezel *w,
                    unsigned char *t, bool czy_zwiekszana)
{
    usun_kolejke(n);
    usun_kolejke(m);
    usun_wezel(w);
    if (czy_zwiekszana)
    {
        free(t);
    }
}

static int sprawdz_pozycje_poczatkowa_koncowa(Tlabirynt labirynt)
{
    bool puste_pole_s, puste_pole_k;
    size_t bit_poczatkowy, bit_koncowy;

    bit_poczatkowy = wylicznie_bitu(labirynt.druga, 0, 0, labirynt);
    puste_pole_s = czy_puste_pole(bit_poczatkowy, labirynt);
    bit_koncowy = wylicznie_bitu(labirynt.trzecia, 0, 0, labirynt);
    puste_pole_k = czy_puste_pole(bit_koncowy, labirynt);
    zmien_bit_na_zero(bit_poczatkowy, &labirynt);

    if (!puste_pole_s)
    {
        printf("ERROR 2\n");
        return 1;
    }
    else if (!puste_pole_k)
    {
        printf("ERROR 3\n");
        return 1;
    }
    return 0;
}

static int czy_moge_zaalokowac_pamiec(Tlabirynt labirynt)
{
    bool czy_zwiekszane = false;
    Tkolejka magazyn1, magazyn2;
    int blad = 0;
    size_t *kopia = kopiuj_tablic(labirynt.druga, labirynt.tablice_r, &blad);
    Tworz_pusta(&magazyn1);
    Tworz_pusta(&magazyn2);

    if (blad)
    {
        usun_kolejke(&magazyn1);
        free(kopia);
        return 1;
    }

    Twezel *pole_startowe = nowy_wezel(kopia, &blad);
    if (blad)
    {
        usun_kolejke(&magazyn1);
        free(kopia);
        return 1;
    }

    if (Wstaw(&magazyn1, pole_startowe))
    {
        wyczysc(&magazyn1, &magazyn2, pole_startowe,
                labirynt.czwarta, czy_zwiekszane);
        return 1;
    }

    if (dodaj_zer_wiodacych(&labirynt, &czy_zwiekszane))
    {
        wyczysc(&magazyn1, &magazyn2, pole_startowe,
                labirynt.czwarta, czy_zwiekszane);
        return 1;
    }
    wyczysc(&magazyn1, &magazyn2, pole_startowe,
            labirynt.czwarta, czy_zwiekszane);
    return 0;
}

/**
 *  Algorytm przeszukiwania grafu wszerz.
 *  Jesli nastapi blad na zmiennej "blad" zapisuje 1 i konczy dzialanie
 */
size_t przeszukiwanie_wszerz(Tlabirynt labirynt, bool *jest_droga, int *blad)
{
    bool czy_zwiekszane = false;
    size_t wynik = 0, i = 0;
    Tkolejka magazyn1, magazyn2;
    Tworz_pusta(&magazyn1);
    Tworz_pusta(&magazyn2);

    if (czy_moge_zaalokowac_pamiec(labirynt))
    {
        *blad = 1;
        return 1;
    }

    // Inicjowanie pozycji startowej i sprawdzanie jej poprawnosci.
    size_t *kopia = kopiuj_tablic(labirynt.druga, labirynt.tablice_r, blad);
    Twezel *obecny, *pole_startowe = nowy_wezel(kopia, blad);
    Wstaw(&magazyn1, pole_startowe);
    dodaj_zer_wiodacych(&labirynt, &czy_zwiekszane);
    if (sprawdz_pozycje_poczatkowa_koncowa(labirynt))
    {
        *blad = 1;
        wyczysc(&magazyn1, &magazyn2, pole_startowe,
                labirynt.czwarta, czy_zwiekszane);
        return wynik;
    }

    while ((!Pusta(magazyn1) || !Pusta(magazyn2)) && !*jest_droga)
    {
        while (!Pusta(magazyn1))
        {
            obecny = Pobierz(&magazyn1);
            if (czy_pole_koncowe(obecny, labirynt))
            {
                wynik = i;
                *jest_droga = true;
                break;
            }
            if (przetwarzanie(obecny, labirynt, &magazyn2))
            {
                wyczysc(&magazyn1, &magazyn2, pole_startowe,
                        labirynt.czwarta, czy_zwiekszane);
                *blad = 1;
                return 1;
            }
        }
        ++i;
        while (!Pusta(magazyn2) && !*jest_droga)
        {
            obecny = Pobierz(&magazyn2);
            if (czy_pole_koncowe(obecny, labirynt))
            {
                wynik = i;
                *jest_droga = true;
                break;
            }
            if (przetwarzanie(obecny, labirynt, &magazyn1))
            {
                wyczysc(&magazyn1, &magazyn2, pole_startowe,
                        labirynt.czwarta, czy_zwiekszane);
                *blad = 1;
                return 1;
            }
        }
        ++i;
    }

    wyczysc(&magazyn1, &magazyn2, pole_startowe,
            labirynt.czwarta, czy_zwiekszane);
    return wynik;
}