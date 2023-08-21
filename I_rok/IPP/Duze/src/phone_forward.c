/** @file
 * Implementacja klasy przechowującej przekierowania numerów telefonów.
 *
 * @author Krzysztof Żyndul <kz438842@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#include <stdlib.h>
#include <ctype.h>
#include "phone_forward.h"
#include "przydatne.h"

/** @brief Długość alfabetu.
 */
#define ILOSC_CYFR 12

/** To jest struktura przechowująca ciąg numerów telefonów.
 */
struct PhoneForward {
    struct PhoneForward *dzieci[ILOSC_CYFR]; /**< Tablica synów węzła. */
    char const *przekierowanie;              /**< numer na który ten numer
                                                  został przekierowany */
    struct PhoneForward *Ojciec;             /**< Wskaźnik na ojca lub NULL
                                                  gdy to korzeń. */
    int ktory_syn;                           /**< Informacja, którym synem jest
                                                  węzeł lu -1 gdy korzeń. */
};

/** To jest struktura przechowująca przekierowania numerów telefonów.
 */
struct PhoneNumbers {
    char const **tablica_numerow; /**< Tablica numerów. */
    size_t pierwszy_wolny_indeks; /**< Indeks pierwszego wolnego elementu
                                  tablicy. */
    size_t wielkosc_tablicy;      /**< Rozmiar tablicy. */
};

/** @brief Tworzy nową strukturę.
 * Tworzy nową strukturę niezawierającą żadnych numerów telefonów.
 * @return Wskaźnik na utworzoną strukturę lub NULL, gdy nie udało się
 *         alokować pamięci.
 */
static PhoneNumbers *phonedNew(void)
{
    PhoneNumbers *a = malloc(sizeof(*a));
    if (a)
    {
        a->pierwszy_wolny_indeks = 0;
        a->wielkosc_tablicy = 0;
        a->tablica_numerow = NULL;
    }
    return a;
}

/** @brief Zwraca długość numeru.
 * Zwraca długość numeru przekazanego przez @p num. Zwraca zero jeżeli
 * wskaźnik ten ma wartość NULL.
 * @param num - wskaźnik na numeru którego długość chcemy policzyć.
 * @return Długość numeru lub 0, jeżeli wskaźnik był NULL.
 */
static size_t dlugosc(char const *num)
{
    if (num != NULL)
    {
        size_t i;
        for (i = 0; num[i] != '\0'; ++i)
        {}
        return i + 1;
    }
    return 0;
}

/** @brief Kopiuje tablice.
 * Kopiuje tablicę wskazywaną przez @p num2, zwraca ją jako wynik. Zwraca NULL
 * jeżeli @p num2 jest NULL'em lub nie udało się przydzielić pamięci.
 * @param num2 - wskaźnik na tablicę, której kopię chcę otrzymać.
 * @return kopia tablicy @p num2 lub NULL, gdy nie udało się przydzielić
 * pamięci.
 */
static char const *kopiuj_tablice(char const *num2)
{
    if (num2 != NULL)
    {
        char *wynik = NULL;
        size_t wielkosc = dlugosc(num2);
        if (!mallokuj_tablice_char(&wynik, wielkosc))
        {
            return NULL;
        }
        for (size_t i = 0; i < wielkosc; ++i)
        {
            wynik[i] = num2[i];
        }
        return wynik;
    }
    return NULL;
}

/** @brief Dodaje numer.
 * Dodaje numer do przechowania do @p telefony na pierwsze wolne miejsce.
 * @param telefony - wskaźnik na strukturę przechowującą numery telefonów.
 * @param num      - wskaźnik na napis reprezentujący numer.
 * @return Wartość @p 1 jeżeli udało się dodać numer.
 *         Wartość @p 0 jeżeli nie udało się dodać numeru.
 */
static int addPhone(PhoneNumbers *telefony, char const *num)
{
    if (telefony->pierwszy_wolny_indeks == telefony->wielkosc_tablicy)
    {
        if (!powieksz_tablice_numerow(&(telefony->wielkosc_tablicy),
                                      &(telefony->tablica_numerow)))
        {
            return 0;
        }
    }
    if (telefony->tablica_numerow != NULL)
    {
        telefony->tablica_numerow[telefony->pierwszy_wolny_indeks] =
                kopiuj_tablice(num);

        if (telefony->tablica_numerow[telefony->pierwszy_wolny_indeks] == NULL
            && num != NULL)
        {
            return 0;
        }
        ++telefony->pierwszy_wolny_indeks;
    }
    else
    {
        return 0;
    }
    return 1;
}

char const *phnumGet(PhoneNumbers const *pnum, size_t idx)
{
    if (pnum != NULL && pnum->pierwszy_wolny_indeks > idx)
    {
        return pnum->tablica_numerow[idx];
    }
    return NULL;
}

/** @brief Konwertuje char na int.
 * Konwertuje zmienną typu char na odpowiadającą jej wartość typu int. Zgodnie z
 * definicją alfabetu.
 * @param a - zmienna którą należy konwertować.
 * @return Wartość zmiennej @p a po konwersji.
 */
static int char_na_inta(char a)
{
    if (isdigit(a))
    {
        return a - '0';
    }
    else if (a == '*')
    {
        return 10;
    }
    else if (a == '#')
    {
        return 11;
    }
    return -12;
}

/** @brief Konwertuje int na char.
 * Konwertuje zmienną typu int na odpowiadającą jej wartość typu char. Zgodnie z
 * definicją alfabetu.
 * @param a - zmienna którą należy konwertować.
 * @return Wartość zmiennej @p a po konwersji.
 */
static int int_na_chara(int a)
{
    if (a >= 0 && a <= 9)
    {
        return a + '0';
    }
    else if (a == 10)
    {
        return '*';
    }
    return '#';
}

void phnumDelete(PhoneNumbers *pnum)
{
    if (pnum != NULL)
    {
        if (pnum->tablica_numerow)
        {
            for (size_t i = 0; i < pnum->pierwszy_wolny_indeks; ++i)
            {
                free((char *) pnum->tablica_numerow[i]);
            }
            free(pnum->tablica_numerow);
        }
        free(pnum);
    }
}

/** @brief Tworzy nowy węzeł.
 * Tworzy nowy węzeł, którego ojcem jest @p a, wywołanie a->dzieci[ @p b ]
 * zwróci allokowany węzeł.
 * @param a - wskaźnik na ojca.
 * @param b - informacja którym synem jest.
 * @return Wskaźnik na nowy węzeł drzewa, lub NULL jeżeli nie udało się
 *         przyznać pamięci.
 */
static PhoneForward *nowy_wezel(PhoneForward *a, int b)
{
    PhoneForward *wynik = NULL;
    wynik = malloc(sizeof(*wynik));

    if (wynik)
    {
        wynik->przekierowanie = NULL;
        for (int i = 0; i < ILOSC_CYFR; ++i)
        {
            wynik->dzieci[i] = NULL;
        }
        wynik->Ojciec = a;
        wynik->ktory_syn = b;
    }
    return wynik;
}

PhoneForward *phfwdNew(void)
{
    return nowy_wezel(NULL, -1);
}

/** @brief Sprawdza czy znak jest elementem alfabetu.
 * Sprawdza czy znak przekazany jako @p c jest znakiem o odpowiednim kodzie
 * ascii.
 * @param c - sprawdzany znak.
 * @return Wartość @p true jeżeli znak @p c jest poprawna literą alfabetu, w
 *         przeciwnym przypadku @p false.
 */
static bool jest_cyfra(char c)
{
    return (isdigit(c) > 0 || c == '*' || c == '#');
}

/** @brief Sprawdza poprawność numeru.
 * Sprawdza czy przydatne wskazywany przez zmienna @p num jest poprawnym numerem.
 * @param num - wskaźnik na tablicę przechowująca numeru.
 * @return Wartość @p true jeżeli wskaźnik @p num wskazuje na numer
 *         zawierającego poprawny numer w przeciwnym przypadku zwraca @p false.
 */
static bool czy_poprawny_numer(char const *num)
{
    if (num != NULL)
    {
        size_t dlugosc = 0;
        while (jest_cyfra(num[dlugosc]))
        {
            ++dlugosc;
        }
        return (num[dlugosc] == '\0' && dlugosc != 0);
    }
    return false;
}

/** @brief Sprawdza czy dwa numery są takie same.
 * Sprawdza czy numer @p num1 jest taki sam jak numer @p num2.
 * @param num1 - wskaźnik na pierwszy numer.
 * @param num2 - wskaźnik na drugi numer.
 * @return Wartość @p true jeżeli dwa numery są identyczne, lub @p false
 *         jeżeli nie są.
 */
static bool czy_takie_same_napisy(char const *num1, char const *num2)
{
    if (num1 != NULL && num2 != NULL)
    {
        size_t i = 0;
        while (jest_cyfra(num1[i]) && jest_cyfra(num2[i]))
        {
            if (num1[i] != num2[i])
            {
                return false;
            }
            ++i;
        }
        return (num1[i] == num2[i] && num1[i] == '\0');
    }
    return false;
}

void phfwdDelete(PhoneForward *pf)
{
    if (pf != NULL)
    {
        int n = 0;
        PhoneForward *pomoc = pf;
        PhoneForward *JD = pf->Ojciec;
        while (pomoc != JD)
        {
            for (; n < ILOSC_CYFR; ++n)
            {
                if (pomoc->dzieci[n] != NULL)
                {
                    pomoc = pomoc->dzieci[n];
                    n = -1;
                }
            }
            free((char *) pomoc->przekierowanie);
            n = pomoc->ktory_syn + 1;
            PhoneForward *v = pomoc;
            pomoc = pomoc->Ojciec;
            if (pomoc)
            {
                pomoc->dzieci[n - 1] = NULL;
            }
            free(v);
        }
    }
}

bool phfwdAdd(PhoneForward *pf, char const *num1, char const *num2)
{
    if (pf != NULL)
    {
        if ((czy_poprawny_numer(num1) && czy_poprawny_numer(num2)) &&
            !czy_takie_same_napisy(num1, num2))
        {
            int i = 0, ktorego_syna = -1;
            PhoneForward *pomoc = pf, *usun = NULL;
            bool czy_dodalem = false;

            while (num1[i] != '\0')
            {
                if (pomoc->dzieci[char_na_inta(num1[i])] == NULL)
                {
                    pomoc->dzieci[char_na_inta(num1[i])] =
                            nowy_wezel(pomoc, char_na_inta(num1[i]));
                    if (!czy_dodalem)
                    {
                        czy_dodalem = true;
                        usun = pomoc;
                        ktorego_syna = i;
                    }
                }
                if (pomoc->dzieci[char_na_inta(num1[i])] == NULL)
                {
                    phfwdDelete(usun->dzieci[ktorego_syna]);
                    usun->dzieci[ktorego_syna] = NULL;
                    return false;
                }
                pomoc = pomoc->dzieci[char_na_inta(num1[i])];
                ++i;
            }
            free((char *) pomoc->przekierowanie);
            pomoc->przekierowanie = kopiuj_tablice(num2);
            if (!pomoc->przekierowanie)
            {
                phfwdDelete(usun->dzieci[ktorego_syna]);
                usun->dzieci[ktorego_syna] = NULL;
                return false;
            }
            return (pomoc->przekierowanie != NULL);
        }
        return false;
    }
    return false;
}

void phfwdRemove(PhoneForward *pf, char const *num)
{
    if (pf != NULL && czy_poprawny_numer(num))
    {
        PhoneForward *pomoc = pf;
        size_t i = 0;
        while (num[i] != '\0' && pomoc != NULL)
        {
            pomoc = pomoc->dzieci[char_na_inta(num[i])];
            ++i;
        }
        if (pomoc != NULL)
        {
            PhoneForward *JD = pomoc->Ojciec;
            int k = pomoc->ktory_syn;
            phfwdDelete(pomoc);
            JD->dzieci[k] = NULL;
        }
    }
}

/** @brief Łączy dwa numery.
 * Łączy dwa numery, zamienia pierwsze @p i cyfr numeru @p num1 na numer
 * @p num2. Jeżeli @p num2 jest NULL'em to kopiuje @p num1.
 * @param num1 - wskaźnik na numer, którego pierwsze @p i cyfr zamieniamy.
 * @param num2 - wskaźnik na numer którym zastępujemy pierwsze @p i cyfr.
 * @param i    - ile pierwszych cyfr zamieniamy.
 * @return Wskaźnik na numery będący wynikiem połączenia @p num1 z @p num2, lub
 *         NULL jeżeli nie udało się przydzielić pamięci.
 */
static char const *polacz_stringi(char const *num1, char *num2, size_t i)
{
    if (czy_poprawny_numer(num1) && czy_poprawny_numer(num2))
    {
        size_t wielkosc = dlugosc(num1) + dlugosc(num2) - 1 - i;
        size_t j;
        char *wynik = NULL;
        if (!mallokuj_tablice_char(&wynik, wielkosc))
        {
            return NULL;
        }
        for (j = 0; num2[j] != '\0'; ++j)
        {
            wynik[j] = num2[j];
        }
        for (; j < wielkosc; ++j)
        {
            wynik[j] = num1[i];
            ++i;
        }
        return wynik;
    }
    else if (czy_poprawny_numer(num1))
    {
        return kopiuj_tablice(num1);
    }
    else if (czy_poprawny_numer(num2))
    {
        return kopiuj_tablice(num2);
    }
    return NULL;

}

PhoneNumbers *phfwdGet(PhoneForward const *pf, char const *num)
{
    if (pf != NULL)
    {
        if (czy_poprawny_numer(num))
        {
            size_t i = 0, j = 0;
            char *wynik = NULL;
            PhoneForward const *pomoc = pf;

            while (num[i] != '\0' && pomoc != NULL)
            {
                if (pomoc->przekierowanie != NULL)
                {
                    wynik = (char *) pomoc->przekierowanie;
                    j = i;
                }
                pomoc = pomoc->dzieci[char_na_inta(num[i])];
                ++i;
            }
            if (pomoc != NULL)
            {
                if (pomoc->przekierowanie != NULL)
                {
                    wynik = (char *) pomoc->przekierowanie;
                    j = i;
                }
            }
            char const *b = polacz_stringi(num, wynik, j);
            if (b == NULL)
            {
                return NULL;
            }
            PhoneNumbers *a = phonedNew();
            if (a)
            {
                if (addPhone(a, b))
                {
                    free((char *) b);
                    return a;
                }
            }
            free((char *) b);
            phnumDelete(a);
            return NULL;
        }
        else
        {
            PhoneNumbers *a = phonedNew();
            return a;
        }
    }
    return NULL;
}

/** @brief Sprawdza czy numery się zawierają.
 * Sprawdza czy @p num1 zawieriera @p num2 i naodwrót.
 * @param num1 - pierwszy sprawdzany numer.
 * @param num2 - drugi sprawdzany numer.
 * @return Wartość @p true jeżeli któryś z numerów jest prefiksem drugiego, w
 *         przeciwnym przypadku @p false.
 */
static bool czy_siezawieraja(char const *num1, char const *num2)
{
    if (num2 != NULL)
    {
        int c = 0;
        while (jest_cyfra(num1[c]) && jest_cyfra(num2[c]))
        {
            if (num1[c] != num2[c])
            {
                break;
            }
            ++c;
        }
        return (num1[c] == '\0' || num2[c] == '\0');
    }
    return false;
}

/** @brief Sprawdza czy numer jest prefiksem.
 * Sprawdza czy @p num1 jest prefiksem @p num2.
 * @param num1 - pierwszy sprawdzany numer.
 * @param num2 - drugi sprawdzany numer.
 * @return Wartość @p true jeżeli któryś z numerów jest prefiksem drugiego, w
 *         przeciwnym przypadku @p false.
 */
static bool czy_jest_prefiksem(char const *num1, char const *num2)
{
    if (num2 != NULL && num1 != NULL)
    {
        int c = 0;
        while (jest_cyfra(num1[c]) && jest_cyfra(num2[c]))
        {
            if (num1[c] != num2[c])
            {
                break;
            }
            ++c;
        }
        return (num1[c] == '\0');
    }
    return false;
}


/** @brief Porównuje dwa numery.
 * Porównuje dwa numery leksykograficznie.
 * @param num1 - pierwszy sprawdzany numer.
 * @param num2 - drugi sprawdzany numer.
 * @return Wartość większa od zera, jeżeli @p num1 jest po @p num2 w kolejności
 *         leksykograficznej, wartość mniejsza od zera jeżeli @p num1 jest przed
 *         @p num2 w kolejności leksykograficznej, jeżeli są równe 0.
 */
static int porownaj(char const *num1, char const *num2)
{
    if (num1 == NULL && num2 == NULL)
    {
        return 0;
    }
    else if (num1 == NULL)
    {
        return 1;
    }
    else if (num2 == NULL)
    {
        return -1;
    }
    size_t i = 0;
    while (jest_cyfra(num1[i]) && jest_cyfra(num2[i]))
    {
        if (num1[i] != num2[i])
        {
            break;
        }
        ++i;
    }
    return char_na_inta(num1[i]) - char_na_inta(num2[i]);
}

/**
 * Funkcja pomocnicza żeby móc wywołać qsort.
 */
static int porownaj_pomoc(const void *arg1, const void *arg2)
{
    char *const *num1 = arg1;
    char *const *num2 = arg2;
    return porownaj(*num1, *num2);
}

/** @brief Znajduje brakujący sufiks.
 * Znajduje brakujący sufiks, taki że po konkatenacji tego sufiksu z
 * @p przekierowanie nowy numer będzie równy @p num1, lub będzie go zawierał.
 * Zwraca NULL jeżeli @p num1 jest prefiksem @p przekierowanie.
 * @param num1          - numer do którego uzupełniamy.
 * @param przekierowanie - numer który uzupełniamy o brakujący sufiks.
 * @param blad          - zmienna informująca czy nastąpił bład przy alokacji
 *                        pamięci.
 * @return Wskaźnik na numery będący brakujący sufiksem numeru @p przekierowanie.
 */
static char const *brakujacy_sufiks(char const *num1,
                                    char const *przekierowanie,
                                    int *blad)
{
    if (przekierowanie != NULL)
    {
        size_t c = 0, i = 0;
        while (num1[c] != '\0' && przekierowanie[c] != '\0')
        {
            ++c;
        }
        if (num1[c] == '\0')
        {
            return NULL;
        }
        size_t wielkosc = dlugosc(num1);
        char *wynik = NULL;
        if (!mallokuj_tablice_char(&wynik, wielkosc))
        {
            *blad = 1;
            return NULL;
        }
        while (num1[c] != '\0')
        {
            wynik[i] = num1[c];
            ++c;
            ++i;
        }
        wynik[i] = '\0';
        wynik = realloc(wynik, (i + 1) * sizeof(*wynik));
        return wynik;
    }
    return NULL;
}

/** @brief Usuwa powtórzenia.
 * Usuwa powtarzające się numery ze struktury @p a
 * @param a - wskaźnik na strukturę z której usuwamy powtórzenia.
 */
static void usun_powtorzenia(PhoneNumbers *a)
{
    if (a->pierwszy_wolny_indeks >= 2)
    {
        size_t obecnie_sprawdzany = 1, poprzedni = 0, ile_usunietych = 0;
        while (obecnie_sprawdzany < a->pierwszy_wolny_indeks)
        {
            if (czy_takie_same_napisy(a->tablica_numerow[poprzedni],
                                      a->tablica_numerow[obecnie_sprawdzany]))
            {
                free((char *) a->tablica_numerow[obecnie_sprawdzany]);
                a->tablica_numerow[obecnie_sprawdzany] = NULL;
                ++obecnie_sprawdzany;
                ++ile_usunietych;
            }
            else
            {
                poprzedni = obecnie_sprawdzany;
                ++obecnie_sprawdzany;
            }
        }
        qsort(a->tablica_numerow, a->pierwszy_wolny_indeks,
              sizeof(const char *), porownaj_pomoc);

        a->pierwszy_wolny_indeks = a->pierwszy_wolny_indeks - ile_usunietych;
    }
}

PhoneNumbers *phfwdReverse(PhoneForward const *pf, char const *num)
{
    if (pf != NULL && czy_poprawny_numer(num))
    {
        PhoneForward const *pomoc = pf;
        int n = 0, blad = 0;
        PhoneNumbers *wynik = phonedNew();
        Tstring *obecny = inicjuj();
        if (!wynik || !obecny)
        {
            phnumDelete(wynik);
            usun(obecny);
            return NULL;
        }
        if (!addPhone(wynik, num))
        {
            usun(obecny);
            phnumDelete(wynik);
            return NULL;
        }
        while (pomoc != NULL)
        {
            for (; n < ILOSC_CYFR; ++n)
            {
                if (pomoc->dzieci[n] != NULL)
                {
                    pomoc = pomoc->dzieci[n];
                    if (!dodaj_element(obecny, int_na_chara(n)))
                    {
                        usun(obecny);
                        phnumDelete(wynik);
                        return NULL;
                    }
                    n = -1;
                }
            }
            if (czy_siezawieraja(num, pomoc->przekierowanie))
            {
                char const *potencjalny = NULL;
                char const *numer_wynikow = NULL;
                potencjalny = brakujacy_sufiks(num, pomoc->przekierowanie,
                                               &blad);
                if (blad)
                {
                    usun(obecny);
                    phnumDelete(wynik);
                    return NULL;
                }
                numer_wynikow = polacz_stringi(potencjalny, obecny->string, 0);
                if (numer_wynikow == NULL)
                {
                    free((char *) potencjalny);
                    usun(obecny);
                    phnumDelete(wynik);
                    return NULL;
                }
                if (!addPhone(wynik, numer_wynikow))
                {
                    usun(obecny);
                    phnumDelete(wynik);
                    free((char *) potencjalny);
                    free((char *) numer_wynikow);
                    return NULL;
                }
                free((char *) potencjalny);
                free((char *) numer_wynikow);
            }
            n = pomoc->ktory_syn + 1;
            pomoc = pomoc->Ojciec;
            usun_element(obecny);
        }

        qsort(wynik->tablica_numerow, wynik->pierwszy_wolny_indeks,
              sizeof(const char *), porownaj_pomoc);
        usun_powtorzenia(wynik);
        usun(obecny);
        return wynik;
    }
    if (pf != NULL)
    {
        PhoneNumbers *a = phonedNew();
        return a;
    }
    return NULL;
}

PhoneNumbers *phfwdGetReverse(PhoneForward const *pf, char const *num)
{
    if (pf != NULL && czy_poprawny_numer(num))
    {
        PhoneForward const *pomoc = pf;
        int n = 0, blad = 0;
        PhoneNumbers *wynik = phonedNew();
        Tstring *obecny = inicjuj();
        if (!wynik || !obecny)
        {
            phnumDelete(wynik);
            usun(obecny);
            return NULL;
        }
        PhoneNumbers *test = phfwdGet(pf, num);
        if (czy_takie_same_napisy(phnumGet(test, 0), num))
        {
            if (!addPhone(wynik, num))
            {
                phnumDelete(test);
                phnumDelete(wynik);
                usun(obecny);
                return NULL;
            }
        }
        phnumDelete(test);
        while (pomoc != NULL)
        {
            for (; n < ILOSC_CYFR; ++n)
            {
                if (pomoc->dzieci[n] != NULL)
                {
                    pomoc = pomoc->dzieci[n];
                    if (!dodaj_element(obecny, int_na_chara(n)))
                    {
                        usun(obecny);
                        phnumDelete(wynik);
                        return NULL;
                    }
                    n = -1;
                }
            }
            PhoneNumbers *chwilowy = phfwdGet(pf, obecny->string);
            if (czy_jest_prefiksem(phnumGet(chwilowy, 0), num))
            {
                char const *potencjalny = NULL;
                char const *numer_wynikow = NULL;
                potencjalny = brakujacy_sufiks(num, pomoc->przekierowanie,
                                               &blad);
                if (blad)
                {
                    usun(obecny);
                    phnumDelete(wynik);
                    phnumDelete(chwilowy);
                    return NULL;
                }

                numer_wynikow = polacz_stringi(potencjalny, obecny->string, 0);
                if (numer_wynikow == NULL)
                {
                    free((char *) potencjalny);
                    usun(obecny);
                    phnumDelete(wynik);
                    phnumDelete(chwilowy);
                    return NULL;
                }
                phnumDelete(chwilowy);
                chwilowy = phfwdGet(pf, numer_wynikow);
                if (czy_takie_same_napisy(phnumGet(chwilowy, 0), num))
                {
                    if (!addPhone(wynik, numer_wynikow))
                    {
                        usun(obecny);
                        phnumDelete(wynik);
                        free((char *) potencjalny);
                        free((char *) numer_wynikow);
                        phnumDelete(chwilowy);
                        return NULL;
                    }
                }
                free((char *) potencjalny);
                free((char *) numer_wynikow);
            }
            phnumDelete(chwilowy);
            n = pomoc->ktory_syn + 1;
            pomoc = pomoc->Ojciec;
            usun_element(obecny);
        }
        qsort(wynik->tablica_numerow, wynik->pierwszy_wolny_indeks,
              sizeof(const char *), porownaj_pomoc);
        usun_powtorzenia(wynik);
        usun(obecny);
        return wynik;
    }
    else if (pf)
    {
        return phonedNew();
    }
    return NULL;
}


