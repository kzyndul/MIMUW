/** @file
 * Interfejs kalsy zajmującej się pamięcia i dynamiczny stringiem.
 *
 * @author Krzysztof Żyndul <kz438842@students.mimuw.edu.pl>
 * @copyright Uniwersytet Warszawski
 * @date 2022
 */

#ifndef STRING_H
#define STRING_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/**
 * To jest struktura przechowująca dynamiczne powiększany numer.
 */
struct przydatne
{
    char *string;           /**< Tablica przechwująca numer. */
    size_t wielkosc;        /**< Obecna wielkość tablicy. */
    size_t wolny_indeks;    /**< Pierwszy wolny indeks tablicy. */
};

/**
 * Deklaracja typu Tstring.
 */
typedef struct przydatne Tstring;

/** @brief Tworzy nowego Tstringa.
 * Tworzy nowy dynamiczny numer.
 * @return Wskaźnik na dynamiczne powiększany numer, lub NULL jeżeli nie udał
 *         się przydzielić pamięci.
 */
Tstring *inicjuj(void);

/** @brief Dodaje cyfre do numeru.
 * Dodaje @p b do numeru na którego początek wskazuje @p a.
 * @param a - wskaźnik na początek numeru.
 * @param b - cyfra, którą dodajemy na koniec numeru.
 * @return @p true jeżeli udało się dodać cyfrę, lub @p false w przeciwnym
 *         przypadku.
 */
bool dodaj_element(Tstring *a, char b);

/** @brief Usuwa ostatnią cyfrę.
 * Usuwa ostatnią cyfrę dynamicznego numeru wskazywanego przez @p a.
 * @param a wskaźnik na numer, z którego należy usunąć ostatnią cyfrę.
 */
void usun_element(Tstring *a);

/** @brief Usuwa strukturę.
 * Usuwa strukturę wskazywaną przez @p a. Nic nie robi, jeśli wskaźnik ten ma
 * wartość NULL.
 * @param a – wskaźnik na usuwaną strukturę.
 */
void usun(Tstring *a);

/**
 * @brief Mallokuje tablice char.
 * Mallokuje tablice char o rozmiarze @p rozmiar i zapisuje ją na @p a.
 * @param a       - wskaźnik na początek zaalokowanej tablicy typu char.
 * @param rozmiar - rozmiar tablicy, którą chcemy zaalokować.
 * @return Wartość @p 1 jeżeli allokacja się powiodła.
 *         Wartość @p 0 jeżeli nie udało się przydzielić pamięci.
 */
int mallokuj_tablice_char(char **a, size_t rozmiar);

/** @brief Powiększa tablicę numerów.
 * Powiększa dwuwymiarową tablicę numerów przekazaną jako wskaźnik przez @p a.
 * Powiększa ją 3/2 raza wstosunku do jej starej wielkości przekazanej jako
 * @p wielkosc.
 * @param wielkosc - wielkość starej tablicy.
 * @param a        - wskaźnik na powiększaną tablicę.
 */
bool powieksz_tablice_numerow(size_t *wielkosc, const char ***a);

#endif