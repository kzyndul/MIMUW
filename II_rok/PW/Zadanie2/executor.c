#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <stdatomic.h>

#include "utils.h"
#include "err.h"

#define IN_SIZE 512
#define LICZBA_ZADAN 4096
#define OUT_SIZE 1022

int numer_zadania = 0;


pthread_mutex_t mutex_pisze;
bool pisze = false;

pthread_cond_t zakonczylem_proces;
pthread_cond_t polecenie;
pthread_mutex_t mutex;

bool wykonuje_polecenie = true;
int zakonczone_procesy = 0;

struct Para {
    int numer;
    int deskryptor;
};


char wyjscie[LICZBA_ZADAN][OUT_SIZE];
pthread_mutex_t mutex_out[LICZBA_ZADAN];
bool robi_out[LICZBA_ZADAN];

char wyjscie_diagnostyczne[LICZBA_ZADAN][OUT_SIZE];
pthread_mutex_t mutex_err[LICZBA_ZADAN];
bool robi_err[LICZBA_ZADAN];

int test[LICZBA_ZADAN];
struct Para dane_out[LICZBA_ZADAN];
struct Para dane_err[LICZBA_ZADAN];


pthread_t threads[2 * LICZBA_ZADAN];
pthread_t zakonczenie[LICZBA_ZADAN];
pid_t procesy[LICZBA_ZADAN];

void polecenie_wejdz()
{
    ASSERT_ZERO(pthread_mutex_lock(&mutex));
    while (zakonczone_procesy)
    {
        ASSERT_ZERO(pthread_cond_wait(&polecenie, &mutex));
    }
    wykonuje_polecenie = true;
    ASSERT_ZERO(pthread_mutex_unlock(&mutex));

}

void polecenie_wyjdz()
{
    ASSERT_ZERO(pthread_mutex_lock(&mutex));
    wykonuje_polecenie = false;
    ASSERT_ZERO(pthread_cond_signal(&zakonczylem_proces));
    ASSERT_ZERO(pthread_mutex_unlock(&mutex));
}

void zakonczony_wejdz()
{
    ASSERT_ZERO(pthread_mutex_lock(&mutex));
    ++zakonczone_procesy;
    while (wykonuje_polecenie)
    {
        ASSERT_ZERO(pthread_cond_wait(&zakonczylem_proces, &mutex));
    }
    ASSERT_ZERO(pthread_mutex_unlock(&mutex));
}

void zakoncz_wyjdz()
{
    ASSERT_ZERO(pthread_mutex_lock(&mutex));
    --zakonczone_procesy;
    if (!zakonczone_procesy)
    {
        ASSERT_ZERO(pthread_cond_signal(&polecenie));
    }
    else
    {
        ASSERT_ZERO(pthread_cond_signal(&zakonczylem_proces));
    }
    ASSERT_ZERO(pthread_mutex_unlock(&mutex));
}



void wypisz_wejdz()
{
    ASSERT_ZERO(pthread_mutex_lock(&mutex_pisze));
    while (pisze)
    {
        ASSERT_ZERO(pthread_mutex_lock(&mutex_pisze));
    }
    pisze = true;
}

void wypisz_wyjdz()
{
    pisze = false;
    ASSERT_ZERO(pthread_mutex_unlock(&mutex_pisze));
}

void wejdz_out(int numer)
{
    ASSERT_ZERO(pthread_mutex_lock(&mutex_out[numer]));
    while (robi_out[numer])
    {
        ASSERT_ZERO(pthread_mutex_lock(&mutex_out[numer]));
    }
    robi_out[numer] = true;
}

void wyjdz_out(int numer)
{
    robi_out[numer] = false;
    ASSERT_ZERO(pthread_mutex_unlock(&mutex_out[numer]));
}

void wejdz_err(int numer)
{
    ASSERT_ZERO(pthread_mutex_lock(&mutex_err[numer]));
    while (robi_err[numer])
    {
        ASSERT_ZERO(pthread_mutex_lock(&mutex_err[numer]));
    }
    robi_err[numer] = true;
}

void wyjdz_err(int numer)
{
    robi_err[numer] = false;
    ASSERT_ZERO(pthread_mutex_unlock(&mutex_err[numer]));
}

int zamien(const char *liczba)
{
    int i = 0;
    int wynik = 0;
    while (liczba[i] != '\000')
    {
        wynik = wynik * 10;
        wynik += liczba[i] - '0';
        ++i;
    }
    return wynik;
}

void kopiuj_out(const char *obecny, int i, int numer)
{
    memcpy(wyjscie[numer], obecny, i);
}

void kopiuj_err(const char *obecny, int i, int numer)
{
    memcpy(wyjscie_diagnostyczne[numer], obecny, i);
}

void *zapisuj_out(void *data)
{
    struct Para *argumenty = (struct Para *) data;
    char c;
    int i = 0;
    char obecny[OUT_SIZE];
    while (read(argumenty->deskryptor, &c, 1))
    {
        if (c == '\n')
        {
            obecny[i++] = '\000';

            wejdz_out(argumenty->numer);
            kopiuj_out(obecny, i, argumenty->numer);
            wyjdz_out(argumenty->numer);

            i = 0;
        }
        else
        {
            obecny[i++] = c;
        }
    }
    ASSERT_SYS_OK(close(argumenty->deskryptor));
    return 0;
}


void *zapisuj_err(void *data)
{
    struct Para *argumenty = (struct Para *) data;
    char c;
    int i = 0;
    char obecny[OUT_SIZE];
    while (read(argumenty->deskryptor, &c, 1))
    {
        if (c == '\n')
        {
            obecny[i++] = '\000';

            wejdz_err(argumenty->numer);
            kopiuj_err(obecny, i, argumenty->numer);
            wyjdz_err(argumenty->numer);

            i = 0;
        }
        else
        {
            obecny[i++] = c;
        }
    }
    ASSERT_SYS_OK(close(argumenty->deskryptor));
    return 0;
}

void *na_zakonczenie(void *dane)
{
    int *numer = dane;
    pid_t pid = procesy[*numer];
    int status;
    waitpid(pid, &status, 0);
    zakonczony_wejdz();
    if (WIFSIGNALED(status))
    {
        wypisz_wejdz();
        printf("Task %d ended: signalled.\n", *numer);
        wypisz_wyjdz();
    }
    else if (WIFEXITED(status))
    {
        wypisz_wejdz();
        printf("Task %d ended: status %d.\n", *numer, WEXITSTATUS(status));
        wypisz_wyjdz();
    }
    zakoncz_wyjdz();
    return 0;
}

void run(char **input)
{
    int pipe_dsc_out[2];
    int pipe_dsc_err[2];
    ASSERT_SYS_OK(pipe(pipe_dsc_out));
    ASSERT_SYS_OK(pipe(pipe_dsc_err));

    set_close_on_exec(pipe_dsc_out[0], true);
    set_close_on_exec(pipe_dsc_err[0], true);

    pid_t pid = fork();
    ASSERT_SYS_OK(pid);
    if (!pid)
    {
        char *program_name = input[1];
        char **program_args = &input[1];
        wejdz_out(numer_zadania);
        wyjdz_out(numer_zadania);
        ASSERT_SYS_OK(dup2(pipe_dsc_out[1], STDOUT_FILENO));
        ASSERT_SYS_OK(dup2(pipe_dsc_err[1], STDERR_FILENO));

        ASSERT_SYS_OK(close(pipe_dsc_out[0]));
        ASSERT_SYS_OK(close(pipe_dsc_err[0]));
        ASSERT_SYS_OK(close(pipe_dsc_out[1]));
        ASSERT_SYS_OK(close(pipe_dsc_err[1]));

        ASSERT_SYS_OK(execvp(program_name, program_args));
    }
    else
    {
        procesy[numer_zadania] = pid;
        wypisz_wejdz();
        printf("Task %d started: pid %d.\n", numer_zadania, pid);
        wypisz_wyjdz();


        struct Para dane_out_temp;
        dane_out_temp.numer = numer_zadania;
        dane_out_temp.deskryptor = pipe_dsc_out[0];
        dane_out[numer_zadania] = dane_out_temp;

        ASSERT_SYS_OK(close(pipe_dsc_out[1]));
        ASSERT_ZERO(pthread_create(&threads[2 * numer_zadania], NULL, zapisuj_out, &dane_out[numer_zadania]));


        struct Para dane_err_temp;
        dane_err_temp.numer = numer_zadania;
        dane_err_temp.deskryptor = pipe_dsc_err[0];
        dane_err[numer_zadania] = dane_err_temp;

        ASSERT_SYS_OK(close(pipe_dsc_err[1]));
        ASSERT_ZERO(pthread_create(&threads[2 * numer_zadania + 1], NULL, zapisuj_err, &dane_err[numer_zadania]));


        ASSERT_ZERO(pthread_create(&zakonczenie[numer_zadania], NULL, na_zakonczenie, &test[numer_zadania]));

        ++numer_zadania;
    }
}


void out(char **input)
{
    int zadanie = zamien(input[1]);

    wejdz_out(zadanie);
    wypisz_wejdz();
    printf("Task %d stdout: '%s'.\n", zadanie, wyjscie[zadanie]);
    wypisz_wyjdz();
    wyjdz_out(zadanie);
}


void err(char **input)
{
    int zadanie = zamien(input[1]);
    wejdz_err(zadanie);
    printf("Task %d stderr: '%s'.\n", zadanie, wyjscie_diagnostyczne[zadanie]);
    wyjdz_err(zadanie);
}


void zabij(char **input)
{
    int zadanie = zamien(input[1]);
    kill(procesy[zadanie], SIGINT);
}


void czekaj(char **input)
{
    int ile = zamien(input[1]);
    ASSERT_SYS_OK(usleep(ile * 1000));
}

void quit(void)
{
    for (int i = 0; i < numer_zadania; ++i)
    {
        kill(procesy[i], SIGKILL);
    }

    for (int i = 0; i < 2 * numer_zadania; ++i)
    {
        ASSERT_ZERO(pthread_join(threads[i], NULL));
    }

    for (int i = 0; i < numer_zadania; ++i)
    {
        ASSERT_ZERO(pthread_join(zakonczenie[i], NULL));
    }

    ASSERT_ZERO(pthread_mutex_destroy(&mutex_pisze));
    ASSERT_ZERO(pthread_mutex_destroy(&mutex));
    ASSERT_ZERO(pthread_cond_destroy(&polecenie));
    ASSERT_ZERO(pthread_cond_destroy(&zakonczylem_proces));
    for (int i = 0; i < LICZBA_ZADAN; ++i)
    {
        ASSERT_ZERO(pthread_mutex_destroy(&mutex_out[i]));
        ASSERT_ZERO(pthread_mutex_destroy(&mutex_err[i]));
    }
    exit(0);
}

int main(void)
{
setbuf(stdout, NULL);
    ASSERT_ZERO(pthread_mutex_init(&mutex_pisze, NULL));
    ASSERT_ZERO(pthread_mutex_init(&mutex, NULL));
    ASSERT_ZERO(pthread_cond_init(&polecenie, NULL));
    ASSERT_ZERO(pthread_cond_init(&zakonczylem_proces, NULL));


    for (int i = 0; i < LICZBA_ZADAN; ++i)
    {
        wyjscie_diagnostyczne[i][0] = '\0';
        wyjscie[i][0] = '\0';
        ASSERT_ZERO(pthread_mutex_init(&mutex_out[i], NULL));
        ASSERT_ZERO(pthread_mutex_init(&mutex_err[i], NULL));
        test[i] = i;
        robi_err[i] = false;    
        robi_out[i] = false;
    }

    char linia[IN_SIZE];
    char **linia_podzielona;
    while (read_line(linia, IN_SIZE, stdin))
    {
        linia_podzielona = split_string(linia);
        switch (linia_podzielona[0][0])
        {
            case 'r':
                polecenie_wejdz();
                run(linia_podzielona);
                polecenie_wyjdz();
                break;
            case 'o':
                polecenie_wejdz();

                out(linia_podzielona);
                polecenie_wyjdz();

                break;
            case 'e':
                polecenie_wejdz();

                err(linia_podzielona);
                polecenie_wyjdz();

                break;
            case 'k':
                polecenie_wejdz();

                zabij(linia_podzielona);
                polecenie_wyjdz();

                break;
            case 's':
                polecenie_wejdz();

                czekaj(linia_podzielona);
                polecenie_wyjdz();

                break;
            case 'q':
                free_split_string(linia_podzielona);
                quit();
            default:
                break;
        }
        free_split_string(linia_podzielona);
    }
    quit();
    return 0;
}
