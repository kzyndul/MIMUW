CC       = gcc
CFLAGS   = -Wall -Wextra -Wno-implicit-fallthrough -O2 -std=c17

.PHONY: all clean

all: labirynt

labirynt: main.o przetwarzanie.o wczytywanie.o przydatne.o lista.o obsluga_bledow.o kolejka.o labirynt.o
labirynt.o: labirynt.h
obsluga_bledow.o: labirynt.h obsluga_bledow.h
lista.o: lista.h
kolejka.o: lista.h kolejka.h
przydatne.o: labirynt.h lista.h
wczytywanie.o: labirynt.h obsluga_bledow.h przydatne.h
przetwarzanie.o: labirynt.h przydatne.h kolejka.h
main.o: przetwarzanie.o wczytywanie.o przydatne.o lista.o obsluga_bledow.o kolejka.o labirynt.o

clean:
	rm -f *.o labirynt
