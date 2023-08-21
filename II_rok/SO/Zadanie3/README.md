### Systemy operacyjne

<b> Rozwiązanie trzeciego zadania z SO MIMUW 2023 </b> <br />
<b> Autor: Krzysztof Żyndul </b>


**System płatności w MINIX-ie**

Twoim zadaniem jest dodanie nowego wywołania systemowego PM_TRANSFER_MONEY oraz funkcji bibliotecznej `int transfermoney(pid_t recipient, int amount)` w systemie MINIX. Poniżej znajduje się opis, jak te elementy powinny działać:

1. **Wywołanie systemowe PM_TRANSFER_MONEY**

Dodaj nowe wywołanie systemowe `PM_TRANSFER_MONEY` do serwera PM w MINIX. To wywołanie umożliwi przelewanie pieniędzy między procesami. Musisz również zdefiniować własny typ komunikatu, który będzie używany do przekazywania informacji o przelewie między procesami.

2. **Funkcja biblioteczna `int transfermoney(pid_t recipient, int amount)`**

Dodaj funkcję `int transfermoney(pid_t recipient, int amount)` do pliku nagłówkowego `unistd.h`. Ta funkcja powinna realizować przelew pieniędzy między dwoma procesami. Jej działanie powinno być oparte na nowym wywołaniu systemowym `PM_TRANSFER_MONEY`.

Funkcja `transfermoney` powinna działać w następujący sposób:

- Jeśli przelew jest możliwy (warunki opisane w treści zadania są spełnione), funkcja przeprowadza przelew między procesami.
- Jeśli przelew się udaje, funkcja zwraca stan konta procesu, który ją wywołał, po wykonaniu tego przelewu.
- Jeśli przelew się nie udaje (ze względu na warunki opisane w treści zadania), funkcja zwraca -1 i ustawia `errno` na odpowiedni kod błędu.

Kody błędów `errno` i ich przypisane sytuacje:

- `ESRCH` - recipient nie jest identyfikatorem aktualnie działającego procesu.
- `EPERM` - recipient jest identyfikatorem procesu będącego potomkiem lub przodkiem procesu wywołującego funkcję `transfermoney`.
- `EINVAL` - wartość `amount` jest ujemna, proces wywołujący funkcję ma na koncie mniej niż `amount` jednostek waluty, lub proces o identyfikatorze `recipient` ma więcej niż `MAX_BALANCE - amount` jednostek waluty.

Stałe `INIT_BALANCE` i `MAX_BALANCE` powinny być zdefiniowane w pliku `minix/config.h` zgodnie z treścią zadania.

---
