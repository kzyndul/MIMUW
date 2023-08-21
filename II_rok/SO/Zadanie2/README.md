### Systemy operacyjne

<b> Rozwiązanie drugiego zadania z SO MIMUW 2023 </b> <br />
<b> Autor: Krzysztof Żyndul </b>



---

**Symulator Rozproszonej Maszyny Stosowej**

Zaimplementuj w asemblerze x86_64 symulator rozproszonej maszyny stosowej. Maszyna składa się z N rdzeni, które są numerowane od 0 do N - 1, gdzie N jest pewną stałą ustalaną podczas kompilowania symulatora. Symulator będzie używany w języku C w taki sposób: N wątków będzie uruchamianych, a każdy wątek będzie wywoływał funkcję:

```c
uint64_t core(uint64_t n, char const *p);
```

Parametr `n` zawiera numer rdzenia, a parametr `p` jest wskaźnikiem na napis ASCIIZ, który definiuje obliczenie do wykonania przez rdzeń. Obliczenie składa się z operacji wykonywanych na stosie, który na początku jest pusty. Interpretacja znaków napisu jest następująca:

- '+' - Zdejmij dwie wartości ze stosu, oblicz ich sumę i wstaw wynik na stos.
- '*' - Zdejmij dwie wartości ze stosu, oblicz ich iloczyn i wstaw wynik na stos.
- '-' - Zaneguj arytmetycznie wartość na szczycie stosu.
- '0' do '9' - Wstaw na stos odpowiednie wartości (od 0 do 9).
- 'n' - Wstaw na stos numer rdzenia.
- 'B' - Zdejmij wartość ze stosu. Jeśli wartość na szczycie stosu jest różna od zera, traktuj zdjętą wartość jako liczbę w kodzie uzupełnieniowym do dwójki i przesuń się o tyle operacji.
- 'C' - Zdejmij wartość ze stosu i ją odrzuć.
- 'D' - Zduplikuj wartość na szczycie stosu.
- 'E' - Zamień miejscami dwie wartości na szczycie stosu.
- 'G' - Wstaw na stos wartość uzyskaną z wywołania funkcji `uint64_t get_value(uint64_t n)` zaimplementowanej gdzie indziej w C.
- 'P' - Zdejmij wartość ze stosu (oznaczmy ją jako 'w'). Wywołaj funkcję `void put_value(uint64_t n, uint64_t w)` zaimplementowaną gdzie indziej w C.
- 'S' - Synchronizuj rdzenie, zdejmij wartość ze stosu, potraktuj ją jako numer rdzenia 'm'. Poczekaj na operację 'S' rdzenia 'm' ze zdjętym ze stosu numerem rdzenia 'n' i zamień wartości na szczycie stosów rdzenia 'm' i 'n'.

Po zakończeniu obliczenia przez rdzeń, wynikiem obliczenia jest wartość na szczycie jego stosu. Wszystkie operacje są wykonywane na liczbach 64-bitowych modulo 2 do potęgi 64.

Zakładamy, że podany numer rdzenia jest prawidłowy. Zakładamy, że obliczenie jest poprawne, co oznacza, że zawiera tylko opisane znaki, kończy się bajtem o wartości zero, nie próbuje pobierać wartości z pustego stosu i nie prowadzi do zakleszczenia. Zachowanie rdzenia dla niepoprawnego obliczenia jest niezdefiniowane.

Wynik obliczenia "1nS" jest niezdefiniowany i zależy od implementacji, choć sensowne wydaje się potraktowanie sekwencji operacji 'n' i 'S' jako operacji 'C'.

Jako stosu, który rdzeń używa do opisanych obliczeń, użyj sprzętowego stosu procesora. Nie jest dozwolone korzystanie z żadnych bibliotek. Synchronizację rdzeni, czyli operację 'S', należy zaimplementować za pomocą pewnej formy blokady wirującej.

Nie zakładamy żadnego ograniczenia górnego na wartość N, inne niż to, które wynika z architektury procesora i dostępnej pamięci.

W specyfikacji operacji wykonywanych przez rdzeń terminy "zdjęcie ze stosu" i "wstawienie na stos" należy rozumieć jako definicję operacji do wykonania, a nie jako wymaganie użycia instrukcji pop lub push.

---
