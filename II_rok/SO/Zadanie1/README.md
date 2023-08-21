### Systemy operacyjne

<b> Rozwiązanie pierwszego zadania z SO MIMUW 2023 </b> <br />
<b> Autor: Krzysztof Żyndul </b>


Odwracanie permutacji
Zaimplementuj w asemblerze wołaną z języka C funkcję: <code >bool inverse_permutation(size_t n, int *p); </code> <br>
Argumentami funkcji są wskaźnik p na niepustą tablicę liczb całkowitych oraz rozmiar tej tablicy n. Jeśli tablica wskazywana przez p zawiera permutację
liczb z przedziału od 0 do n-1, to funkcja odwraca tę permutację w miejscu, a wynikiem funkcji jest true. W przeciwnym przypadku wynikiem funkcji
jest false, a zawartość tablicy wskazywanej przez p po zakończeniu wykonywania funkcji jest taka sama jak w momencie jej wywołania. Funkcja powinna
wykrywać ewidentnie niepoprawne wartości n – patrz przykład użycia. Wolno natomiast założyć, że wskaźnik p jest poprawny.
