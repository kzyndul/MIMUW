# Indywidualny projekt programistyczny
<b> Rozwiązanie małego zadania z IPP MIMUW 2022 </b> <br />
<b> Autor: Krzysztof Żyndul </b>

<b> Ocena 10 / 20
</b>

Program znajduje drogę w wielowymiarowym labiryncie, labiryntu to niepusty przedział k-wymiarowej przestrzeni z k-wymiarowymi kostkami jednostkowymi,
gdzie każda kostka może być pusta lub wypełniona. Droga w labiryncie to ciąg przejść między pustymi kostkami, a jej długość to liczba przejść.
Pozycje początkową i końcową definiuje się przez podanie współrzędnych dwóch pustych kostek.

Program oczekuje na dane wejściowe, czyli cztery linie tekstu. Pierwsza linia zawiera k liczby całkowite określające rozmiary labiryntu w
poszczególnych wymiarach. Druga i trzecia linia to współrzędne pozycji początkowej i końcowej. Czwarta linia zawiera liczbę całkowitą opisującą
położenia ścian labiryntu.

Liczba opisująca położenie ścian labiryntu może być podana w formie liczby szesnastkowej lub w formie ciągu pięciu liczb całkowitych z przedziału
od 0 do UINT32_MAX: a, b, m, r, s0. Liczby s1, s2, s3, …, sr wyliczane są ze wzoru si=(asi−1+b)modm, a następnie wyliczane są reszty wi=simodn1n2…nk.
Liczba opisująca położenie ścian labiryntu ma ustawiony bit numer j, jeśli istnieje taki indeks i, że j mod 2^32 = wi.

Po otrzymaniu danych wejściowych, program oblicza długość najkrótszej drogi od pozycji początkowej do pozycji końcowej i wypisuje wynik na
standardowe wyjście.
