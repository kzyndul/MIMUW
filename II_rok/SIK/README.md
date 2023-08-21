### Sieci Komputerowe
<b> SIK MIMUW 2023 </b> <br />
<b> Autor: Krzysztof Żyndul </b> <br />
<b> Sprawdzający: Paweł Parys </b> <br />

<b> Oceny: (testy automatyczne, ocena słuchowa sprawdzającego)

    - Zadanie 1 - 4.17 / 5, 4.9 / 5

    - Zadanie 2 - 7 / 9, 7.4 / 9
</b>


---
"Zmienne" użyte w treści:
- MCAST_ADDR - adres rozgłaszania ukierunkowanego, ustawiany obowiązkowym parametrem -a nadajnika
- DISCOVER_ADDR - adres używany przez odbiornik do wykrywania aktywnych nadajników, ustawiany parametrem -d odbiornika, domyślnie 255.255.255.255
- DATA_PORT - port UDP używany do przesyłania danych, ustawiany parametrem -P nadajnika, domyślnie 20000 + (numer_albumu % 10000)
- CTRL_PORT - port UDP używany do transmisji pakietów kontrolnych, ustawiany parametrem -C nadajnika i odbiornika, domyślnie 30000 + (numer_albumu % 10000)
- UI_PORT - port TCP, na którym udostępniany jest prosty interfejs tekstowy do przełączania się między stacjami, domyślnie 10000 + (numer_albumu % 10000); ustawiany parametrem -U odbiornika
- PSIZE - rozmiar w bajtach pola audio_data paczki, ustawiany parametrem -p nadajnika, domyślnie 512B
- BSIZE - rozmiar w bajtach bufora, ustawiany parametrem -b odbiornika, domyślnie 64kB (65536B)
- FSIZE - rozmiar w bajtach kolejki FIFO nadajnika, ustawiany parametrem -f nadajnika, domyślnie 128kB
- RTIME - czas (w milisekundach) pomiędzy wysłaniem kolejnych raportów o brakujących paczkach (dla odbiorników) oraz czas pomiędzy kolejnymi retransmisjami paczek, ustawiany parametrem -R, domyślnie 250
- NAME - nazwa to nazwa nadajnika, ustawiana parametrem -n, domyślnie "Nienazwany Nadajnik"

**Cześć A (nadajnik)**
Nadajnik służy do wysyłania strumienia danych otrzymanego na standardowe wejście do odbiorników. Nadajnik powinien otrzymywać na standardowe wejście strumień danych z taką prędkością, z jaką odbiorcy są w stanie dane przetwarzać, a następnie wysyłać te dane zapakowane w datagramy UDP na port DATA_PORT na wskazany w linii poleceń adres ukierunkowanego rozgłaszania MCAST_ADDR.

Nadajnik powinien przechowywać w kolejce FIFO ostatnich FSIZE bajtów przeczytanych z wejścia tak, żeby mógł ponownie wysłać te paczki, o których retransmisję poprosiły odbiorniki.

Nadajnik cały czas zbiera od odbiorników prośby o retransmisje paczek. Gromadzi je przez czas RTIME, następnie wysyła serię retransmisji (podczas ich wysyłania nadal zbiera prośby, do wysłania w kolejnej serii), następnie znów gromadzi je przez czas RTIME, itd.

Nadajnik nasłuchuje na UDP na porcie CTRL_PORT, przyjmując także pakiety rozgłoszeniowe. Powinien rozpoznawać dwa rodzaje komunikatów:

LOOKUP (prośby o identyfikację): na takie natychmiast odpowiada komunikatem REPLY zgodnie ze specyfikacją protokołu poniżej.

REXMIT (prośby o retransmisję paczek): na takie nie odpowiada bezpośrednio; raz na jakiś czas ponownie wysyła paczki, według opisu powyżej.

**Uruchomienie nadajnika**
Na przykład, żeby wysłać ulubioną MP-trójkę w jakości płyty CD, można użyć takiego polecenia:

```
sox -S "05 Muzyczka.mp3" -r 44100 -b 16 -e signed-integer -c 2 -t raw - | pv -q -L \$((44100\*4)) | ./sikradio-sender -a 239.10.11.12 -n "Radio Muzyczka"
```

Pierwsza część polecenia konwertuje plik MP3 na strumień surowych danych (44100 4-bajtowych sampli na każdą sekundę pliku wejściowego), druga część ogranicza prędkość przekazywania danych do nadajnika tak, żeby odbiorniki wyrabiały się z pobieraniem danych.


**Część B (odbiornik)**

Odbiornik odbiera dane wysyłane przez nadajnik i wyprowadza je na standardowe wyjście.

Odbiornik co ok. 5s wysyła na adres DISCOVER_ADDR na port CTRL_PORT prośbę o identyfikację (komunikat LOOKUP). Na podstawie otrzymanych odpowiedzi (komunikatów REPLY) tworzy listę dostępnych stacji radiowych. Stacja, od której przez 20 sekund odbiornik nie otrzymał komunikatu REPLY, jest usuwana z listy. Jeśli to była stacja aktualnie odtwarzana, rozpoczyna się odtwarzanie innej stacji. Jeśli podano parametr -n, odbiornik rozpoczyna odtwarzanie stacji o zadanej nazwie, gdy tylko ją wykryje. Jeśli nie podano argumentu -n, odbiornik rozpoczyna odtwarzanie pierwszej wykrytej stacji.

Odbiornik posiada bufor o rozmiarze BSIZE bajtów, przeznaczony do przechowywania danych z maksymalnie ⌊BSIZE/PSIZE⌋ kolejnych paczek.

Rozpoczynając odtwarzanie, odbiornik:
- Czyści bufor, w szczególności porzucając dane w nim się znajdujące, a jeszcze nie wyprowadzone na standardowe wyjście.
- Jeśli potrzeba, wypisuje się z poprzedniego adresu grupowego, a zapisuje się na nowy.
- Po otrzymaniu pierwszej paczki audio, zapisuje z niej wartość pola session_id oraz numer pierwszego odebranego bajtu (nazwijmy go BYTE0; patrz specyfikacja protokołu poniżej), oraz rozpoczyna wysyłanie próśb o retransmisję zgodnie z opisem poniżej.
- Aż do momentu odebrania bajtu o numerze BYTE0 + ⌊BSIZE*3/4⌋ lub większym, odbiornik nie przekazuje danych na standardowe wyjście. Gdy jednak to nastąpi, przekazuje dane na standardowe wyjście tak szybko, jak tylko standardowe wyjście na to pozwala.

Powyższą procedurę należy zastosować wszędzie tam, gdzie w treści zadania mowa jest o rozpoczynaniu odtwarzania.

Jeśli odbiornik miałby wyprowadzić na standardowe wyjście dane, których jednakże brakuje w buforze, choćby to była tylko jedna paczka, rozpoczyna odtwarzanie od nowa. UWAGA: to wymaganie nie występowało w zadaniu 1.

Jeśli odbiornik odbierze nową paczkę, o numerze większym niż dotychczas odebrane, umieszcza ją w buforze i w razie potrzeby rezerwuje miejsce na brakujące paczki, których miejsce jest przed nią. Jeśli do wykonania tego potrzeba usunąć stare dane, które nie zostały jeszcze wyprowadzone na standardowe wyjście, należy to zrobić.

Odbiornik wysyła prośby o retransmisję brakujących paczek. Prośbę o retransmisję paczki o numerze n powinien wysłać w momentach t + k*RTIME, gdzie t oznacza moment odebrania pierwszej paczki o numerze większym niż n, dla k = 1, 2, … (w nieskończoność, póki dana stacja znajduje się na liście dostępnych stacji). Odbiornik nie wysyła próśb o retransmisję paczek zawierających bajty wcześniejsze niż BYTE0, ani tak starych, że i tak nie będzie na nie miejsca w buforze.

Odbiornik oczekuje połączeń TCP na porcie UI_PORT. Jeśli użytkownik podłączy się tam np. programem telnet, powinien zobaczyć prosty tekstowy interfejs, w którym za pomocą strzałek góra/dół można zmieniać stacje (bez konieczności wciskania Enter). Oczywiście, jeśli jest kilka połączeń, wszystkie powinny wyświetlać to samo i zmiany w jednym z nich powinny być widoczne w drugim. Podobnie, wyświetlana lista stacji powinna się aktualizować w przypadku wykrycia nowych stacji lub porzucenia już niedostępnych. Powinno to wyglądać dokładnie tak:

```
------------------------------------------------------------------------

 SIK Radio

------------------------------------------------------------------------

PR1

Radio "357"

 > Radio "Disco Pruszkow"

------------------------------------------------------------------------
```

Stacje powinny być posortowane alfabetycznie po nazwie. Przy każdorazowej zmianie stanu (aktywnej stacji, listy dostępnych stacji itp.) listę należy ponownie wyświetlić w całości; w ten sposób będzie można w sposób automatyczny przetestować działanie programów.

**Uruchomienie odbiorn

ika**
```
./sikradio-receiver | play -t raw -c 2 -r 44100 -b 16 -e signed-integer --buffer 32768 -
```
Polecenia "play" należy szukać w pakiecie z programem "sox".

**Protokół przesyłania danych audio**
Wymiana danych: wymiana danych odbywa się po UDP. Komunikacja jest jednostronna - nadajnik wysyła paczki audio, a odbiornik je odbiera.

Format datagramów: w datagramach przesyłane są dane binarne, zgodne z poniżej zdefiniowanym formatem komunikatów.

Porządek bajtów: w komunikatach wszystkie liczby przesyłane są w sieciowej kolejności bajtów (big-endian).

Paczka audio:
- uint64 session_id
- uint64 first_byte_num
- byte[] audio_data

Pole session_id jest stałe przez cały czas uruchomienia nadajnika. Na początku jego działania inicjowane jest datą wyrażoną w sekundach od początku epoki.

Odbiornik zaś zapamiętuje wartość session_id z pierwszej paczki, jaką otrzymał po rozpoczęciu odtwarzania. W przypadku odebrania paczki z:
- mniejszym session_id, ignoruje ją,
- z większym session_id, rozpoczyna odtwarzanie od nowa.

Bajty odczytywane przez nadajnik ze standardowego wejścia numerowane są od zera. Nadajnik w polu first_byte_num umieszcza numer pierwszego spośród bajtów zawartych w audio_data.

Nadajnik wysyła paczki, w których pole audio_data ma dokładnie PSIZE bajtów (a first_byte_num jest podzielne przez PSIZE).

**Protokół kontrolny**
Wymiana danych odbywa się po UDP.

W datagramach przesyłane są dane tekstowe, zgodne z formatem opisanym poniżej.

Każdy komunikat to pojedyncza linia tekstu zakończona uniksowym znakiem końca linii. Poza znakiem końca linii dopuszcza się jedynie znaki o numerach od 32 do 127 według kodowania ASCII.

Poszczególne pola komunikatów oddzielone są pojedynczymi spacjami. Ostatnie pole (np. nazwa stacji w REPLY) może zawierać spacje.

Komunikat LOOKUP wygląda następująco:
```
ZERO_SEVEN_COME_IN
```
Komunikat REPLY wygląda następująco:
```
BOREWICZ_HERE [MCAST_ADDR] [DATA_PORT] [nazwa stacji]
```
Maksymalna długość nazwy stacji wynosi 64 znaki.

Komunikat REXMIT wygląda następująco:
```
LOUDER_PLEASE [lista numerów paczek oddzielonych przecinkami]
```
gdzie numer paczki to wartość jej pola first_byte_num, np.
```
LOUDER_PLEASE 512,1024,1536,5632,3584
```

**FORMAT TEXT**

I hope this helps! If you need any further clarification or assistance, feel free to ask.