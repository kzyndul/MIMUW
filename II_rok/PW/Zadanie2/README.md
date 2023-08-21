# Programowanie współbieżne
<b> Rozwiązanie zadania 2 z PW MIMUW 2022 </b> <br />
<b> Autor: Krzysztof Żyndul </b>

<b> Ocena 10 / 10 </b>


Sara rozwija pewien algorytm i pisze programy, które wykonują się długo, wypisując co jakiś czas informacje o postępach. Sara chciałaby testować swoje programy, wykonując je na różnych przykładach i sprawdzając co wypisują.

Przykładów do sprawdzenia i wypisywanych informacji jest jednak bardzo dużo. Przygotuj dla Sary program executor, który w tym pomoże, wykonując wskazane zadania, sprawdzając ich ostatnie słowa (w trakcie działania programu) i ubijając je.

**Egzekutor (program executor)** powinien obsługiwać następujące polecenia (każde polecenie to jedna linia wejścia):

- **Polecenie run**
  Polecenie run A B C ... tworzy nowe zadanie, rozpoczynając wykonanie programu A z argumentami B C ... w tle. Zadania identyfikowane są liczbami 0, 1, 2, ..., w kolejności rozpoczęcia (tzn. poleceń run). Egzekutor wypisuje `Task T started: pid P.\n`, gdzie T to identyfikator zadania, zaś P to pid procesu wykonującego program A. Cokolwiek program A wypisze na standardowe wyjście i wyjście błędów nie powinno się pokazywać. Program A nigdy nie będzie oczekiwać standardowego wejścia (można je zostawić bez zmian).

- **Polecenie out**
  Polecenie out T wypisuje `Task T stdout: 'S'.\n`, gdzie S to ostatnia linia (bez znaku końca linii) dotychczas wypisana przez program A z zadania nr T. W rozwiązaniu na mniej punktów, egzekutor może zaczekać, aż program A wypisze kolejną linię (lub EOF) i użyć jej jako S. W rozwiązaniu na pełną liczbę punktów, egzekutor powinien niezwłocznie wypisać ostatnią linię (o ile program A przez dłuższy czas nic więcej nie wypisze; jeśli program A w krótkim czasie wypisze kolejne linie, można użyć też dowolnej z nich). Program A powinien kontynuować działanie.

- **Polecenie err**
  Polecenie err T wypisuje `Task T stderr: 'S'.\n`, analogicznie jak out T, ale dla standardowego wyjścia błędów programu A. Wypisuje to na zwykłe standardowe wyjście egzekutora (tak jak wszystkie polecenia egzekutora).

- **Polecenie kill**
  Polecenie kill T wysyła sygnał SIGINT do programu A z zadania nr T. Jeśli program już się zakończył, można wysłać sygnał (nieskutecznie, ignorując błąd) lub nie robić nic.

- **Polecenia pomocnicze**
  - sleep N – po prostu usypia egzekutora na N milisekund (używając np. usleep, który używa mikrosekund), gdzie N to liczba całkowita. Wstrzymuje na ten czas przetwarzanie kolejnych poleceń; nie wstrzymując żadnych zadań.
  - quit lub koniec wejścia (EOF) – kończy egzekutor (i wszystkie programy).
  - pusta linia – nic nie robi, przechodzi do kolejnego polecenia.

**Kończenie zadań**

Gdy program w zadaniu nr T się zakończy, egzekutor powinien o tym poinformować wypisując `Task T ended: status X.\n`, gdzie T to identyfikator zadania, zaś X to jego kod wyjścia. Jeśli kod wyjścia nie jest dostępny (bo zadanie zostało przerwane sygnałem lub przez system), należy zamiast tego wypisać `Task T ended: signalled.\n`.

Jeśli program zakończy się w trakcie obsługi jakiegoś polecenia przez egzekutora, informacja powinna się wypisać dopiero po zakończeniu tej obsługi. W rozwiązaniu na mniej punktów, można poczekać z wypisaniem informacji aż do najbliższego polecenia (i jego zakończenia). W rozwiązaniu na pełną liczbę punktów, informacja powinna wypisać się niezwłocznie po zakończeniu programu, czyli podczas czekania egzekutora na kolejne polecenia (o ile nie trwa właśnie obsługa polecenia).

Kiedy egzekutor poinformuje o zakończeniu ostatniego (z dotychczas rozpoczętych) zadań, nie powinien istnieć żaden proces ani wątek stworzony przez egzekutora, poza samym głównym wątkiem egzekutora i ewentualnie jednym pomocniczym wątkiem/procesem.

**Przykład**
Wykonanie w bashu

```
echo -e "foo\nbar" > in.txt;
echo -e "run cat in.txt\nsleep 100\nout 0" | ./executor
```

powinno wypisać na przykład:

```
Task 0 started: pid 1234.
Task 0 ended: status 0.
Task 0 stdout: 'bar'.
```

**Uwagi**
- Zużycie pamięci egzekutora powinno być niezależne, O(1), od długości wyjścia i czasu działania wykonywanych programów.
- Egzekutor nie może tworzyć zwykłych plików.
- Egzekutor nie powinien zaczynać przyjmować kolejnego polecenia na wejściu do czasu zakończenia obsługi poprzedniego.
- Zakazane jest użycie funkcji poll() i wariantów (ppoll, select, pselect, epoll).
- Poza funkcjami omawianymi na zajęciach, można użyć też innych