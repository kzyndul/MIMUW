# Programowanie współbieżne
<b> Rozwiązanie zadania 2 z PW MIMUW 2022 </b> <br />
<b> Autor: Krzysztof Żyndul </b>

<b> Ocena 10 / 10 </b>


**Program executor** powinien obsługiwać następujące polecenia:

- **Polecenie run**
  uruchamia program A z argumentami B C ... i wypisuje `Task T started: pid P.\n`.
  
- **Polecenie out**
  wypisuje `Task T stdout: 'S'.\n` – ostatnia linia stdout programu A.


- **Polecenie err**
  wypisuje `Task T stderr: 'S'.\n` – ostatnia linia stderr programu A.


- **Polecenie kill**
  wysyła sygnał SIGINT do programu A.
  
- **Polecenia pomocnicze**
  - **sleep N**: usypia egzekutora na N ms.
  - **quit**: kończy egzekutor (i wszystkie programy).
  - pusta linia – nic nie robi, przechodzi do kolejnego polecenia.

**Kończenie zadań**

Gdy program w zadaniu nr T się zakończy, egzekutor powinien o tym poinformować wypisując `Task T ended: status X.\n`, gdzie T to identyfikator zadania, zaś X to jego kod wyjścia. Jeśli kod wyjścia nie jest dostępny (bo zadanie zostało przerwane sygnałem lub przez system), należy zamiast tego wypisać `Task T ended: signalled.\n`.
