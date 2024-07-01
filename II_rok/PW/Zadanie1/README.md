# Programowanie współbieżne
<b>Rozwiązanie zadania 1 z PW MIMUW 2022</b> <br />
<b>Autor: Krzysztof Żyndul</b> <br />
<b>Ocena 7.5 / 10</b>

# Specyfikacja
W warsztacie znajdują się stanowiska robocze (cp2022.base.Workplace) z unikalnymi identyfikatorami (cp2022.base.WorkplaceId). Użytkownicy warsztatu to wątki Javy z unikalnymi identyfikatorami.

Warsztat (cp2022.base.Workshop) koordynuje dostęp do stanowisk:
```java
public interface Workshop {
    
    public Workplace enter(WorkplaceId wid);
    
    public Workplace switchTo(WorkplaceId wid);
    
    public void leave();
    
}
```

### Działanie pojedynczego użytkownika

1. Użytkownik zajmuje stanowisko (enter) i zaczyna pracę (use).

2. Może opuścić warsztat (leave) lub zmienić stanowisko (switchTo) i kontynuować pracę.

3. Może wielokrotnie zmieniać stanowiska.

### Koordynacja użytkowników

1. Stanowiska nie mogą być zajęte jednocześnie przez wielu użytkowników.

2. Należy unikać zagłodzenia użytkowników – użytkownik musi wejść lub zmienić stanowisko przed wejściem 2*N innych użytkowników, gdzie N to liczba stanowisk.

### Wymagania

Zadanie polega na zaimplementowaniu warsztatu zgodnie z powyższą specyfikacją, używając mechanizmów współbieżności języka Java 11, zgodnie z dobrymi praktykami. Rozwiązania oparte na aktywnym oczekiwaniu nie będą punktowane.
