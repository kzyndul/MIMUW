# Programowanie współbieżne
<b> Rozwiązanie zadania 1 z PW MIMUW 2022 </b> <br />
<b> Autor: Krzysztof Żyndul </b>

<b> Ocena 7.5 / 10 </b>

# Zadanie zaliczeniowe ze współbieżności w języku Java

Gospodarka współdzielenia (ang. sharing economy) to trend społeczno-ekonomiczny opierający się na dzieleniu się potencjalnie kosztownymi zasobami z innymi uczestnikami rynku. Pozwala to zamortyzować koszty utrzymania takich zasobów. Przykładem trendu są warsztaty wyposażone w wysokiej jakości specjalistyczne stanowiska. Te stanowiska mogą być poza finansowym zasięgiem przeciętnego użytkownika. Jednakże, gdy pojedynczy użytkownik nie potrzebuje tych stanowisk na co dzień, ale tylko od czasu do czasu, współdzielenie ich staje się ekonomicznie uzasadnione. Przykładowo, warsztat, którego użytkownikami jest wielu osób o podobnych potrzebach, może skorzystać na współdzieleniu takich zasobów.

Twoim zadaniem będzie rozwiązanie problemu współbieżnej koordynacji dostępu użytkowników do stanowisk w takim warsztacie, zgodnie z poniższymi wymaganiami. Implementację rozwiązania należy oprzeć na dostarczonym szablonie.

### Specyfikacja

W naszym modelu pojedynczy warsztat składa się z kolekcji stanowisk roboczych (obiektów klas dziedziczących po klasie `cp2022.base.Workplace` z załączonego szablonu). Każde stanowisko ma unikalny identyfikator w ramach warsztatu (obiekt klasy dziedziczącej po klasie `cp2022.base.WorkplaceId`, dostępny poprzez metodę `getId` klasy `cp2022.base.Workplace`).

Każde stanowisko może być używane przez dowolnego użytkownika (metoda `use` klasy `cp2022.base.Workplace`).

Każdy użytkownik warsztatu to wątek Javy. Posiada on unikalny identyfikator (metoda `getId` klasy `Thread`). Zbiór osób chcących korzystać z warsztatu może być dowolnie duży i dynamicznie się zmieniać.

Warsztat (obiekt implementujący interfejs `cp2022.base.Workshop`) jest odpowiedzialny za koordynację dostępu użytkowników do stanowisk.

```java
public interface Workshop {
    
    public Workplace enter(WorkplaceId wid);
    
    public Workplace switchTo(WorkplaceId wid);
    
    public void leave();
    
}
```

W systemie może działać wiele niezależnych warsztatów. Zbiory stanowisk tworzących warsztaty są parami rozłączne. Użytkownik może korzystać z różnych warsztatów w ciągu swojego życia, ale w danym momencie tylko z jednego.

### Działanie pojedynczego użytkownika

1. Użytkownik wchodząc do warsztatu zajmuje konkretne stanowisko (metoda `enter` interfejsu `Workshop`) i rozpoczyna przy nim pracę (wywołując `use` na obiekcie stanowiska).

2. Użytkownik może wykonać jedną z dwóch czynności: opuścić warsztat (metoda `leave` interfejsu `Workshop`) lub zmienić stanowisko pracy (metoda `switchTo` interfejsu `Workshop`) i rozpocząć przy nim pracę (wywołując `use` na obiekcie stanowiska).

3. Użytkownik może wielokrotnie zmieniać stanowiska pracy i wracać do tego samego stanowiska przed opuszczeniem warsztatu.

### Koordynacja użytkowników

Warsztat koordynuje dostęp użytkowników do stanowisk zgodnie z regułami:

1. Użytkownik nie może pracować na zajętym stanowisku.

2. Dążymy do jak najlepszego wykorzystania stanowisk, unikając jednak zagłodzenia użytkowników. Rozwiązanie powinno zagwarantować, że użytkownik chcący wejść do warsztatu lub zmienić stanowisko zrobi to przed wejściem 2*N innych użytkowników, którzy zaczęli chcieć wejść lub zmienić stanowisko po nim (gdzie N to liczba stanowisk).

### Wymagania

Zadanie polega na zaimplementowaniu warsztatu zgodnie z powyższą specyfikacją przy wykorzystaniu mechanizmów współbieżności języka Java 11. Kod źródłowy powinien być napisany zgodnie z dobrymi praktykami programistycznymi. Rozwiązania oparte na aktywnym lub półaktywnym oczekiwaniu (np. sleep, yield) nie będą punktowane.
