# Język Oolong

## Opis jęzka

### Program:

Program jest listą deklaracji funkcji. Interpreter zaczyna działanie od
wykonania funkcji `main`.



### Funkcje

```
Type Ident "(" [Arg] ")" Block
``` 
Funkcje mają nazwę, listę argumentów oddzielonych przecinkami i typ zwracany.
Funkcje mogą przyjmować i wyliczać się do następujących typó `int`, `bool`,
`string`, `List<T>`. Argumenty fukcji mogą być poprzedzone modyfikatorem `ref`
oznaczjącym, że argument przekazywany jest przez referencję. Podczas deklaracji
funkcji decydujemy, które argumenty będą przekazywane przez wartość, a które
przez referencje. Funkcja można zagnieżdżać, tzn. można deklarować funkcje w
funkcji. Funkcje zagnieżdżone nie są widoczne poza funkcją zewnętrzną.



### Wołanie funkcji

```
Ident "(" [Arg] ")"
```
Wywołujemy funkcję podając jej identyfikator i w nawiasie listę argumentów.
Jeśli argument w definicji funkcji poprzedzony jest modyfikatorem `ref`, to 
podczas wywołania funkcji należy przed tym argumentem dodać modyfikator `ref`
(nie może to być wyrażenie, musi to być identyfikator). 



### Typy

Dostępne są trzy typy proste: `int`, `bool` oraz `string`. Stałe liczbowe
to po prostu `\-?[0-9]+`, stałe typu `bool` to `true` i `false`. Literały
napisowe zapisujemy w cudzysłowach. Nie ma procedur, są tylko funkcjie.
Listy są typu `List<T>`, gdzie T jest dowolnym innym typem. Typowanie jest
statyczne. Zmienne można deklarować w dowolnym miejscu programu. Mogą być
zainicjalizowane lub nie.



### Listy

```
List<T> lista;
lista.push [Expr];
lista.pop;
lista[Expr];
lista.length ;
```
Listy mogą przechowywać dowolny typ (także inne listy). Dostępne są cztery
funkcje wbudowane: `push`, `[Expr]`, `length`, `pop`. `Push` wkłada element na
początek listy, `pop` usuwa pierwszy element listy. `[Expr]` zwraca element pod
danym indeksem (gdy indeks jest poza zakresem to zgłaszany jest błąd
wykonania),`length` zwraca długość listy.



### Arytmetyka

```
Operatory arytmetyczne: +, -, *, /, minus unarny - tylko na int'ach
Operatory porównania: <, <=, >, >=               - tylko na int'ach
Operatory porównania: ==, !=                     - dowolny typ
Operatory logiczne: &&, ||, !                    - tylko na bool'ach
```



### Block

`Block` to lista `"{"[Stmt]"}"`. `Stmt` oddzielone są `;`. Wynika z tego, że
wszsytkie `Stmt` wewnątrz bloku kończą się `;`. To się również tyczy  `if`,
`while`, czy deklaracji funkcji.



### if:

```
if [Expr] {
	[Stmt]
} else {
	[Stmt]
}
```
Expr musi wyliczać się do typu `bool`. Nawiasy klamrowe są wymagane. Gałęź else
jest opcjonalny.



### while

```
while [Expr] {
    [Stmt]
}
```

Expr musi wyliczać się do typu `bool`, jest sprawdzany *przed* każdym obrotem
pętli.



### print

```
print [Expr]
```

Instrukcja wbudowana slużąca do wypisywania na stdout. Wylicza wartość
wyrażenia i wypisuje je na standardowe wyjście.



### Obsługa błędów
Występują dwa typy wyjątków:
 
- `Error` - taki wyjątek jest zgłaszany podczas statycznego typowania.
- `RuntimeException` - taki wyjątek jest zgłaszany podczas interpretowania
    programu (np. dzielenie przez 0).



### Struktura katalogów

Plik Main.hs uruchamia interpreter. Pliki źródłowe znajdują są w katalogu src/:
- src/Interpreter/: 
  * `Interpreter.hs` - przygotowuje `env` interpreterowi.
  * `Eval.hs` - właściwy interpreter.
  * `IUtils.hs` - pomocnicze funkcje do zarządzania pamięcią interpretera.
  * `ITypes.hs` - wszystkie wykorzystane typy w interpreterze.
- src/TypeChecker: 
  * `TypeChecker.hs` - moduł odpowiedzialny za statyczne typowanie.
  * `TUtils.hs` - pomocnicze funkcje do zarządzania pamięci typecheckerze.
  * `TTypes.hs` - wszystkie wykorzystane typy w typecheckerze.
- src/generated - pliki wygenerowane automatycznie przez bnfc.



## Tabela cech
```
Na 15 punktów
1 (trzy typy)
2 (literały, arytmetyka, porównania)
3 (zmienne, przypisanie)
4 (print)
5 (while, if)
6 (funkcje, rekurencja)
8 (przez zmienną i wartość)
Na 20 punktów
9 (przesłanianie i statyczne wiązanie)
10 (obsługa błędów wykonania)
11 (funkcje zwracające wartość)
Na 30 punktów
12 (4) (statyczne typowanie)
13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
14 (1) (listy)
16 (1) (break, continue)
```
Razem: 28
