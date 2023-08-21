### Systemy operacyjne

<b> Rozwiązanie piątego zadania z SO MIMUW 2023 </b> <br />
<b> Autor: Krzysztof Żyndul </b>



**Blokowanie plików przez użytkowników na wyłączność w systemie MINIX**

Twoim zadaniem jest rozszerzenie serwera vfs w systemie MINIX o mechanizm umożliwiający użytkownikom blokowanie plików na wyłączność. Oto opis, jak ten mechanizm powinien działać:

1. **Wywołania systemowe VFS_FEXCLUSIVE i VFS_EXCLUSIVE**

Dodaj nowe wywołania systemowe `VFS_FEXCLUSIVE` i `VFS_EXCLUSIVE` do serwera vfs w MINIX. Te wywołania umożliwią użytkownikom blokowanie plików na wyłączność w opisany sposób.

2. **Mechanizm blokowania plików**

Mechanizm blokowania plików opiera się na dwóch wywołaniach systemowych: `VFS_FEXCLUSIVE` i `VFS_EXCLUSIVE`. Te wywołania pozwalają użytkownikom blokować pliki oraz odblokowywać je w zależności od określonych warunków.

- `VFS_FEXCLUSIVE`:
  - Blokuje plik wskazany przez deskryptor na wyłączność użytkownika, który wywołał to wywołanie.
  - Jeśli plik nie zostanie odblokowany jawnie przez użytkownika, to plik zostanie odblokowany automatycznie w momencie zamknięcia deskryptora.
  - Działa tylko na pliku, który jest otwarty w trybie do odczytu lub zapisu.
  - Jeśli plik jest już zablokowany, wywołanie kończy się błędem EALREADY.
  - Użytkownik może zablokować co najwyżej `NR_EXCLUSIVE` plików.

- `VFS_EXCLUSIVE`:
  - Blokuje plik wskazany przez ścieżkę na wyłączność użytkownika, który wywołał to wywołanie.
  - Plik pozostaje zablokowany do momentu jego odblokowania przez użytkownika lub do momentu, gdy plik przestanie być używany przez wszystkich użytkowników (np. plik zostanie usunięty lub przeniesiony).
  - Działa tylko na plikach, do których użytkownik ma uprawnienia odczytu lub zapisu.
  - Możliwe akcje:
    - `EXCL_LOCK`: Blokuje plik. Plik pozostaje zablokowany, aż nie zostanie jawnie odblokowany przez użytkownika lub spełniony warunek automatycznego odblokowania.
    - `EXCL_LOCK_NO_OTHERS`: Blokuje plik, ale tylko jeśli nie jest otwarty przez innych użytkowników. W przeciwnym razie wywołanie kończy się błędem EAGAIN.
    - `EXCL_UNLOCK`: Odblokowuje plik. Plik odblokować może tylko użytkownik, który go zablokował.
    - `EXCL_UNLOCK_FORCE`: Odblokowuje plik. Plik odblokować może użytkownik, który go zablokował, właściciel pliku lub superużytkownik.

**Uwagi:**
- Blokady plików są wykonywane na poziomie użytkowników, nie procesów.
- Użytkownik jest identyfikowany przez swój rzeczywisty UID.
- Mechanizm blokowania obejmuje standardowe operacje na plikach, a nie dotyczy specjalnych typów plików (np. katalogów, pseudo-urządzeń).
- Blokady plików nie są zachowywane po odmontowaniu systemu plików.
- Wprowadzone wywołania systemowe mają odpowiednie kody błędów (np. EINVAL, EBADF, EACCES) w zależności od sytuacji.

Implementacja tego mechanizmu polega na dodaniu nowych wywołań systemowych oraz odpowiedniego zarządzania blokadami plików w serwerze vfs w MINIX.