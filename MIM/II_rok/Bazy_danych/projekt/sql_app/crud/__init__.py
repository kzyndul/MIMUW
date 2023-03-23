from .utwor import utwor_z_najwiecej_audiobukow, utwor_ile_audiobukow, get_utwor_by_id, get_utwory, create_utwor,\
    utwor_z_najwiecej_razem, utwor_z_najwiecej_wydan, podaj_liczbe_wydan_utworu, podaj_liczbe_audiobukow_utworu,\
    podaj_liczbe_razem_utworu, utwor_ile_wydan, utwor_ile_razem, get_utwor_by_tytul
from .autor import autor_z_najwiecej_utworow, autor_ile_utworow, get_autor_by_id, get_autor_by_imie, get_autorzy,\
    create_autor, podaj_liczbe_utworow_autora, przypisz_autora
from .uzytkownik import uzytkownik_ile_wydan, uzytkownik_ile_audiobukow, uzytkownik_z_najwiecej_audiobukow,\
    uzytkownik_z_najwiecej_razem, uzytkownik_z_najwiecej_wydan, create_uzytkownik, get_uzytkownicy_by_id, get_uzytkownicy, \
    podaj_liczbe_razem_uzytkownik, podaj_liczbe_audiobukow_uzytkownika, podaj_liczbe_wydan_uzytkownika, uzytkownik_ile_razem, \
    get_uzytkownik_by_nazwisko, get_uzytkownik_by_imie
from .wydanie import get_wydanie_by_id, create_wydanie, przypisz_wydanie_uzytkownikowi, get_wydania
from .audiobuk import get_audiobuk_by_id, create_audiobuk, get_audiobuki, przypisz_audiobuk_uzytkownikowi