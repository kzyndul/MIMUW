from sql_app.schemas import Autor, Uzytkownik, Audiobuk, Wydanie, Utwor




# Lista przeczytanych przez Urzytkownikow Wydan
class UzytkownikWydanie(Uzytkownik):
    przeczytal: list[Wydanie]


# Lista przesluchanych przez Urzytkownikow Audiobukow
class UzytkownikAudiobuk(Uzytkownik):
    przesluchal: list[Audiobuk]


# Lista przeczytanych przez Urzytkownikow Wydan i przesluchanych Audiobukow
class UzytkownikWszystko(UzytkownikWydanie, UzytkownikAudiobuk):
    def __init__(self):
        self.id = super().id
        self.imie = super().imie
        self.nazwisko = super().nazwisko
        self.dataUrodzenia = super().data_urodzenia


# Lista Autorow utworu
class UtworAutorzy(Utwor):
    autorzy: list[Autor]


# Lista Wydan utworu
class UtworWydanie(Utwor):
    wydania: list[Wydanie]


# Lista Audiobukow Utworu
class UtworAudiobuk(Utwor):
    audiobuki: list[Audiobuk]


# Lista Audiobukow i Wydan Utworu
class UtworWszystko(UtworAutorzy, UtworWydanie, UtworAudiobuk):
    def __init__(self):
        self.id = super().id
        self.tytul = super().tytul


# Lista Utworow napisanych przez Autora
class AutorSchema(Autor):
    utwory: list[Utwor]