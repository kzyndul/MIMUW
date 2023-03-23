from sqlalchemy import func
from sqlalchemy.orm import Session

from sql_app import models, schemas
from sql_app.crud.baseCrude import get_by_id_tabela, get_tabela, uzytkownik_z_najwiecej, uzytkownik_ile_uniwersalna, \
    tabela_ile_uniwersalna_razem, podaj_liczbe_uniwersalna_uzytkownika, tabela_z_najwiecej_razem


def get_uzytkownicy(db: Session, skip: int = 0, limit: int = 100):
    return get_tabela(db, models.Uzytkownik, skip, limit)

def get_uzytkownicy_by_id(db: Session, id_uzytkownik: int):
    return get_by_id_tabela(db, models.Uzytkownik, id_uzytkownik)

def get_uzytkownik_by_imie(db: Session, imie: str):
    return db.query(models.Uzytkownik).filter(models.Uzytkownik.imie.ilike("%" + imie + "%")).all()

def get_uzytkownik_by_nazwisko(db: Session, nazwisko: str):
    return db.query(models.Uzytkownik).filter(models.Uzytkownik.nazwisko.ilike("%" + nazwisko + "%")).all()



def uzytkownik_z_najwiecej_wydan(db: Session, skip: int = 0, limit: int = 100):
    return uzytkownik_z_najwiecej(db, models.Uzytkownik_Wydanie, skip, limit)


def uzytkownik_z_najwiecej_audiobukow(db: Session, skip: int = 0, limit: int = 100):
    return uzytkownik_z_najwiecej(db, models.Uzytkownik_Audiobuk, skip, limit)

def uzytkownik_z_najwiecej_razem(db: Session, skip: int = 0, limit: int = 100):
    return tabela_z_najwiecej_razem(db, models.Uzytkownik, models.Uzytkownik_Audiobuk, models.Uzytkownik_Wydanie, skip, limit)




def uzytkownik_ile_wydan(db: Session, skip: int = 0, limit: int = 100):
    return uzytkownik_ile_uniwersalna(db, models.Uzytkownik_Wydanie, skip, limit)

def uzytkownik_ile_audiobukow(db: Session, skip: int = 0, limit: int = 100):
    return uzytkownik_ile_uniwersalna(db, models.Uzytkownik_Audiobuk, skip, limit)

def uzytkownik_ile_razem(db: Session, skip: int = 0, limit: int = 100):
    return tabela_ile_uniwersalna_razem(db, models.Uzytkownik, models.Uzytkownik_Wydanie, models.Uzytkownik_Audiobuk, skip, limit)





def podaj_liczbe_wydan_uzytkownika(db: Session, id_uzytkownik: int):
    return podaj_liczbe_uniwersalna_uzytkownika(db, models.Uzytkownik_Wydanie, id_uzytkownik)

def podaj_liczbe_audiobukow_uzytkownika(db: Session, id_uzytkownik: int):
    return podaj_liczbe_uniwersalna_uzytkownika(db, models.Uzytkownik_Audiobuk, id_uzytkownik)

def podaj_liczbe_razem_uzytkownik(db: Session, id_uzytkownik: int):
    return podaj_liczbe_audiobukow_uzytkownika(db, id_uzytkownik=id_uzytkownik) + podaj_liczbe_wydan_uzytkownika(db, id_uzytkownik=id_uzytkownik)




def create_uzytkownik(db: Session, uzytkownik: schemas.UzytkownikStworz):
    db_uzytkownik = models.Uzytkownik(imie=uzytkownik.imie, nazwisko=uzytkownik.nazwisko, data_urodzenia=uzytkownik.data_urodzenia)
    for id_wydania in uzytkownik.idWydan:
        wydanie = get_by_id_tabela(db, models.Wydanie,id_wydania)
        db_uzytkownik.przeczytal.append(wydanie)

    for id_audiobuk in uzytkownik.idAudiobukow:
        audiobuk = get_by_id_tabela(db, models.Audiobuk,id_audiobuk)
        db_uzytkownik.przesluchal.append(audiobuk)

    db.add(db_uzytkownik)
    db.commit()
    db.refresh(db_uzytkownik)
    return db_uzytkownik