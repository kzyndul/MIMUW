from sqlalchemy.orm import Session

from sql_app import models, schemas
from sql_app.crud.baseCrude import get_by_id_tabela, get_tabela

def get_audiobuki(db: Session, skip: int = 0, limit: int = 100):
    return get_tabela(db, models.Audiobuk, skip, limit)


def get_audiobuk_by_id(db: Session, id_audiobuk: int):
    return get_by_id_tabela(db, models.Audiobuk, id_audiobuk)


def create_audiobuk(db: Session, audiobuk: schemas.AudiobukStworz, id_utwor: int):
    db_audiobuk = models.Audiobuk(**audiobuk.dict(), id_utwor=id_utwor)
    db_utwor = get_by_id_tabela(db, models.Utwor,id_utwor)
    db_utwor.audiobuki.append(db_audiobuk)
    db.add(db_audiobuk)
    db.commit()
    db.refresh(db_audiobuk)
    return db_audiobuk


def przypisz_audiobuk_uzytkownikowi(db: Session, id_uzytkownik: int, id_audiobuk: int):
    db_uzytkownik = get_by_id_tabela(db, models.Uzytkownik, id_uzytkownik)
    db_audiobuk = get_audiobuk_by_id(db, id_audiobuk=id_audiobuk)
    db_uzytkownik.przesluchal.append(db_audiobuk)
    db.commit()
    db.refresh(db_uzytkownik)
