from sqlalchemy.orm import Session

from sql_app import models, schemas
from sql_app.crud.baseCrude import get_by_id_tabela, get_tabela

def get_wydania(db: Session, skip: int = 0, limit: int = 100):
    return get_tabela(db, models.Wydanie, skip, limit)


def get_wydanie_by_id(db: Session, id_wydanie: int):
    return get_by_id_tabela(db, models.Wydanie, id_wydanie)


def create_wydanie(db: Session, wydanie: schemas.WydanieStworz, id_utwor: int):
    db_wydanie = models.Wydanie(**wydanie.dict(), id_utwor=id_utwor)
    db_utwor = get_by_id_tabela(db, models.Utwor, id_utwor)
    db_utwor.wydania.append(db_wydanie)
    db.add(db_wydanie)
    db.commit()
    db.refresh(db_wydanie)
    return db_wydanie




def przypisz_wydanie_uzytkownikowi(db: Session, id_uzytkownik: int, id_wydanie: int):
    db_uzytkownik = get_by_id_tabela(db, models.Uzytkownik, id_uzytkownik)
    db_wydanie = get_wydanie_by_id(db, id_wydanie=id_wydanie)
    db_uzytkownik.przeczytal.append(db_wydanie)
    db.commit()
    db.refresh(db_uzytkownik)
