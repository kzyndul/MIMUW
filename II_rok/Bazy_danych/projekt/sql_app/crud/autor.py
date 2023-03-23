from sqlalchemy.orm import Session

from sql_app import models, schemas
from sql_app.crud.baseCrude import get_by_id_tabela, get_tabela, autor_z_najwiecej, autor_ile_uniwersalna, \
    podaj_liczbe_uniwersalna_autora


def get_autorzy(db: Session, skip: int = 0, limit: int = 100):
    return get_tabela(db, models.Autor, skip, limit)


def get_autor_by_id(db: Session, id: int):
    return get_by_id_tabela(db, models.Autor, id)


def autor_z_najwiecej_utworow(db: Session, skip: int = 0, limit: int = 100):
    return autor_z_najwiecej(db, models.Autor_Utwor, skip, limit)


def autor_ile_utworow(db: Session, skip: int = 0, limit: int = 100):
    return autor_ile_uniwersalna(db, models.Autor_Utwor, skip, limit)


def create_autor(db: Session, autor: schemas.AutorStworz):
    db_autor = models.Autor(autor=autor.autor)
    db.add(db_autor)
    db.commit()
    db.refresh(db_autor)
    return db_autor


def podaj_liczbe_utworow_autora(db: Session, id_autora: int):
    return podaj_liczbe_uniwersalna_autora(db, models.Autor_Utwor, id_autora)


def get_autor_by_imie(db: Session, autor: str):
    return db.query(models.Autor).filter(models.Autor.autor.ilike("%" + autor + "%")).all()


def przypisz_autora(db: Session, id_utwor: int, id_autora: int):
    db_utwor = get_by_id_tabela(db, models.Utwor, id_utwor)
    db_autor = get_autor_by_id(db, id_autora)

    db_utwor.autorzy.append(db_autor)
    db.commit()
    db.refresh(db_utwor)