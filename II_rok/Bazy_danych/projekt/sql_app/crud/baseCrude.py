from sqlalchemy import func
from sqlalchemy.orm import Session

from sql_app import models

import sys

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)




def get_by_id_tabela(db: Session, tabela, id: int):
    return db.query(tabela).filter(tabela.id == id).first()

def get_tabela(db: Session, tabela, skip: int = 0, limit: int = 100):
    return db.query(tabela).offset(skip).limit(limit).all()






def tabela_z_najwiecej(db: Session, tabela, tabel_z_utworami, skip: int = 0, limit: int = 100):
    temp = db.query(tabela.id.label("id"), func.count(tabel_z_utworami.id_utwor).label('count')).join(tabel_z_utworami, isouter=True).\
        group_by(tabela.id).subquery()
    temp_a = db.query(func.max(temp.c.count)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count == temp_a).subquery()
    return db.query(tabela).filter(tabela.id.in_(ids)).offset(skip).limit(limit).all()


def utwor_z_najwiecej(db: Session, tabel_z_utworami, skip: int = 0, limit: int = 100):
    return tabela_z_najwiecej(db, models.Utwor, tabel_z_utworami, skip, limit)


def autor_z_najwiecej(db: Session, tabel_z_utworami, skip: int = 0, limit: int = 100):
    return tabela_z_najwiecej(db, models.Autor, tabel_z_utworami, skip, limit)


def uzytkownik_z_najwiecej(db: Session, tabel_z_utworami, skip: int = 0, limit: int = 100):
    return tabela_z_najwiecej(db, models.Uzytkownik, tabel_z_utworami, skip, limit)




def tabela_ile_uniwersalna(db: Session, tabela, tabel_z_utworami, skip: int = 0, limit: int = 100):
    uniwersalna_count_query = db.query(tabela.id, func.count(tabel_z_utworami.id_utwor).label('count')).join(tabel_z_utworami, isouter=True).\
        group_by(tabela.id).order_by(tabela.id).subquery()
    wynik = db.query(tabela, uniwersalna_count_query.c.count). \
        join(uniwersalna_count_query, tabela.id == uniwersalna_count_query.c.id). \
        order_by(tabela.id).offset(skip).limit(limit).all()
    return wynik

def utwor_ile_uniwersalna(db: Session, tabel_z_utworami, skip: int = 0, limit: int = 100):
    return tabela_ile_uniwersalna(db, models.Utwor, tabel_z_utworami, skip, limit)

def autor_ile_uniwersalna(db: Session, tabel_z_utworami, skip: int = 0, limit: int = 100):
    return tabela_ile_uniwersalna(db, models.Autor, tabel_z_utworami, skip, limit)

def uzytkownik_ile_uniwersalna(db: Session, tabel_z_utworami, skip: int = 0, limit: int = 100):
    return tabela_ile_uniwersalna(db, models.Uzytkownik, tabel_z_utworami, skip, limit)




def podaj_liczbe_uniwersalna_tabela(db: Session, tabela, tabel_z_utworami, id: int):
    temp = db.query(tabela, func.count(tabel_z_utworami.id_utwor).label('count')).join(tabel_z_utworami, isouter=True)\
        .filter(tabela.id == id).group_by(tabela.id).first()
    if temp is None:
        return 0
    return db.query(temp.count).scalar()

def podaj_liczbe_uniwersalna_uzytkownika(db: Session, tabel_z_utworami, id_uzytkownik: int):
    return podaj_liczbe_uniwersalna_tabela(db, models.Uzytkownik, tabel_z_utworami, id_uzytkownik)

def podaj_liczbe_uniwersalna_autora(db: Session, tabel_z_utworami, id_autora: int):
    return podaj_liczbe_uniwersalna_tabela(db, models.Autor, tabel_z_utworami, id_autora)



# TODO da sie lepiej pewnie
def tabela_ile_uniwersalna_razem(db: Session, tabela, tabel_z_utworami1, tabel_z_utworami2, skip: int = 0, limit: int = 100):
    uniwersalna_count_query1 = db.query(tabela.id.label('id'), func.count(tabel_z_utworami1.id_utwor).label('count')).join(tabel_z_utworami1, isouter=True).\
        group_by(tabela.id).order_by(tabela.id).subquery()

    uniwersalna_count_query2 = db.query(tabela.id.label('id'), func.count(tabel_z_utworami2.id_utwor).label('count')).join(
        tabel_z_utworami2, isouter=True). \
        group_by(tabela.id).order_by(tabela.id).subquery()

    razem_count_query = db.query(uniwersalna_count_query1.c.id.label('id'), func.sum(uniwersalna_count_query1.c.count + uniwersalna_count_query2.c.count).label('count'))\
        .join(uniwersalna_count_query2, uniwersalna_count_query1.c.id == uniwersalna_count_query2.c.id, isouter=True)\
        .group_by(uniwersalna_count_query1.c.id).subquery()

    wynik = db.query(tabela, razem_count_query.c.count).join(razem_count_query, tabela.id == razem_count_query.c.id). \
        order_by(tabela.id).offset(skip).limit(limit).all()

    return wynik


# TODO da sie lepiej pewnie
def tabela_z_najwiecej_razem(db: Session, tabela, tabel_z_utworami1, tabel_z_utworami2, skip: int = 0, limit: int = 100):
    temp = db.query(tabela.id.label("id"), func.count(tabel_z_utworami1.id_utwor).label('count1'), func.count(tabel_z_utworami2.id_utwor).label('count'))\
        .join(tabel_z_utworami1, tabel_z_utworami1.id_uzytkownik == tabela.id, isouter=True)\
        .join(tabel_z_utworami2, tabel_z_utworami2.id_uzytkownik == tabela.id, isouter=True).group_by(tabela.id).subquery()
    temp_a = db.query(func.max(temp.c.count + temp.c.count1)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count + temp.c.count1 == temp_a).subquery()
    return db.query(tabela).filter(tabela.id.in_(ids)).offset(skip).limit(limit).all()

