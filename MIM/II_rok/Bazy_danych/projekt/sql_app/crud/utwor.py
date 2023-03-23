from sqlalchemy import func
from sqlalchemy.orm import Session

from sql_app import models, schemas
from sql_app.crud.baseCrude import utwor_z_najwiecej, utwor_ile_uniwersalna, get_by_id_tabela, get_tabela, \
    tabela_ile_uniwersalna_razem, tabela_z_najwiecej_razem


def get_utwory(db: Session, skip: int = 0, limit: int = 100):
    return get_tabela(db, models.Utwor, skip, limit)

def get_utwor_by_id(db: Session, id: int):
    return get_by_id_tabela(db, models.Utwor, id)

def get_utwor_by_tytul(db: Session, tytul: str):
    return db.query(models.Utwor).filter(models.Utwor.tytul.ilike("%" + tytul + "%")).all()




def utwor_z_najwiecej_audiobukow(db: Session, skip: int = 0, limit: int = 100):
    return utwor_z_najwiecej(db, models.Audiobuk, skip, limit)

def utwor_z_najwiecej_wydan(db: Session, skip: int = 0, limit: int = 100):
    return utwor_z_najwiecej(db, models.Wydanie, skip, limit)

def utwor_z_najwiecej_razem(db: Session, skip: int = 0, limit: int = 100):
    return tabela_z_najwiecej_razem(db, models.Utwor, models.Audiobuk, models.Wydanie, skip, limit)




def utwor_ile_audiobukow(db: Session, skip: int = 0, limit: int = 100):
    return utwor_ile_uniwersalna(db, models.Audiobuk, skip, limit)

def utwor_ile_wydan(db: Session, skip: int = 0, limit: int = 100):
    return utwor_ile_uniwersalna(db, models.Wydanie, skip, limit)

def utwor_ile_razem(db: Session, skip: int = 0, limit: int = 100):
    return tabela_ile_uniwersalna_razem(db, models.Utwor, models.Wydanie, models.Audiobuk, skip, limit)




def create_utwor(db: Session, utwor: schemas.UtworStworz):
    db_utwor = models.Utwor(tytul=utwor.tytul)
    for id_autora in utwor.idAutorow:
        autor = get_by_id_tabela(db, models.Autor, id_autora)
        db_utwor.autorzy.append(autor)
    db.add(db_utwor)
    db.commit()
    db.refresh(db_utwor)
    return db_utwor





def podaj_liczbe_audiobukow_utworu(db: Session, id_utworu: int):
    temp = db.query(models.Utwor, func.count(models.Audiobuk.id_utwor).label('count')).join(models.Audiobuk, isouter=True).filter(models.Utwor.id == id_utworu). \
        group_by(models.Utwor.id).first()
    if temp is None:
        return 0
    return db.query(temp.count).scalar()


def podaj_liczbe_wydan_utworu(db: Session, id_utworu: int):
    temp = db.query(models.Utwor, func.count(models.Wydanie.id_utwor).label('count')).join(models.Wydanie, isouter=True).filter(models.Utwor.id == id_utworu). \
        group_by(models.Utwor.id).first()
    if temp is None:
        return 0
    return db.query(temp.count).scalar()

def podaj_liczbe_razem_utworu(db: Session, id_utworu: int):
    return podaj_liczbe_audiobukow_utworu(db, id_utworu=id_utworu) + podaj_liczbe_wydan_utworu(db, id_utworu=id_utworu)











