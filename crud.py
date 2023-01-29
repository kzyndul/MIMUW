from sqlalchemy import func
from sqlalchemy.orm import Session
import sys


from . import models, schemas


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)



# get wszystkie





def get_autorzy(db: Session, skip: int = 0, limit: int = 100):
    return db.query(models.Autor).offset(skip).limit(limit).all()

def get_utwory(db: Session, skip: int = 0, limit: int = 100):
    return db.query(models.Utwor).offset(skip).limit(limit).all()

def get_uzytkownicy(db: Session, skip: int = 0, limit: int = 100):
    return db.query(models.Uzytkownik).offset(skip).limit(limit).all()

def get_audiobuki(db: Session, skip: int = 0, limit: int = 100):
    return db.query(models.Audiobuk).offset(skip).limit(limit).all()

def get_wydania(db: Session, skip: int = 0, limit: int = 100):
    return db.query(models.Wydanie).offset(skip).limit(limit).all()




# get by id



def get_utwor_by_id(db: Session, id_utwor: int):
    return db.query(models.Utwor).filter(models.Utwor.id == id_utwor).first()

def get_autor_by_id(db: Session, id_autor: int):
    return db.query(models.Autor).filter(models.Autor.id == id_autor).first()

def get_wydanie_by_id(db: Session, id_wydanie: int):
    return db.query(models.Wydanie).filter(models.Wydanie.id == id_wydanie).first()

def get_audiobuk_by_id(db: Session, id_audiobuk: int):
    return db.query(models.Audiobuk).filter(models.Audiobuk.id == id_audiobuk).first()

def get_uzytkownicy_by_id(db: Session, id_uzytkownik: int):
    return db.query(models.Uzytkownik).filter(models.Uzytkownik.id == id_uzytkownik).first()


# get by imie
def get_autor_by_imie(db: Session, autor: str):
    return db.query(models.Autor).filter(models.Autor.autor == autor).first()






# create

def create_autor(db: Session, autor: schemas.AutorStworz):
    db_autor = models.Autor(autor=autor.autor)
    db.add(db_autor)
    db.commit()
    db.refresh(db_autor)
    return db_autor


def create_utwor(db: Session, utwor: schemas.UtworStworz):
    db_utwor = models.Utwor(tytul=utwor.tytul)
    for id_autora in utwor.idAutorow:
        autor = get_autor_by_id(db, id_autora)
        db_utwor.autorzy.append(autor)
    db.add(db_utwor)
    db.commit()
    db.refresh(db_utwor)
    return db_utwor


def create_wydanie(db: Session, wydanie: schemas.WydanieStworz, id_utwor: int):
    db_wydanie = models.Wydanie(**wydanie.dict(), id_utwor=id_utwor)
    db_utwor = get_utwor_by_id(db, id_utwor)
    db_utwor.wydania.append(db_wydanie)
    db.add(db_wydanie)
    db.commit()
    db.refresh(db_wydanie)
    return db_wydanie


def create_audiobuk(db: Session, audiobuk: schemas.AudiobukStworz, id_utwor: int):
    db_audiobuk = models.Audiobuk(**audiobuk.dict(), id_utwor=id_utwor)
    db_utwor = get_utwor_by_id(db, id_utwor)
    db_utwor.audiobuki.append(db_audiobuk)
    db.add(db_audiobuk)
    db.commit()
    db.refresh(db_audiobuk)
    return db_audiobuk


def create_uzytkownik(db: Session, uzytkownik: schemas.UzytkownikStworz):
    db_uzytkownik = models.Uzytkownik(imie=uzytkownik.imie, nazwisko=uzytkownik.nazwisko, data_urodzenia=uzytkownik.data_urodzenia)
    for id_wydania in uzytkownik.idWydan:
        wydanie = get_wydanie_by_id(db, id_wydania)
        db_uzytkownik.przeczytal.append(wydanie)

    for id_audiobuk in uzytkownik.idAudiobukow:
        audiobuk = get_audiobuk_by_id(db, id_audiobuk)
        db_uzytkownik.przesluchal.append(audiobuk)

    db.add(db_uzytkownik)
    db.commit()
    db.refresh(db_uzytkownik)
    return db_uzytkownik




def przypisz_wydanie_uzytkownikowi(db: Session, id_uzytkownik: int, id_wydanie: int):
    db_uzytkownik = get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    db_wydanie = get_wydanie_by_id(db, id_wydanie=id_wydanie)
    db_uzytkownik.przeczytal.append(db_wydanie)
    db.commit()
    db.refresh(db_uzytkownik)


def przypisz_audiobuk_uzytkownikowi(db: Session, id_uzytkownik: int, id_audiobuk: int):
    db_uzytkownik = get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    db_audiobuk = get_audiobuk_by_id(db, id_audiobuk=id_audiobuk)
    db_uzytkownik.przesluchal.append(db_audiobuk)
    db.commit()
    db.refresh(db_uzytkownik)


def przypisz_autora(db: Session, id_utwor: int, id_autora: int):
    db_utwor = get_utwor_by_id(db, id_utwor)
    db_autor = get_autor_by_id(db, id_autora)

    db_utwor.autorzy.append(db_autor)
    db.commit()
    db.refresh(db_utwor)

#######################################################################################################
# autor-utwor
def autor_z_najwiecej_utworow(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Autor.id.label("id"), func.count(models.Autor_Utwor.id_utwor).label('count')).join(models.Autor_Utwor, isouter=True).\
        group_by(models.Autor.id).subquery()
    temp_a = db.query(func.max(temp.c.count)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count == temp_a).subquery()
    return db.query(models.Autor).filter(models.Autor.id.in_(ids)).offset(skip).limit(limit).all()


def podaj_liczbe_utworow_autora(db: Session, id_autora: int):
    temp = db.query(models.Autor, func.count(models.Autor_Utwor.id_utwor).label('count')).join(models.Autor_Utwor).filter(models.Autor.id == id_autora). \
        group_by(models.Autor.id).first()
    if temp is None:
        return 0
    return db.query(temp.count).scalar()


def autor_ile_utworow(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Autor.id, func.count(models.Autor_Utwor.id_utwor).label('count')).join(models.Autor_Utwor, isouter=True).\
        group_by(models.Autor.id).order_by(models.Autor.id).offset(skip).limit(limit).all()
    druga = db.query(models.Autor).order_by(models.Autor.id).offset(skip).limit(limit).all()
    test = []
    for x in range(len(temp)):
        test.append(temp[x][1])
    return zip(druga, test)








# Utwor - wydanie
def utwor_z_najwiecej_wydan(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Utwor.id.label("id"), func.count(models.Wydanie.id_utwor).label('count')).join(models.Wydanie, isouter=True).\
        group_by(models.Utwor.id).subquery()
    temp_a = db.query(func.max(temp.c.count)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count == temp_a).subquery()
    return db.query(models.Utwor).filter(models.Utwor.id.in_(ids)).offset(skip).limit(limit).all()


def podaj_liczbe_wydan_utworu(db: Session, id_utworu: int):
    temp = db.query(models.Utwor, func.count(models.Wydanie.id_utwor).label('count')).join(models.Wydanie, isouter=True).filter(models.Utwor.id == id_utworu). \
        group_by(models.Utwor.id).first()
    if temp is None:
        return 0
    return db.query(temp.count).scalar()


def utwor_ile_wydan(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Utwor.id, func.count(models.Wydanie.id_utwor).label('count')).join(models.Wydanie, isouter=True).\
        group_by(models.Utwor.id).order_by(models.Utwor.id).offset(skip).limit(limit).all()
    druga = db.query(models.Utwor).order_by(models.Utwor.id).offset(skip).limit(limit).all()
    test = []
    for x in range(len(temp)):
        test.append(temp[x][1])
    return zip(druga, test)

def utwor_ile_wydan2(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Utwor.id, func.count(models.Wydanie.id_utwor).label('count')).join(models.Wydanie, isouter=True).\
        group_by(models.Utwor.id).order_by(models.Utwor.id).offset(skip).limit(limit).all()
    druga = db.query(models.Utwor).order_by(models.Utwor.id).offset(skip).limit(limit).all()
    test = []
    for x in range(len(temp)):
        test.append(temp[x][1])
    return zip(druga, test)





# Utwor - audiobuk
# def utwor_z_najwiecej_audiobukow(db: Session, skip: int = 0, limit: int = 100):
#     temp = db.query(models.Utwor.id.label("id"), func.count(models.Audiobuk.id_utwor).label('count')).join(models.Audiobuk, isouter=True).\
#         group_by(models.Utwor.id).subquery()
#     temp_a = db.query(func.max(temp.c.count)).scalar()
#     ids = db.query(temp.c.id).filter(temp.c.count == temp_a).subquery()
#     return db.query(models.Utwor).filter(models.Utwor.id.in_(ids)).offset(skip).limit(limit).all()


def podaj_liczbe_audiobukow_utworu(db: Session, id_utworu: int):
    temp = db.query(models.Utwor, func.count(models.Audiobuk.id_utwor).label('count')).join(models.Audiobuk, isouter=True).filter(models.Utwor.id == id_utworu). \
        group_by(models.Utwor.id).first()
    if temp is None:
        return 0
    return db.query(temp.count).scalar()


def utwor_ile_audiobukow(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Utwor.id, func.count(models.Audiobuk.id_utwor).label('count')).join(models.Audiobuk, isouter=True).\
        group_by(models.Utwor.id).order_by(models.Utwor.id).offset(skip).limit(limit).all()
    druga = db.query(models.Utwor).order_by(models.Utwor.id).offset(skip).limit(limit).all()
    test = []
    for x in range(len(temp)):
        test.append(temp[x][1])
    return zip(druga, test)






# Utwor - razem
def utwor_z_najwiecej_razem(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Utwor.id.label("id"), func.count(models.Audiobuk.id_utwor).label('count1'), func.count(models.Wydanie.id_utwor).label('count'))\
        .join(models.Wydanie, models.Wydanie.id_utwor == models.Utwor.id, isouter=True)\
        .join(models.Audiobuk, models.Audiobuk.id_utwor == models.Utwor.id, isouter=True).group_by(models.Utwor.id).subquery()
    temp_a = db.query(func.max(temp.c.count + temp.c.count1)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count + temp.c.count1 == temp_a).subquery()
    return db.query(models.Utwor).filter(models.Utwor.id.in_(ids)).offset(skip).limit(limit).all()


def podaj_liczbe_razem_utworu(db: Session, id_utworu: int):
    return podaj_liczbe_audiobukow_utworu(db, id_utworu=id_utworu) + podaj_liczbe_wydan_utworu(db, id_utworu=id_utworu)

def utwor_ile_razem(db: Session, skip: int = 0, limit: int = 100):
    temp1 = db.query(models.Utwor.id, func.count(models.Wydanie.id_utwor).label('count')).join(models.Wydanie, isouter=True).\
        group_by(models.Utwor.id).order_by(models.Utwor.id).offset(skip).limit(limit).all()
    temp2 = db.query(models.Utwor.id, func.count(models.Audiobuk.id_utwor).label('count')).join(models.Audiobuk, isouter=True).\
        group_by(models.Utwor.id).order_by(models.Utwor.id).offset(skip).limit(limit).all()

    druga = db.query(models.Utwor).order_by(models.Utwor.id).offset(skip).limit(limit).all()
    test = []
    for x in range(len(temp1)):
        test.append(temp1[x][1] + temp2[x][1])
    return zip(druga, test)








# Urzytkownik - wydanie

def uzytkownik_z_najwiecej_wydan(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Uzytkownik.id.label("id"), func.count(models.Uzytkownik_Wydanie.id_utwor).label('count'))\
        .join(models.Uzytkownik_Wydanie, isouter=True).group_by(models.Uzytkownik.id).subquery()
    temp_a = db.query(func.max(temp.c.count)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count == temp_a).subquery()
    return db.query(models.Uzytkownik).filter(models.Uzytkownik.id.in_(ids)).offset(skip).limit(limit).all()

def podaj_liczbe_wydan_uzytkownika(db: Session, id_uzytkownik: int):
    temp = db.query(models.Uzytkownik, func.count(models.Uzytkownik_Wydanie.id_utwor).label('count')).join(models.Uzytkownik_Wydanie, isouter=True)\
        .filter(models.Uzytkownik.id == id_uzytkownik).group_by(models.Uzytkownik.id).first()
    if temp is None:
        return 0
    return db.query(temp.count).scalar()

def uzytkownik_ile_wydan(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Uzytkownik.id, func.count(models.Uzytkownik_Wydanie.id_utwor).label('count')).join(models.Uzytkownik_Wydanie, isouter=True).\
        group_by(models.Uzytkownik.id).order_by(models.Uzytkownik.id).offset(skip).limit(limit).all()
    druga = db.query(models.Uzytkownik).order_by(models.Uzytkownik.id).offset(skip).limit(limit).all()
    test = []
    for x in range(len(temp)):
        test.append(temp[x][1])
    return zip(druga, test)







# Uzytkownik Audiobuk



def uzytkownik_z_najwiecej_audiobukow(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Uzytkownik.id.label("id"), func.count(models.Uzytkownik_Audiobuk.id_utwor).label('count'))\
        .join(models.Uzytkownik_Audiobuk, isouter=True).group_by(models.Uzytkownik.id).subquery()
    temp_a = db.query(func.max(temp.c.count)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count == temp_a).subquery()
    return db.query(models.Uzytkownik).filter(models.Uzytkownik.id.in_(ids)).offset(skip).limit(limit).all()

def podaj_liczbe_audiobukow_uzytkownika(db: Session, id_uzytkownik: int):
    temp = db.query(models.Uzytkownik, func.count(models.Uzytkownik_Audiobuk.id_utwor).label('count')).join(models.Uzytkownik_Audiobuk, isouter=True)\
        .filter(models.Uzytkownik.id == id_uzytkownik).group_by(models.Uzytkownik.id).first()
    if temp is None:
        return 0
    return db.query(temp.count).scalar()

def uzytkownik_ile_audiobukow(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Uzytkownik.id, func.count(models.Uzytkownik_Audiobuk.id_utwor).label('count')).join(models.Uzytkownik_Audiobuk, isouter=True).\
        group_by(models.Uzytkownik.id).order_by(models.Uzytkownik.id).offset(skip).limit(limit).all()
    druga = db.query(models.Uzytkownik).order_by(models.Uzytkownik.id).offset(skip).limit(limit).all()
    test = []
    for x in range(len(temp)):
        test.append(temp[x][1])
    return zip(druga, test)



# Uzytkownik Razem


def uzytkownik_z_najwiecej_razem(db: Session, skip: int = 0, limit: int = 100):
    temp = db.query(models.Uzytkownik.id.label("id"), func.count(models.Uzytkownik_Wydanie.id_utwor).label('count1'), func.count(models.Uzytkownik_Audiobuk.id_utwor).label('count'))\
        .join(models.Uzytkownik_Wydanie, models.Uzytkownik_Wydanie.id_uzytkownik == models.Uzytkownik.id, isouter=True)\
        .join(models.Uzytkownik_Audiobuk, models.Uzytkownik_Audiobuk.id_uzytkownik == models.Uzytkownik.id, isouter=True).group_by(models.Uzytkownik.id).subquery()
    temp_a = db.query(func.max(temp.c.count + temp.c.count1)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count + temp.c.count1 == temp_a).subquery()
    return db.query(models.Uzytkownik).filter(models.Uzytkownik.id.in_(ids)).offset(skip).limit(limit).all()


def podaj_liczbe_razem_uzytkownik(db: Session, id_uzytkownik: int):
    return podaj_liczbe_audiobukow_uzytkownika(db, id_uzytkownik=id_uzytkownik) + podaj_liczbe_wydan_uzytkownika(db, id_uzytkownik=id_uzytkownik)

def uzytkownik_ile_razem(db: Session, skip: int = 0, limit: int = 100):
    temp1 = db.query(models.Uzytkownik.id, func.count(models.Uzytkownik_Wydanie.id_utwor).label('count')).join(models.Uzytkownik_Wydanie, isouter=True).\
        group_by(models.Uzytkownik.id).order_by(models.Uzytkownik.id).offset(skip).limit(limit).all()
    temp2 = db.query(models.Uzytkownik.id, func.count(models.Uzytkownik_Audiobuk.id_utwor).label('count')).join(models.Uzytkownik_Audiobuk, isouter=True).\
        group_by(models.Uzytkownik.id).order_by(models.Uzytkownik.id).offset(skip).limit(limit).all()

    druga = db.query(models.Uzytkownik).order_by(models.Uzytkownik.id).offset(skip).limit(limit).all()
    test = []
    for x in range(len(temp1)):
        test.append(temp1[x][1] + temp2[x][1])
    return zip(druga, test)






# Ulepszone zapytanie

def get_by_id_uniwersalne(db: Session, tabela, id: int):
    return db.query(tabela).filter(tabela.id == id).first()

def get_uniwersalne(db: Session, tabela, skip: int = 0, limit: int = 100):
    return db.query(tabela).offset(skip).limit(limit).all()







def tabela_z_najwiecej(db: Session, tabela, tabel_z_utworami, skip: int = 0, limit: int = 100):
    temp = db.query(tabela.id.label("id"), func.count(tabel_z_utworami.id_utwor).label('count')).join(tabel_z_utworami, isouter=True).\
        group_by(tabela.id).subquery()
    temp_a = db.query(func.max(temp.c.count)).scalar()
    ids = db.query(temp.c.id).filter(temp.c.count == temp_a).subquery()
    return db.query(tabela).filter(tabela.id.in_(ids)).offset(skip).limit(limit).all()

def utwor_z_najwiecej(db: Session, tabel_z_utworami, skip: int = 0, limit: int = 100):
    return tabela_z_najwiecej(db, models.Utwor, tabel_z_utworami, skip, limit)

def utwor_z_najwiecej_audiobukow(db: Session, skip: int = 0, limit: int = 100):
    return utwor_z_najwiecej(db, models.Audiobuk, skip, limit)





def utwor_ile_audiobukow_2(db: Session, skip: int = 0, limit: int = 100):
    audiobook_count_query = db.query(models.Utwor.id, func.count(models.Audiobuk.id_utwor).label('count')).join(models.Audiobuk, isouter=True).\
        group_by(models.Utwor.id).order_by(models.Utwor.id).subquery()
    utwor_ile_audiobookow = db.query(models.Utwor, audiobook_count_query.c.count). \
        join(audiobook_count_query, models.Utwor.id == audiobook_count_query.c.id). \
        order_by(models.Utwor.id).offset(skip).limit(limit).all()
    return utwor_ile_audiobookow







