from typing import List

from fastapi import Depends, FastAPI, HTTPException
from sqlalchemy.orm import Session

from . import crud, models, schemas
from .database import get_db, create_tables

create_tables(models)

app = FastAPI()


import sys

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

# @app.post("/users/", response_model=schemas.User)
# def create_user(user: schemas.UserCreate, db: Session = Depends(get_db)):
#     db_user = crud.get_user_by_email(db, email=user.email)
#     if db_user:
#         raise HTTPException(status_code=400, detail="Email already registered")
#     return crud.create_user(db=db, user=user)
#
#
# @app.get("/users/", response_model=list[schemas.User])
# def read_users(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
#     users = crud.get_users(db, skip=skip, limit=limit)
#     return users
#
#
# @app.get("/users/{user_id}", response_model=schemas.User)
# def read_user(user_id: int, db: Session = Depends(get_db)):
#     db_user = crud.get_user(db, user_id=user_id)
#     if db_user is None:
#         raise HTTPException(status_code=404, detail="User not found")
#     return db_user
#
#
# @app.post("/users/{user_id}/items/", response_model=schemas.Item)
# def create_item_for_user(
#         user_id: int, item: schemas.ItemCreate, db: Session = Depends(get_db)
# ):
#     return crud.create_user_item(db=db, item=item, user_id=user_id)
#
#
# @app.get("/items/", response_model=list[schemas.Item])
# def read_items(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
#     items = crud.get_items(db, skip=skip, limit=limit)
#     return items

# @app.post("/users/", response_model=schemas.User)
# def create_user(user: schemas.UserCreate, db: Session = Depends(get_db)):
#     db_user = crud.get_useget_user_by_emailr_by_email(db, email=user.email)
#     if db_user:
#         raise HTTPException(status_code=400, detail="Email already registered")
#     return crud.create_user(db=db, user=user)


@app.post("/autor/", response_model=schemas.Autor)
def create_autor(autor: schemas.AutorStworz, db: Session = Depends(get_db)):
    temp = crud.get_autor_by_imie(db, autor=autor.autor)
    if temp is not None:
        return temp
    return crud.create_autor(db=db, autor=autor)


@app.get("/autor/", response_model=list[schemas.Autor])
def read_autor(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    autorzy = crud.get_autorzy(db, skip=skip, limit=limit)
    return autorzy

@app.get("/autor/{id_autora}", response_model=schemas.AutorSchema)
def read_autor_by_id(id_autora: int, db: Session = Depends(get_db)):
    autor = crud.get_autor_by_id(db, id_autora)
    if autor is None:
        raise HTTPException(status_code=404, detail="Autor not found")
    return autor


@app.post("/utwor/", response_model=schemas.Utwor)
def create_utwor(utwor: schemas.UtworStworz, db: Session = Depends(get_db)):
    if len(set(utwor.idAutorow)) != len(utwor.idAutorow):
        raise HTTPException(status_code=404, detail="Autor sie powtarza")
    for id_autora in utwor.idAutorow:
        autor = crud.get_autor_by_id(db, id_autora)
        if autor is None:
            raise HTTPException(status_code=404, detail="Autor not found")
    return crud.create_utwor(db=db, utwor=utwor)


@app.post("/przypisz/{id_utwor}/{id_autora}")
def przypisz_utwor_do_autor(id_utwor: int, id_autora: int,  db: Session = Depends(get_db)):
    db_utwor = crud.get_utwor_by_id(db, id_utwor)
    db_autor = crud.get_autor_by_id(db, id_autora)

    if db_autor is None:
        raise HTTPException(status_code=404, detail="Autor not found")

    if db_utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")

    if db_autor in db_utwor.autorzy:
        raise HTTPException(status_code=404, detail="Autor sie powtarza")

    crud.przypisz_autora(db=db, id_utwor=id_utwor, id_autora=id_autora)
    return {"Success", True}


@app.get("/utwor/", response_model=list[schemas.Utwor])
def read_utwor(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    utwory = crud.get_utwory(db, skip=skip, limit=limit)
    return utwory

@app.get("/utwor/{id_utwor}", response_model=schemas.UtworAutorzy)
def read_utwor_by_id(id_utwor: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utwor=id_utwor)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return utwor

@app.get("/utwor/{id_utwor}/wydanie/", response_model=schemas.UtworWydanie)
def read_utwor_with_wydania(id_utwor: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utwor=id_utwor)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return utwor

@app.get("/utwor/{id_utwor}/audiobuk/", response_model=schemas.UtworAudiobuk)
def read_utwor_with_audiobuk(id_utwor: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utwor=id_utwor)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return utwor

@app.get("/utwor/{id_utwor}/all/", response_model=schemas.UtworWszystko)
def read_utwor_with_all(id_utwor: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utwor=id_utwor)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return utwor


@app.post("/utwor/{id_utwor}/wydanie/", response_model=schemas.Wydanie)
def create_wydanie_for_utwor(
        id_utwor: int, wydanie: schemas.WydanieStworz, db: Session = Depends(get_db)
):
    db_utwor = crud.get_utwor_by_id(db, id_utwor=id_utwor)
    if db_utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.create_wydanie(db=db, wydanie=wydanie, id_utwor=id_utwor)


@app.post("/utwor/{id_utwor}/audiobuk/", response_model=schemas.Audiobuk)
def create_audiobuk_for_utwor(
        id_utwor: int, audiobuk: schemas.AudiobukStworz, db: Session = Depends(get_db)
):
    db_utwor = crud.get_utwor_by_id(db, id_utwor=id_utwor)
    if db_utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.create_audiobuk(db=db, audiobuk=audiobuk, id_utwor=id_utwor)


@app.post("/uzytkownik/", response_model=schemas.Uzytkownik)
def create_uzytkownik(uzytkownik: schemas.UzytkownikStworz, db: Session = Depends(get_db)):
    if len(set(uzytkownik.idWydan)) != len(uzytkownik.idWydan):
        raise HTTPException(status_code=404, detail="Wydanie sie powtarza")
    for id_wydania in uzytkownik.idWydan:
        wydanie = crud.get_wydanie_by_id(db, id_wydania)
        if wydanie is None:
            raise HTTPException(status_code=404, detail="Wydanie not found")
    if len(set(uzytkownik.idAudiobukow)) != len(uzytkownik.idAudiobukow):
        raise HTTPException(status_code=404, detail="Audiobuk sie powtarza")
    for id_audiobuk in uzytkownik.idAudiobukow:
        audiobuk = crud.get_audiobuk_by_id(db, id_audiobuk)
        if audiobuk is None:
            raise HTTPException(status_code=404, detail="Audiobuk not found")
    return crud.create_uzytkownik(db=db, uzytkownik=uzytkownik)


@app.get("/uzytkownik/", response_model=list[schemas.Uzytkownik])
def read_uzytkownik(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    uzytkownicy = crud.get_uzytkownicy(db, skip=skip, limit=limit)
    return uzytkownicy


@app.get("/uzytkownik/{id_uzytkownik}", response_model=schemas.Uzytkownik)
def read_uzytkownik_by_id(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik

@app.post("/uzytkownik/wydanie/{id_uzytkownik}/{id_wydanie}")
def dopisz_do_przeczytanych(id_uzytkownik: int, id_wydanie: int, db: Session = Depends(get_db)):
    db_uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    db_wydanie = crud.get_wydanie_by_id(db, id_wydanie=id_wydanie)

    if not db_uzytkownik:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")

    if not db_wydanie:
        raise HTTPException(status_code=404, detail="Wydanie not found")

    if db_wydanie in db_uzytkownik.przeczytal:
        raise HTTPException(status_code=404, detail="Wydanie juz przeczytane")
    crud.przypisz_wydanie_uzytkownikowi(db, id_uzytkownik=id_uzytkownik, id_wydanie=id_wydanie)
    return {"Success", True}


@app.post("/uzytkownik/audiobuk/{id_uzytkownik}/{id_audiobuk}")
def dopisz_do_przesluchanych(id_uzytkownik: int, id_audiobuk: int, db: Session = Depends(get_db)):
    db_uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    db_audiobuk = crud.get_audiobuk_by_id(db, id_audiobuk=id_audiobuk)

    if not db_uzytkownik:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")

    if not db_audiobuk:
        raise HTTPException(status_code=404, detail="Audiobuk not found")

    if db_audiobuk in db_uzytkownik.przesluchal:
        raise HTTPException(status_code=404, detail="Audiobuk juz przesluchany")
    crud.przypisz_audiobuk_uzytkownikowi(db, id_uzytkownik=id_uzytkownik, id_audiobuk=id_audiobuk)
    return {"Success", True}

@app.get("/uzytkownik/{id_uzytkownik}/przeczytane", response_model=schemas.UzytkownikWydanie)
def read_uzytkownik_przeczytane(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik

@app.get("/uzytkownik/{id_uzytkownik}/przesluchane", response_model=schemas.UzytkownikAudiobuk)
def read_uzytkownik_przesluchane(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik

@app.get("/uzytkownik/{id_uzytkownik}/wszystko", response_model=schemas.UzytkownikWszystko)
def read_uzytkownik_przesluchane(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik


@app.get("/audiobuk/", response_model=list[schemas.Audiobuk])
def read_audiobuk(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.get_audiobuki(db, skip=skip, limit=limit)


@app.get("/wydanie/", response_model=list[schemas.Wydanie])
def read_wydanie(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.get_wydania(db, skip=skip, limit=limit)



###################################################################################

# Dotyczace autora
@app.get("/autor/max/utwory", response_model=list[schemas.Autor])
def autor_z_najwiecej_utworow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.autor_z_najwiecej_utworow(db, skip=skip, limit=limit)

@app.get("/autor/{id_autora}/liczba/utwory", response_model=int)
def podaj_liczbe_utworow_autora(id_autora: int, db: Session = Depends(get_db)):
    autor = crud.get_autor_by_id(db, id_autora)
    if autor is None:
        raise HTTPException(status_code=404, detail="Autor not found")
    return crud.podaj_liczbe_utworow_autora(db, id_autora=id_autora)


@app.get("/autor/liczba/utworow", response_model=list[tuple[schemas.Autor, int]])
def autor_ile_utworow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.autor_ile_utworow(db, skip=skip, limit=limit))







# Dotyczace utworu - wydania
@app.get("/utwor/max/wydan", response_model=list[schemas.Utwor])
def utwor_z_najwiecej_wydan(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.utwor_z_najwiecej_wydan(db, skip=skip, limit=limit)

@app.get("/utwor/{id_utworu}/liczba/wydan", response_model=int)
def podaj_liczbe_wydan_utworu(id_utworu: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utworu)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.podaj_liczbe_wydan_utworu(db, id_utworu=id_utworu)

@app.get("/utwor/liczba/wydan", response_model=list[tuple[schemas.Utwor, int]])
def utwor_ile_wydan(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.utwor_ile_wydan(db, skip=skip, limit=limit))








# Dotyczace utworu - audiobuki
@app.get("/utwor/max/audiobukow", response_model=list[schemas.Utwor])
def utwor_z_najwiecej_audiobukow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.utwor_z_najwiecej_audiobukow(db, skip=skip, limit=limit)

@app.get("/utwor/{id_utworu}/liczba/audiobukow", response_model=int)
def podaj_liczbe_audiobukow_utworu(id_utworu: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utworu)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.podaj_liczbe_audiobukow_utworu(db, id_utworu=id_utworu)


@app.get("/utwor/liczba/audiobukow/", response_model=List[schemas.UtworCount])
def utwor_ile_audiobukow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.utwor_ile_audiobukow_2(db, skip=skip, limit=limit))

# @app.get("/utwor/liczba/audiobukow/", response_model=list[tuple[schemas.Utwor, int]])
# def utwor_ile_audiobukow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
#     return list(crud.utwor_ile_audiobukow_2(db, skip=skip, limit=limit))







# Dotyczace utworu - razem
@app.get("/utwor/max/razem", response_model=list[schemas.Utwor])
def utwor_z_najwiecej_razem(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.utwor_z_najwiecej_razem(db, skip=skip, limit=limit)

@app.get("/utwor/{id_utworu}/liczba/razem", response_model=int)
def podaj_liczbe_razem_utworu(id_utworu: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utworu)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.podaj_liczbe_razem_utworu(db, id_utworu=id_utworu)

@app.get("/utwor/liczba/razem/", response_model=list[tuple[schemas.Utwor, int]])
def utwor_ile_razem(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.utwor_ile_razem(db, skip=skip, limit=limit))




# Uzytkownik - wudanie

@app.get("/uzytkownik/max/wydanie", response_model=list[schemas.Uzytkownik])
def uzytkownik_z_najwiecej_wydan(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.uzytkownik_z_najwiecej_wydan(db, skip=skip, limit=limit)

@app.get("/uzytkownik/{id_uzytkownik}/liczba/wydan", response_model=int)
def podaj_liczbe_wydan_uzytkownika(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return crud.podaj_liczbe_wydan_uzytkownika(db, id_uzytkownik=id_uzytkownik)

@app.get("/uzytkownik/liczba/wydan/", response_model=list[tuple[schemas.Uzytkownik, int]])
def uzytkownik_ile_wydan(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.uzytkownik_ile_wydan(db, skip=skip, limit=limit))





# Uzytkownik - audiobuk

@app.get("/uzytkownik/max/audiobuk", response_model=list[schemas.Uzytkownik])
def uzytkownik_z_najwiecej_audiobukow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.uzytkownik_z_najwiecej_audiobukow(db, skip=skip, limit=limit)

@app.get("/uzytkownik/{id_uzytkownik}/liczba/audiobukow", response_model=int)
def podaj_liczbe_audiobukow_uzytkownika(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return crud.podaj_liczbe_audiobukow_uzytkownika(db, id_uzytkownik=id_uzytkownik)

@app.get("/uzytkownik/liczba/audiobukow/", response_model=list[tuple[schemas.Uzytkownik, int]])
def uzytkownik_ile_audiobukow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.uzytkownik_ile_audiobukow(db, skip=skip, limit=limit))





# Uzytkownik - razem

@app.get("/uzytkownik/max/razem", response_model=list[schemas.Uzytkownik])
def uzytkownik_z_najwiecej_razem(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.uzytkownik_z_najwiecej_razem(db, skip=skip, limit=limit)

@app.get("/uzytkownik/{id_uzytkownik}/liczba/razem", response_model=int)
def podaj_liczbe_razem_uzytkownik(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return crud.podaj_liczbe_razem_uzytkownik(db, id_uzytkownik=id_uzytkownik)

@app.get("/uzytkownik/liczba/razem/", response_model=list[tuple[schemas.Uzytkownik, int]])
def uzytkownik_ile_razem(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.uzytkownik_ile_razem(db, skip=skip, limit=limit))












