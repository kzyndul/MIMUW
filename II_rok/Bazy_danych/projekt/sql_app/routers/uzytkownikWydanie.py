from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/uzytkownik",
    tags=["uzytkownik - wydanie"],
)


@router.get("/{id_uzytkownik}/wydanie", response_model=schemas.UzytkownikWydanie, tags=["uzytkownik - wydanie"])
def read_uzytkownik_przeczytane(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik

@router.get("/max/wydanie", response_model=list[schemas.Uzytkownik], tags=["uzytkownik - wydanie"])
def uzytkownik_z_najwiecej_wydan(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.uzytkownik_z_najwiecej_wydan(db, skip=skip, limit=limit)

@router.get("/{id_uzytkownik}/liczba/wydan", response_model=int, tags=["uzytkownik - wydanie"])
def podaj_liczbe_wydan_uzytkownika(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return crud.podaj_liczbe_wydan_uzytkownika(db, id_uzytkownik=id_uzytkownik)

@router.get("/liczba/wydan/", response_model=list[schemas.UzytkownikCount], tags=["uzytkownik - wydanie"])
def uzytkownik_ile_wydan(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.uzytkownik_ile_wydan(db, skip=skip, limit=limit))