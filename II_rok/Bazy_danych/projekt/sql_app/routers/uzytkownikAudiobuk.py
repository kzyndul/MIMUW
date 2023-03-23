from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/uzytkownik",
    tags=["uzytkownik - audiobuk"],
)


@router.get("/{id_uzytkownik}/audiobuk", response_model=schemas.UzytkownikAudiobuk, tags=["uzytkownik - audiobuk"])
def read_uzytkownik_przesluchane(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik




@router.get("/max/audiobuk", response_model=list[schemas.Uzytkownik], tags=["uzytkownik - audiobuk"])
def uzytkownik_z_najwiecej_audiobukow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.uzytkownik_z_najwiecej_audiobukow(db, skip=skip, limit=limit)

@router.get("/{id_uzytkownik}/liczba/audiobukow", response_model=int, tags=["uzytkownik - audiobuk"])
def podaj_liczbe_audiobukow_uzytkownika(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return crud.podaj_liczbe_audiobukow_uzytkownika(db, id_uzytkownik=id_uzytkownik)

@router.get("/liczba/audiobukow/", response_model=list[schemas.UzytkownikCount], tags=["uzytkownik - audiobuk"])
def uzytkownik_ile_audiobukow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.uzytkownik_ile_audiobukow(db, skip=skip, limit=limit))