from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/uzytkownik",
    tags=["uzytkownik - razem"],
)



@router.get("/{id_uzytkownik}/razem", response_model=schemas.UzytkownikWszystko, tags=["uzytkownik - razem"])
def read_uzytkownik_przesluchane(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik




@router.get("/max/razem", response_model=list[schemas.Uzytkownik], tags=["uzytkownik - razem"])
def uzytkownik_z_najwiecej_razem(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.uzytkownik_z_najwiecej_razem(db, skip=skip, limit=limit)

@router.get("/{id_uzytkownik}/liczba/razem", response_model=int, tags=["uzytkownik - razem"])
def podaj_liczbe_razem_uzytkownik(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return crud.podaj_liczbe_razem_uzytkownik(db, id_uzytkownik=id_uzytkownik)

@router.get("/liczba/razem/", response_model=list[schemas.UzytkownikCount], tags=["uzytkownik - razem"])
def uzytkownik_ile_razem(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return list(crud.uzytkownik_ile_razem(db, skip=skip, limit=limit))