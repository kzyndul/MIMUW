from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/wydanie",
    tags=["wydanie"],
)


@router.get("/", response_model=list[schemas.Wydanie])
def read_wydanie(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.get_wydania(db, skip=skip, limit=limit)


@router.post("/{id_utwor}/", response_model=schemas.Wydanie)
def create_wydanie_for_utwor(
        id_utwor: int, wydanie: schemas.WydanieStworz, db: Session = Depends(get_db)
):
    db_utwor = crud.get_utwor_by_id(db, id=id_utwor)
    if db_utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.create_wydanie(db=db, wydanie=wydanie, id_utwor=id_utwor)


@router.post("/{id_uzytkownik}/{id_wydanie}")
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