from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/utwor",
    tags=["utwor - wydanie"],
)

@router.get("/{id_utwor}/wydanie/", response_model=schemas.UtworWydanie)
def read_utwor_with_wydania(id_utwor: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id=id_utwor)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return utwor


@router.get("/max/wydanie", response_model=list[schemas.Utwor])
def utwor_z_najwiecej_wydan(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.utwor_z_najwiecej_wydan(db, skip=skip, limit=limit)

@router.get("/{id_utworu}/liczba/wydanie", response_model=int)
def podaj_liczbe_wydan_utworu(id_utworu: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utworu)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.podaj_liczbe_wydan_utworu(db, id_utworu=id_utworu)

@router.get("/liczba/wydanie", response_model=list[schemas.UtworCount])
def utwor_ile_wydan(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.utwor_ile_wydan(db, skip=skip, limit=limit)