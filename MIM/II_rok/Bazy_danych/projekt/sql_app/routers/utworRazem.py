from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/utwor",
    tags=["utwor - razem"],
)



@router.get("/{id_utwor}/razem/", response_model=schemas.UtworWszystko)
def read_utwor_with_all(id_utwor: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id=id_utwor)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return utwor






@router.get("/max/razem", response_model=list[schemas.Utwor])
def utwor_z_najwiecej_razem(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.utwor_z_najwiecej_razem(db, skip=skip, limit=limit)

@router.get("/{id_utworu}/liczba/razem", response_model=int)
def podaj_liczbe_razem_utworu(id_utworu: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id_utworu)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.podaj_liczbe_razem_utworu(db, id_utworu=id_utworu)

@router.get("/liczba/razem/", response_model=list[schemas.UtworCount])
def utwor_ile_razem(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.utwor_ile_razem(db, skip=skip, limit=limit)