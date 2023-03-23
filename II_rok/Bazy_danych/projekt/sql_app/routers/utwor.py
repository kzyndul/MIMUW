from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/utwor",
    tags=["utwor"],
)

@router.post("/", response_model=schemas.Utwor)
def create_utwor(utwor: schemas.UtworStworz, db: Session = Depends(get_db)):
    if len(set(utwor.idAutorow)) != len(utwor.idAutorow):
        raise HTTPException(status_code=404, detail="Autor sie powtarza")
    for id_autora in utwor.idAutorow:
        autor = crud.get_autor_by_id(db, id_autora)
        if autor is None:
            raise HTTPException(status_code=404, detail="Autor not found")
    return crud.create_utwor(db=db, utwor=utwor)

@router.get("/", response_model=list[schemas.Utwor])
def read_utwor(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    utwory = crud.get_utwory(db, skip=skip, limit=limit)
    return utwory

@router.get("/{id_utwor}", response_model=schemas.UtworAutorzy)
def read_utwor_by_id(id_utwor: int, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_id(db, id=id_utwor)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return utwor


@router.get("/tytul/{tytul}", response_model=list[schemas.UtworWszystko])
def read_utwor_by_tytul(tytul: str, db: Session = Depends(get_db)):
    utwor = crud.get_utwor_by_tytul(db, tytul)
    if utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    if not utwor:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return utwor










@router.post("/przypisz/{id_utwor}/{id_autora}")
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

