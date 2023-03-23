from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db

router = APIRouter(
    prefix="/autor",
    tags=["autor"],
)

@router.post("/", response_model=schemas.Autor)
def create_autor(autor: schemas.AutorStworz, db: Session = Depends(get_db)):
    # temp = crud.get_autor_by_imie(db, autor=autor.autor)
    # if temp is not None:
    #     return temp
    return crud.create_autor(db=db, autor=autor)


@router.get("/", response_model=list[schemas.Autor])
def read_autor(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    autorzy = crud.get_autorzy(db, skip=skip, limit=limit)
    return autorzy


@router.get("/{id_autora}", response_model=schemas.AutorSchema)
def read_autor_by_id(id_autora: int, db: Session = Depends(get_db)):
    autor = crud.get_autor_by_id(db, id_autora)
    if autor is None:
        raise HTTPException(status_code=404, detail="Autor not found")
    return autor


@router.get("/by/{imie}", response_model=list[schemas.AutorSchema])
def read_autor_by_id(imie: str, db: Session = Depends(get_db)):
    autor = crud.get_autor_by_imie(db, imie)
    if autor is None:
        raise HTTPException(status_code=404, detail="Autor not found")
    if not autor:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return autor



@router.get("/max/utwory", response_model=list[schemas.Autor], tags=["autor - utwor"],)
def autor_z_najwiecej_utworow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.autor_z_najwiecej_utworow(db, skip=skip, limit=limit)

@router.get("/{id_autora}/liczba/utwory", response_model=int, tags=["autor - utwor"])
def podaj_liczbe_utworow_autora(id_autora: int, db: Session = Depends(get_db)):
    autor = crud.get_autor_by_id(db, id_autora)
    if autor is None:
        raise HTTPException(status_code=404, detail="Autor not found")
    return crud.podaj_liczbe_utworow_autora(db, id_autora=id_autora)


@router.get("/liczba/utworow", response_model=list[schemas.AutorCount], tags=["autor - utwor"])
def autor_ile_utworow(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.autor_ile_utworow(db, skip=skip, limit=limit)