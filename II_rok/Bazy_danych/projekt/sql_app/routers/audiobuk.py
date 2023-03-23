from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/audiobuk",
    tags=["audiobuk"],
)




@router.get("/", response_model=list[schemas.Audiobuk])
def read_audiobuk(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    return crud.get_audiobuki(db, skip=skip, limit=limit)


@router.post("/{id_utwor}/", response_model=schemas.Audiobuk)
def create_audiobuk_for_utwor(
        id_utwor: int, audiobuk: schemas.AudiobukStworz, db: Session = Depends(get_db)
):
    db_utwor = crud.get_utwor_by_id(db, id=id_utwor)
    if db_utwor is None:
        raise HTTPException(status_code=404, detail="Utwor not found")
    return crud.create_audiobuk(db=db, audiobuk=audiobuk, id_utwor=id_utwor)



@router.post("/{id_uzytkownik}/{id_audiobuk}")
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