from fastapi import Depends, APIRouter, HTTPException
from .. import schemas, crud
from sqlalchemy.orm import Session
from ..database import get_db


router = APIRouter(
    prefix="/uzytkownik",
    tags=["uzytkownik"],
)

@router.post("/", response_model=schemas.Uzytkownik)
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


@router.get("/", response_model=list[schemas.Uzytkownik])
def read_uzytkownik(skip: int = 0, limit: int = 100, db: Session = Depends(get_db)):
    uzytkownicy = crud.get_uzytkownicy(db, skip=skip, limit=limit)
    return uzytkownicy


@router.get("/{id_uzytkownik}", response_model=schemas.Uzytkownik)
def read_uzytkownik_by_id(id_uzytkownik: int, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownicy_by_id(db, id_uzytkownik=id_uzytkownik)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik

@router.get("/imie/{imie}", response_model=list[schemas.Uzytkownik])
def get_uzytkownik_by_imie(imie: str, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownik_by_imie(db, imie)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    if not uzytkownik:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik

@router.get("/nazwisko/{nazwisko}", response_model=list[schemas.Uzytkownik])
def get_uzytkownik_by_imie(nazwisko: str, db: Session = Depends(get_db)):
    uzytkownik = crud.get_uzytkownik_by_nazwisko(db, nazwisko)
    if uzytkownik is None:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    if not uzytkownik:
        raise HTTPException(status_code=404, detail="Uzytkownik not found")
    return uzytkownik
