from datetime import date
from typing import Optional

from pydantic import BaseModel

class UzytkownikBase(BaseModel):
    imie: str
    nazwisko: str
    data_urodzenia: Optional[date]

class Uzytkownik(UzytkownikBase):
    id: int

    class Config:
        orm_mode = True


class UzytkownikStworz(UzytkownikBase):
    idWydan: list[Optional[int]] = None
    idAudiobukow: list[Optional[int]] = None
