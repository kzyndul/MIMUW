from typing import Optional

from pydantic import BaseModel

class UtworBase(BaseModel):
    tytul: str

class Utwor(UtworBase):
    id: int

    class Config:
        orm_mode = True

class UtworCount(BaseModel):
    Utwor: Utwor
    count: int







class UtworStworz(UtworBase):
    idAutorow: list[Optional[int]] = None


