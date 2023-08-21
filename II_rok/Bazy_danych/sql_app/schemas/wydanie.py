from datetime import date
from typing import Optional

from pydantic import BaseModel

class WydanieBase(BaseModel):
    ilosc_stron: Optional[int] = None
    jezyk_tlumaczenia: Optional[str] = None
    rok_wydania: Optional[date] = None

class WydanieStworz(WydanieBase):
    pass

class Wydanie(WydanieBase):
    id: int
    id_utwor: int

    class Config:
        orm_mode = True