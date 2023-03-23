from typing import Optional

from pydantic import BaseModel

class AudiobukBase(BaseModel):
    dlugosc: Optional[int] = None
    jezyk: Optional[str] = None

class Audiobuk(AudiobukBase):
    id: int
    id_utwor: int

    class Config:
        orm_mode = True

class AudiobukStworz(AudiobukBase):
    pass
