from pydantic import BaseModel

class AutorBase(BaseModel):
    autor: str

class Autor(AutorBase):
    id: int

    class Config:
        orm_mode = True

class AutorCount(BaseModel):
    Autor: Autor
    count: int

class AutorStworz(AutorBase):
    pass

