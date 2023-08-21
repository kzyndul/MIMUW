from pydantic import BaseModel

class AutorBase(BaseModel):
    autor: str

class Autor(AutorBase):
    id: int

    class Config:
        orm_mode = True


class AutorStworz(AutorBase):
    pass

