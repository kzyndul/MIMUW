from sqlalchemy import Boolean, Column, ForeignKey, Integer, String, Date, Table
from sqlalchemy.orm import relationship

from .database import Base

class Autor_Utwor(Base):
    __tablename__ = 'autor_utwor'

    id_autor = Column(ForeignKey('autor.id'), primary_key=True)
    id_utwor = Column(ForeignKey('utwor.id'), primary_key=True)



class Autor(Base):
    __tablename__ = "autor"

    id = Column(Integer, primary_key=True, index=True)
    autor = Column(String, index=True, nullable=False)

    utwory = relationship('Utwor', secondary='autor_utwor', back_populates='autorzy')


class Utwor(Base):
    __tablename__ = "utwor"

    id = Column(Integer, primary_key=True, index=True)
    tytul = Column(String, index=True, nullable=False)

    autorzy = relationship('Autor', secondary='autor_utwor', back_populates='utwory')

    wydania = relationship("Wydanie", backref="utwor")
    audiobuki = relationship("Audiobuk", backref="utwor")



class Uzytkownik_Audiobuk(Base):
    __tablename__ = "uzytkownik_audiobuk"

    id_utwor = Column(ForeignKey("audiobuk.id"), primary_key=True)
    id_uzytkownik = Column(ForeignKey("uzytkownik.id"), primary_key=True)



class Uzytkownik_Wydanie(Base):
    __tablename__ = "uzytkownik_wydanie"

    id_utwor = Column(ForeignKey("wydanie.id"), primary_key=True)
    id_uzytkownik = Column(ForeignKey("uzytkownik.id"), primary_key=True)



class Wydanie(Base):
    __tablename__ = "wydanie"

    id = Column(Integer, primary_key=True, index=True)
    ilosc_stron = Column(Integer)
    jezyk_tlumaczenia = Column(String)
    rok_wydania = Column(Date, index=True)

    id_utwor = Column(Integer, ForeignKey("utwor.id"))

    przeczytany = relationship("Uzytkownik", secondary='uzytkownik_wydanie', back_populates="przeczytal")


class Audiobuk(Base):
    __tablename__ = "audiobuk"

    id = Column(Integer, primary_key=True, index=True)
    dlugosc = Column(Integer)
    jezyk = Column(String)

    id_utwor = Column(Integer, ForeignKey("utwor.id"))

    przesluchany = relationship("Uzytkownik", secondary='uzytkownik_audiobuk', back_populates="przesluchal")



class Uzytkownik(Base):
    __tablename__ = "uzytkownik"

    id = Column(Integer, primary_key=True, index=True)
    imie = Column(String, index=True, nullable=False)
    nazwisko = Column(String, index=True, nullable=False)
    hashed_password = Column(String)
    data_urodzenia = Column(Date)

    przesluchal = relationship("Audiobuk", secondary='uzytkownik_audiobuk', back_populates="przesluchany")
    przeczytal = relationship("Wydanie", secondary='uzytkownik_wydanie', back_populates="przeczytany") \

