from fastapi import  FastAPI

from . import models
from .database import create_tables

from .routers import autor, utwor, uzytkownik, audiobuk, wydanie, uzytkownikAudiobuk, uzytkownikRazem, uzytkownikWydanie, \
    utworWydanie, utworRazem, utworAudiobuk


create_tables(models)

app = FastAPI()

app.include_router(autor.router)
app.include_router(utwor.router)
app.include_router(utworWydanie.router)
app.include_router(utworRazem.router)
app.include_router(utworAudiobuk.router)
app.include_router(audiobuk.router)
app.include_router(wydanie.router)
app.include_router(uzytkownik.router)
app.include_router(uzytkownikAudiobuk.router)
app.include_router(uzytkownikRazem.router)
app.include_router(uzytkownikWydanie.router)
