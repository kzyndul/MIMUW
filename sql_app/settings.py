from typing import Optional

from pydantic import BaseSettings


class Settings(BaseSettings):
    LOCALHOST = '127.0.0.1'
    SERVER_ADDRESS: str
    DB_HOST: str
    DB: str
    PG_USER: str
    PG_PASSWORD: str

    USED_FROM_LK_NETWORK: bool

    SSH_USERNAME: str

    SSH_PRIVATE_KEY_PATH: Optional[str] = None
    SSH_PRIVATE_KEY_PASSWORD: Optional[str] = None

    SSH_PASSWORD: Optional[str] = None

    PG_PORT = 5432
    SSH_PORT = 22

    class Config:
        env_file = '.env'
        case_sensitive = True


settings = Settings()
