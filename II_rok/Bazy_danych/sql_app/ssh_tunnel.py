# https://gist.github.com/danallison/7217d76d944ea4d8dabd0ba3041ebefc
from sshtunnel import SSHTunnelForwarder

from sql_app import settings


def get_credentials():
    credentials = {
        "ssh_username": settings.SSH_USERNAME,
    }

    if settings.SSH_PASSWORD is not None:
        credentials.update({
            "ssh_password": settings.SSH_PASSWORD
        })
    else:
        credentials.update({
            "ssh_pkey": settings.SSH_PRIVATE_KEY_PATH,
            "ssh_private_key_password": settings.SSH_PRIVATE_KEY_PASSWORD,
        })

    return credentials


tunnel = None

if not settings.USED_FROM_LK_NETWORK:
    tunnel = SSHTunnelForwarder(
        (settings.SERVER_ADDRESS, settings.SSH_PORT),
        remote_bind_address=(settings.DB_HOST, settings.PG_PORT),
        **get_credentials()
    )
    tunnel.start()
