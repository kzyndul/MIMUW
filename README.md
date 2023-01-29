# FastApi Template

Created for Databases class @ MIMUW

## Installation

1. Clone repo to desired location.
2. Create (only once) venv at desired location (change VENV_LOCATION, ex. "./venv"): 
    ```bash
    python3 -m venv VENV_LOCATION
    ```
3. Activate venv:
    ```bash
    python3 source VENV_LOCATION/bin/activate
    ```
4. Install requirements:
    ```bash
    pip install -r requirements.txt
    ```
5. Test run app: 
    ```bash
    uvicorn sql_app.main:app   
    ```

## Configuration

1. Set environmental variables in the `.env` file.
2. Mandatory fields:
   
   - `PG_USER` - your login to postgresql
   - `PG_PASSWORD` - your password to postgresql
   - `SSH_USERNAME` - your login to the server
   - [`SSH_PRIVATE_KEY_PATH` and `SSH_PRIVATE_KEY_PASSWORD`] or `SSH_PASSWORD`- credentials to the server

## Development

To run the application and reload on changes, you can add the `--reload` flag:
```bash
uvicorn --reload sql_app.main:app   
```

## File structure

- `crud.py` - database queries
- `database.py` - database connection
- `main.py` - app routing
- `models.py` - models for the database
- `schemas.py` - schemes for api
- `settings.py` - .env and app variables
- `ssh_tunnel.py` - connection to the ssh server


You can create a more complex file structure like (more at https://fastapi.tiangolo.com/tutorial/bigger-applications/):
```
.
├── app                  # "app" is a Python package
│   ├── __init__.py      # this file makes "app" a "Python package"
│   ├── main.py          # "main" module, e.g. import app.main
│   ├── dependencies.py  # "dependencies" module, e.g. import app.dependencies
│   └── routers          # "routers" is a "Python subpackage"
│   │   ├── __init__.py  # makes "routers" a "Python subpackage"
│   │   ├── items.py     # "items" submodule, e.g. import app.routers.items
│   │   └── users.py     # "users" submodule, e.g. import app.routers.users
│   └── internal         # "internal" is a "Python subpackage"
│       ├── __init__.py  # makes "internal" a "Python subpackage"
│       └── admin.py     # "admin" submodule, e.g. import app.internal.admin
```

but you need to manage python modules.

