from pydblite import Base
from manager import make_os_path
from datetime import datetime
from itertools import groupby


class DBError(Exception):
    pass


class DBManager:

    has_changes = False

    def __init__(self):
        self.credentials = self.open_db('credentials')
        self.topics = self.open_db('topics')
        self.now = datetime.now()

    def open_db(self, db_name):
        _db = Base(make_os_path(f'{db_name}.pdl'))
        if _db.exists():
            return _db.open()
        raise DBError(f'Couldn\'t connect to DB: {db_name}')

    def get_cookies(self, tracker):
        return self.credentials(tracker=tracker)[0]['cookies']

    def get_auth_params(self, tracker):
        return self.credentials(tracker=tracker)[0]['auth_params']

    def get_topics(self):
        return groupby((self.topics('next_update') < self.now),
                       key=lambda x: x['tracker'])
