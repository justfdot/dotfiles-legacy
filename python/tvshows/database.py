import tvshows.manager as manager
from tvshows.exceptions import TVShowsDBError
from pydblite import Base
from datetime import datetime
from itertools import groupby
from operator import itemgetter


class DBManager:

    has_changes = False
    list_template = '{:<7}  {:<9}  {:<27}  {:<27}  {}'
    topics_sort_fileds = ['id', 'tracker', 'last_update',
                          'next_update', 'title']

    def __init__(self):
        self.credentials = self.open_db('credentials')
        self.topics = self.open_db('topics')
        self.now = datetime.now()

    def open_db(self, db_name):
        _db = Base(manager.app_dir.joinpath('db', f'{db_name}.pdl'))
        if _db.exists():
            return _db.open()
        raise TVShowsDBError(f'Couldn\'t connect to DB: {db_name}')

    def get_cookies(self, tracker):
        return self.credentials(tracker=tracker)[0]['cookies']

    def get_auth_params(self, tracker):
        return self.credentials(tracker=tracker)[0]['auth_params']

    def get_topics(self, force):
        return groupby(
            (self.topics if force else self.topics('next_update') < self.now),
            key=lambda x: x['tracker'])

    def get_topics_sort_fields(self):
        return self.topics_sort_fileds

    def get_list_topics(self, sortby):
        return sorted(self.topics, key=itemgetter(sortby), reverse=True)

    def format_list_header(self):
        return self.list_template.format(
            *[i.upper() for i in self.topics_sort_fileds])

    def format_list_item(self, item):
        return self.list_template.format(
            item['id'],
            item['tracker'],
            item['last_update'].strftime('%d.%m.%y %H:%M, %A'),
            item['next_update'].strftime('%d.%m.%y %H:%M, %A'),
            item['title'])
