import re
import requests
import socks
import socket
import bittorrent
import manager
from datetime import datetime, timedelta
from bs4 import BeautifulSoup
from database import DBManager
from locale import setlocale, LC_TIME


socks.set_default_proxy(socks.SOCKS5, 'localhost', 9050)
socket.socket = socks.socksocket
setlocale(LC_TIME, 'ru_RU.UTF-8')


class TrackerError(Exception):
    pass


class Tracker:

    LINK_REGEX = re.compile(r'(?<=\b)(\s|\s\[.*\]\s)(?=\()')

    def __init__(self, name, db_obj: DBManager):
        self.NAME = name
        self.session = requests.Session()
        self.db = db_obj

        _cookies = self.db.get_cookies(self.NAME)
        if _cookies:
            self.session.cookies.update(_cookies)
        else:
            self.auth(self.db.get_auth_params(self.NAME))

    def get_id(self, url):
        topic_id = self.TOPIC_REGEX.search(url)
        if topic_id is not None:
            return topic_id.group(1)

    def auth(self, params):
        auth_response = self.session.post(self.LOGIN_URL, params)
        if auth_response.url.startswith(self.LOGIN_URL):
            raise TrackerError(
                f'Invalid login or password. [{self.NAME}]')
        else:
            self.db.credentials.update(
                tracker=self.NAME, cookies=self.session.cookies.get_dict())

    def get_torrent_raw(self, topic_id):
        raw = self.session.get(f'{self.DOWNLOAD_URL}{topic_id}')
        if not raw.ok:
            raise TrackerError(
                f'Failed to download torrent file. ({self.NAME}: {topic_id})')
        return raw.content

    def update(self, topic):
        response = self.session.get(f"{self.PAGE_URL}{topic['topic_id']}")
        soup = BeautifulSoup(response.text, 'html.parser')

        try:
            self.last_update = self.get_datetime(soup)
        except AttributeError:
            raise TrackerError('Couldn\'t find datetime soup')
        except ValueError:
            raise TrackerError(
                    f'Couldn\'t parse datetime string')

        if self.last_update <= topic['last_update']:
            return

        try:
            self.episodes_range = (self.EPISODES_RANGE_REGEX
                                   .search(soup.h1.a.string).groups())
        except AttributeError:
            raise TrackerError('Couldn\'t find episodes range')

        torrent_raw = self.get_torrent_raw(topic['topic_id'])
        torrent_raw_hash = bittorrent.get_info_hash(torrent_raw)
        if topic['info_hash'] == torrent_raw_hash:
            manager.event_log(
                'info',
                f"Hashes are equal. Skipping {topic['title']}",
                suppress_notify=True)
            return
        manager.update_file(topic, torrent_raw)

        _er_from, _er_to = self.episodes_range
        _new_link = self.LINK_REGEX.sub(
            f' [{_er_from}\u2215{_er_to}] ',
            topic['link'])
        manager.rename_link(topic['link'], _new_link)

        if _er_to.isdigit() and int(_er_from) == int(_er_to):
            self.db.topics.delete(topic)
            manager.event_log(
                'info', f"Tracking terminated: {topic['title']}")
        else:
            _td = {'days': 6, 'hours': 20}
            if topic['air'] == 'daily':
                _wd = self.lastupdate.isoweekday()
                _td['days'] = 0 if _wd < 5 else 7 - _wd

            self.db.topics.update(
                topic,
                info_hash=torrent_raw_hash,
                last_update=self.last_update,
                next_update=self.last_update + timedelta(**_td),
                link=_new_link)

        self.db.has_changes = True


class Rutracker(Tracker):

    LOGIN_URL = 'http://rutracker.org/forum/login.php'
    PAGE_URL = 'http://rutracker.org/forum/viewtopic.php?t='
    DOWNLOAD_URL = 'http://rutracker.org/forum/dl.php?t='
    TOPIC_REGEX = re.compile(r't=(\d+)$')
    EPISODES_RANGE_REGEX = re.compile(r'Серии:? \d+-(\d+) (?:из |\()(\d+|\?+)')

    def get_datetime(self, soup):
        return datetime.strptime(
            (soup.find('table', 'attach bordered med')
                 .find_all('tr', limit=2)[1]
                 .find('li').string),
            '%d-%b-%y %H:%M')


class Kinozal(Tracker):

    LOGIN_URL = 'http://kinozal.tv/takelogin.php'
    PAGE_URL = 'http://kinozal.tv/details.php?id='
    DOWNLOAD_URL = 'http://dl.kinozal.tv/download.php?id='
    TOPIC_REGEX = re.compile(r'id=(\d+)$')
    EPISODES_RANGE_REGEX = re.compile(r'\d+-(\d+) серии из (\d+|\?+)')
    LAST_UPDATE_REGEX = re.compile(
        r'^Торрент-файл обновлен\s+(\d+\s\w+\s\d+|\w+)\sв\s(\d+):(\d+)')

    def get_datetime(self, soup):
        _relative_days = {u'сегодня': 0, u'вчера': -1}
        datetime_soup = (soup.find('div', 'mn1_content')
                         .find('div', 'bx1 justify')
                         .find('b').string)
        _date, _hours, _minutes = (self.LAST_UPDATE_REGEX
                                   .search(datetime_soup).groups())
        if _date in _relative_days:
            return (self.db.now.replace(
                        hour=int(_hours), minute=int(_minutes))
                    + timedelta(days=_relative_days[_date]))
        else:
            return datetime.strptime(
                f'{_date} {_hours}:{_minutes}', '%d %B %Y %H:%M')
