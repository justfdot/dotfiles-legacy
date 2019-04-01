import re
import requests
import socks
import socket
import manager
import hashlib
from bencoding import bdecode, bencode
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

    def get_web_page(self):
        return BeautifulSoup(
            self.session.get(f"{self.PAGE_URL}{self.topic['id']}").text,
            'html.parser')

    def get_info_hash(self):
        torrent_bytes = self.session.get(
            f"{self.DOWNLOAD_URL}{self.topic['id']}")
        if not torrent_bytes.ok:
            manager.event_log(
                f"Couldn\t get torrent file. Skipping {self.topic['title']}")

        torrent_bytes = torrent_bytes.content
        _decoded = bdecode(torrent_bytes)[b'info']
        info_hash = hashlib.sha1(bencode(_decoded)).hexdigest().upper()

        if not info_hash:
            manager.event_log((
                'Couldn\t calculate torrent hash.'
                f"Skipping {self.topic['title']}"))
            return

        if info_hash == self.topic['info_hash']:
            manager.event_log(
                f"Hashes are equal. Skipping {self.topic['title']}",
                suppress_notify=True)
            return

        manager.update_file(self.topic, torrent_bytes)
        return info_hash

    def try_get_datetime(self, web_page):
        try:
            return self.get_datetime(web_page)
        except AttributeError:
            raise TrackerError('Couldn\'t find datetime soup')
        except ValueError:
            raise TrackerError(
                    f'Couldn\'t parse datetime string')

    def get_episodes_range(self, web_page):
        try:
            return (self.EPISODES_RANGE_REGEX
                    .search(web_page.h1.a.string).groups())
        except AttributeError:
            raise TrackerError('Couldn\'t find episodes range')

    def correct_link_name(self, ep_range):
        return manager.rename_link(
            self.topic['link'],
            self.LINK_REGEX.sub(
                f' [{ep_range[0]}\u2215{ep_range[1]}] ',
                self.topic['link'].name))

    def stop_tracking(self, ep_range):
        if ep_range[1].isdigit() and int(ep_range[0]) == int(ep_range[1]):
            manager.event_log(f"Stop tracking: {self.topic['title']}")
            return True

    def make_schedule(self, web_page_update, this_week):
        delta = {'days': 6, 'hours': 22}
        if self.topic['air'] == 'daily':
            _weekday = web_page_update.isoweekday()
            if this_week >= 4 or _weekday >= 5:
                delta['days'] = 7 - _weekday
                this_week = 0
            delta['days'] = 0
            this_week += 1
        return (web_page_update + timedelta(**delta), this_week)

    def update(self, topic):

        self.topic = topic

        web_page = self.get_web_page()
        web_page_update = self.try_get_datetime(web_page)
        if web_page_update <= topic['last_update']:
            return

        info_hash = self.get_info_hash()
        if not info_hash:
            return

        ep_range = self.get_episodes_range(web_page)
        link_name = self.correct_link_name(ep_range)

        if self.stop_tracking(ep_range):
            self.db.topics.delete(topic)
        else:
            _nu, _tw = self.make_schedule(web_page_update, topic['this_week'])
            self.db.topics.update(
                topic,
                info_hash=info_hash,
                last_update=web_page_update,
                next_update=_nu,
                this_week=_tw,
                link=link_name)

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
