import os
import logging
from pathlib import Path


app_dir = Path(__file__).parent.resolve()
torrents_dir = Path('/home/justf/.rtorrent/watch/video/')
TORRENT_NAME_TEMPLATE = {
    'rutracker': '[rutracker.org].t{}.torrent',
    'kinozal': '[kinozal.tv]id{}.torrent'}


logging.basicConfig(
    filename=app_dir.joinpath('tvshows.log'),
    level=logging.INFO,
    format='[%(asctime)s] %(message)s',
    datefmt='%d.%m.%Y %H:%M')

logger = logging.getLogger('tvshows')


def make_filename(topic):
    return torrents_dir.joinpath(
        TORRENT_NAME_TEMPLATE[topic['tracker']].format(topic['id']))


def get_path(path_str):
    return Path(path_str)


def rename_link(link_path, new_link):
    new_link = link_path.with_name(new_link)
    link_path.rename(new_link)
    return Path(new_link)


def event_log(message, log_level='info', suppress_notify=False):
    getattr(logger, log_level)(message)
    if not suppress_notify:
        os.system(('notify-send -i refresh '
                   f'"TVSHOWS TRACKER [{log_level.upper()}]" "{message}"'))


def update_file(topic, torrent):
    make_filename(topic).write_bytes(torrent)
    event_log(f"Torrent updated: {topic['title']}")
