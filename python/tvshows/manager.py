import os
import logging


RUN_DIR = os.path.dirname(os.path.realpath(__file__))
TORRENT_DIR = '/home/justf/.rtorrent/watch/video/'
LINK_DIR = '/home/justf/video-linked'
TORRENT_NAME_TEMPLATE = {
    'rutracker': '[rutracker.org].t{}.torrent',
    'kinozal': '[kinozal.tv]id{}.torrent'}


def make_os_path(path, root=RUN_DIR):
    return os.path.join(root, path)


logging.basicConfig(
    filename=make_os_path('tvshows.log'),
    level=logging.INFO,
    format='[%(asctime)s] %(message)s',
    datefmt='%d.%m.%Y %H:%M')

logger = logging.getLogger('tvshows')


def make_filename(topic):
    return os.path.join(
        TORRENT_DIR,
        TORRENT_NAME_TEMPLATE[topic['tracker']].format(topic['topic_id']))


def rename_link(old_link, new_link):
    old_link = make_os_path(old_link, root=LINK_DIR)
    new_link = make_os_path(new_link, root=LINK_DIR)
    os.rename(old_link, new_link)


def event_log(log_level, message, suppress_notify=False):
    getattr(logger, log_level)(message)
    if not suppress_notify:
        os.system(('notify-send -i bell '
                   f'"TVSHOWS MANAGER {log_level.upper()}" "{message}"'))


def update_file(topic, torrent):
    with open(make_filename(topic), 'wb') as f:
        f.write(torrent)
    event_log('info', f"Torrent updated: {topic['title']}")
