import re
from functools import partial


def get_info_hash(raw):
    import hashlib
    hash = hashlib.sha1()
    raw = bdecode(raw)
    info_data = encode_dictionary(raw['info'])
    hash.update(info_data)
    return str(hash.hexdigest().upper())


def bencode(data):
    if isinstance(data, bytes):
        return encode_bytes(data)
    if isinstance(data, str):
        return encode_string(data)
    if isinstance(data, int):
        return encode_integer(data)
    if isinstance(data, list):
        return encode_list(data)
    if isinstance(data, dict):
        return encode_dictionary(data)
    raise TypeError


def encode_bytes(data):
    return str(len(data)).encode() + b':' + data


def encode_string(data):
    return encode_bytes(data.encode('utf-8'))


def encode_integer(data):
    return b'i' + str(data).encode() + b'e'


def encode_list(data):
    encoded = b'l'
    for item in data:
        encoded += bencode(item)
    encoded += b'e'
    return encoded


def encode_dictionary(data):
    encoded = b'd'
    items = list(data.items())
    items.sort()
    for (key, value) in items:
        encoded += bencode(key)
        encoded += bencode(value)
    encoded += b'e'
    return encoded


def tokenize(
        text,
        match=re.compile(b'([idel])|(\d+):|(-?\d+)').match):
    i = 0
    while i < len(text):
        m = match(text, i)
        s = m.group(m.lastindex)
        i = m.end()
        if m.lastindex == 2:
            yield b's'
            yield text[i:i + int(s)]
            i += int(s)
        else:
            yield s


def decode_item(next, token):
    if token == b'i':
        # integer: "i" value "e"
        data = int(next())
        if next() != b'e':
            raise ValueError
    elif token == b's':
        # string: "s" value (virtual tokens)
        data = next()
        # Strings in torrent file are defined as utf-8 encoded
        try:
            data = data.decode('utf-8')
        except UnicodeDecodeError:
            # The pieces field is a byte string,
            # and should be left as such.
            pass
    elif token == b'l' or token == b'd':
        # container: "l" (or "d") values "e"
        data = []
        tok = next()
        while tok != b'e':
            data.append(decode_item(next, tok))
            tok = next()
        if token == b'd':
            data = dict(list(zip(data[0::2], data[1::2])))
    else:
        raise ValueError
    return data


def bdecode(text):
    try:
        src = tokenize(text)
        data = decode_item(partial(next, src), next(src))
        for _ in src:  # look for more tokens
            raise SyntaxError("trailing junk")
    except (AttributeError, ValueError, StopIteration, TypeError) as e:
        raise SyntaxError("syntax error: %s" % e)
    return data
