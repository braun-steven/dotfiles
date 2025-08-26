#!/usr/bin/env python3
# Source: https://github.com/ant-arctica/rofi-bookmarks
import sqlite3
import subprocess
from argparse import ArgumentParser
from configparser import ConfigParser
from os import environ
from pathlib import Path
from hashlib import sha256
from contextlib import closing, contextmanager, suppress
from tempfile import NamedTemporaryFile
from shutil import copyfile
import json
import time

cache_dir = (
    Path(environ.get("XDG_CACHE_HOME", Path.home() / ".cache")) / "rofi-bookmarks"
)
firefox_dir = Path.home() / ".mozilla/firefox"

# Cache settings
CACHE_DURATION = 24 * 60 * 60  # 24 hours in seconds


# b/c sqlite databases are locked by firefox we need copy them into a temporary location and connect to them there
@contextmanager
def temp_sqlite(path):
    with NamedTemporaryFile() as temp_loc:
        copyfile(path, temp_loc.name)
        with closing(sqlite3.connect(temp_loc.name)) as conn:
            yield conn


# go through all installs and chose first profile you find.
# better option would be to use install (which firefox) but that would add a dependency on cityhash
def default_profile_path():
    installs = ConfigParser()
    installs.read(firefox_dir / "installs.ini")
    for i in installs.values():
        with suppress(KeyError):
            return firefox_dir / i["Default"]
    raise Exception("could not find a default profile in installs.ini")


# get Path to profile directory from profil name
def path_from_name(name):
    profiles = ConfigParser()
    profiles.read(firefox_dir / "profiles.ini")
    for i in profiles.values():
        with suppress(KeyError):
            if i["Name"] == name:
                return firefox_dir / i["Path"]
    raise Exception("no profile with this name")


# add icon file to cache (in ~/.cache/rofi-bookmarks)
def cache_icon(icon):
    loc = cache_dir / sha256(icon).hexdigest()
    if not cache_dir.exists():
        cache_dir.mkdir(parents=True, exist_ok=True)
    if not loc.exists():
        loc.write_bytes(icon)
    return loc


def get_cache_key(profile_path, search_path, separator):
    """Generate a unique cache key based on profile path, search path, and separator"""
    key_data = f"{profile_path}:{':'.join(search_path)}:{separator}"
    return sha256(key_data.encode()).hexdigest()


def get_cache_file_path(cache_key):
    """Get the path to the cache file for the given cache key"""
    if not cache_dir.exists():
        cache_dir.mkdir(parents=True, exist_ok=True)
    return cache_dir / f"bookmarks_cache_{cache_key}.json"


def is_cache_valid(cache_file_path):
    """Check if the cache file exists and is less than 24 hours old"""
    if not cache_file_path.exists():
        return False

    cache_age = time.time() - cache_file_path.stat().st_mtime
    return cache_age < CACHE_DURATION


def load_cache(cache_file_path):
    """Load cached bookmarks from file"""
    try:
        with open(cache_file_path, 'r', encoding='utf-8') as f:
            return json.load(f)
    except (json.JSONDecodeError, IOError):
        return None


def save_cache(cache_file_path, bookmarks):
    """Save bookmarks to cache file"""
    try:
        with open(cache_file_path, 'w', encoding='utf-8') as f:
            json.dump(bookmarks, f, ensure_ascii=False, indent=2)
    except IOError:
        pass  # Silently fail if we can't write to cache


def generate_bookmarks_data(profile_loc, search_path=[], sep=" / "):
    """Generate bookmarks data from Firefox databases"""
    bookmarks = []

    with temp_sqlite(profile_loc / "places.sqlite") as places:
        conn_res = places.execute(
            """SELECT moz_bookmarks.id, moz_bookmarks.parent, moz_bookmarks.type, moz_bookmarks.title, moz_places.url
                                     FROM moz_bookmarks LEFT JOIN moz_places ON moz_bookmarks.fk=moz_places.id
                                  """
        ).fetchall()

    by_id = {i: (title, parent) for i, parent, _, title, _ in conn_res}

    def parent_generator(i):  # gives generator, where next is title of parent
        while i > 1:
            title, i = by_id[i]
            yield title

    with temp_sqlite(profile_loc / "favicons.sqlite") as favicons:
        for index, parent, type, title, url in conn_res:
            if type == 1:  # type one means bookmark
                path_arr = reversed(
                    list(parent_generator(index))
                )  # consumes beginning of path_arr and check if matches search_path (which implies path_arr is in a subfolder of seach_path)

                if all(
                    name == next(path_arr) for name in search_path
                ):  # this is safe, because next would only error if path_arr was a 'subpath' of search_path,
                    path = sep.join(
                        [x for x in path_arr if x is not None]
                    )  # but bookmarks are leaves ie don't have children
                    icon = favicons.execute(
                        f"""SELECT max(ic.data) FROM moz_pages_w_icons pg, moz_icons_to_pages rel, moz_icons ic
                                                                    WHERE pg.id = rel.page_id AND ic.id=rel.icon_id AND pg.page_url=?
                                             """,
                        (url,),
                    ).fetchone()[0]

                    bookmark_data = {
                        'path': path,
                        'url': url,
                        'icon': icon.hex() if icon else None
                    }
                    bookmarks.append(bookmark_data)

    return bookmarks


def print_bookmarks_from_data(bookmarks_data):
    """Print bookmarks in rofi format from cached/generated data"""
    for bookmark in bookmarks_data:
        path = bookmark['path']
        url = bookmark['url']
        icon_hex = bookmark['icon']

        if icon_hex:
            icon_data = bytes.fromhex(icon_hex)
            icon_path = cache_icon(icon_data)
            print(f"{path} ({url})\x00info\x1f{url}\x1ficon\x1f{icon_path}")
        else:
            print(f"{path} ({url})\x00info\x1f{url}")


# main function, finds all bookmaks inside of search_path and their corresponding icons and prints them in a rofi readable form
def write_rofi_input(profile_loc, search_path=[], sep=" / "):
    cache_key = get_cache_key(profile_loc, search_path, sep)
    cache_file_path = get_cache_file_path(cache_key)

    # Check if we have valid cached data
    if is_cache_valid(cache_file_path):
        cached_data = load_cache(cache_file_path)
        if cached_data is not None:
            print_bookmarks_from_data(cached_data)
            return

    # Generate fresh data if cache is invalid or missing
    bookmarks_data = generate_bookmarks_data(profile_loc, search_path, sep)

    # Save to cache
    save_cache(cache_file_path, bookmarks_data)

    # Print the results
    print_bookmarks_from_data(bookmarks_data)


if __name__ == "__main__":
    parser = ArgumentParser(
        description="generate list of bookmarks with icons for rofi"
    )
    parser.add_argument(
        "path", default="", nargs="?", help="restrict list to a bookmark folder"
    )
    parser.add_argument(
        "-s", "--separator", default=" / ", metavar="sep", help="seperator for paths"
    )
    parser.add_argument(
        "-p", "--profile", metavar="prof", help="firefox profile to use"
    )
    parser.add_argument(
        "--force-refresh", action="store_true", help="force refresh of cache"
    )
    (
        args,
        _,
    ) = (
        parser.parse_known_args()
    )  # rofi gives us selected entry as additional argument -> ignore (not useful)

    if environ.get("ROFI_RETV") == "1":
        prof = [] if args.profile is None else ["-P", args.profile]
        subprocess.Popen(
            ["firefox", "--new-window", environ["ROFI_INFO"]] + prof,
            close_fds=True,
            start_new_session=True,
            stdout=subprocess.DEVNULL,
        )
    else:
        search_path = [i for i in args.path.split("/") if i != ""]
        profile_path = (
            default_profile_path()
            if args.profile is None
            else path_from_name(args.profile)
        )

        print("\x00prompt\x1f ")  # change prompt

        # Handle force refresh option
        if args.force_refresh:
            cache_key = get_cache_key(profile_path, search_path, args.separator)
            cache_file_path = get_cache_file_path(cache_key)
            if cache_file_path.exists():
                cache_file_path.unlink()

        write_rofi_input(profile_path, search_path=search_path, sep=args.separator)
