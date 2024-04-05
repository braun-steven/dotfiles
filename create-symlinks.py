#!/usr/bin/env python3

import os
import sys
import shutil
from pathlib import Path
import logging

import argparse


def create_link(entry: os.DirEntry):
    assert entry.is_file(), f"'{entry.path}' is not a file!"
    src = entry.path

    # Replace $HOME/dotfiles/configs/<CONFIG>/ with $HOME
    rplc = "/".join(src.split("/")[:6])
    dst = Path(src.replace(rplc, HOME))

    # Create parent directories if they don't exist
    dst.parent.mkdir(parents=True, exist_ok=True)

    # Check if dst already exists
    if dst.exists():
        # Check if dst is a symlink
        if os.path.islink(dst):
            # If dst links to src, all is good
            if Path(os.readlink(dst)) == Path(src):
                logger.debug(f"{dst} already linked correctly, skipping ...")
                return
            elif ARGS.remove_symlinks:
                # Remove symlinks if arg is set
                logger.info(f"Removing {dst}")
                os.remove(dst)
        else:
            # Not a symlink, but file exists
            logger.warning(f"Destination: {dst} already exists")
            logger.warning(f"Moving {dst} to {dst}.backup")
            shutil.move(dst, str(dst) + ".backup")


    # Create symlink
    os.symlink(src, dst)
    logger.info(f"{src} -> {dst}")



def link_config(entry: os.DirEntry):
    if entry.is_dir():
        for e in os.scandir(entry):
            link_config(e)
    else:
        create_link(entry)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--remove-symlinks", "-r", action="store_true", help="Remove all symlinks."
    )
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output.")
    ARGS = parser.parse_args()

    # Setup logging
    if ARGS.verbose:
        log_level = logging.DEBUG
    else:
        log_level = logging.INFO

    logging.basicConfig(level=log_level, format='%(levelname)s: %(message)s')
    logger = logging.getLogger(__name__)

    # Home path
    HOME = os.getenv("HOME")
    DOT_CONFIG = os.path.join(HOME, ".config")
    CONFIG_DIR = os.path.join(os.getcwd(), "configs")

    # Scan all dirs in "./configs/"
    for entry in os.scandir(CONFIG_DIR):
        assert entry.is_dir(), f"'{entry.path}' is not a directory!"
        for f in os.scandir(entry):

            # Skip .DS_store files on MacOS
            if f.path.endswith(".DS_Store"):
                continue

            link_config(f)
