#!/usr/bin/env python3
"""Create symlinks from the dotfiles repo into $HOME with configurable safeguards."""

import argparse
import logging
import os
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


logger = logging.getLogger(__name__)


@dataclass
class Stats:
    """Simple container tracking operations performed during a run.

    Attributes:
        created: Number of new symlinks successfully created.
        skipped: Number of entries skipped for any reason (already linked, unsupported, etc.).
        removed: Number of destination symlinks removed due to mismatch.
        backups: Number of conflicting files that were moved to ``.backup``.
        errors: Number of operations that failed irrecoverably.
    """

    created: int = 0
    skipped: int = 0
    removed: int = 0
    backups: int = 0
    errors: int = 0


def display_path(path: Path, home: Path) -> str:
    """Return a readable path, replacing the home prefix with ~ for logs.

    Args:
        path: The path to display.
        home: The user's home directory used for prefix replacement.

    Returns:
        A string suitable for log messages.
    """
    return str(path).replace(str(home), "~")


def ensure_parent(path: Path, *, dry_run: bool) -> None:
    """Create parent directories unless we are dry-running.

    Args:
        path: Destination parent directory.
        dry_run: When True, do not touch the filesystem.
    """
    if dry_run:
        return
    path.mkdir(parents=True, exist_ok=True)


def safe_backup(path: Path, *, dry_run: bool) -> None:
    """Move conflicting files aside, respecting dry-run mode.

    Args:
        path: Destination file to back up.
        dry_run: When True, log the intent without moving data.
    """
    backup_path = Path(f"{path}.backup")
    if dry_run:
        logger.info(f"Would move {display_path(path, HOME)} to {display_path(backup_path, HOME)}")
        return
    shutil.move(path, backup_path)
    logger.info(f"Moved {display_path(path, HOME)} to {display_path(backup_path, HOME)}")


def create_link(src: Path, dst: Path, args: argparse.Namespace, stats: Stats) -> None:
    """Link src -> dst, handling conflicts and summary bookkeeping.

    Args:
        src: Source file inside the dotfiles repository.
        dst: Target path inside the user's home directory.
        args: Parsed CLI arguments controlling dry-run/force/removal.
        stats: Mutable Stats struct used to track outcomes.
    """
    if not src.is_file():
        logger.debug(f"Skipping non-file '{src}'")
        stats.skipped += 1
        return

    if dst == HOME:
        logger.error(f"Refusing to overwrite home directory with '{src}'")
        stats.errors += 1
        return

    try:
        ensure_parent(dst.parent, dry_run=args.dry_run)
    except OSError as exc:
        logger.error(f"Failed to prepare destination '{dst}': {exc}")
        stats.errors += 1
        return

    src_resolved = src.resolve()

    if dst.is_symlink():
        # Destination is already a symlink; check whether it points to the right place.
        try:
            dst_target = dst.resolve()
        except OSError:
            dst_target = None

        if dst.exists() and dst_target == src_resolved:
            logger.debug(f"{display_path(dst, HOME)} already linked correctly, skipping")
            stats.skipped += 1
            return

        if not args.remove_symlinks:
            logger.warning(
                f"{display_path(dst, HOME)} is a mismatched symlink; rerun with --remove-symlinks"
            )
            stats.skipped += 1
            return

        if not args.force:
            logger.warning(f"Would remove {display_path(dst, HOME)} but --force not set")
            stats.skipped += 1
            return

        if args.dry_run:
            logger.info(f"Would remove {display_path(dst, HOME)}")
        else:
            dst.unlink()
            logger.info(f"Removed {display_path(dst, HOME)}")
        stats.removed += 1

    elif dst.exists():
        # A regular file conflicts with the destination; only move it aside when the
        # user explicitly forces the action.
        logger.warning(f"Destination {display_path(dst, HOME)} exists")
        if not args.force:
            logger.warning("Use --force to move existing files to a .backup suffix")
            stats.skipped += 1
            return
        safe_backup(dst, dry_run=args.dry_run)
        stats.backups += 1

    if args.dry_run:
        logger.info(f"Would link {display_path(src, HOME)} -> {display_path(dst, HOME)}")
        return

    try:
        os.symlink(src, dst)
        logger.info(f"{display_path(src, HOME)} -> {display_path(dst, HOME)}")
        stats.created += 1
    except OSError as exc:
        logger.error(f"Failed to create link for '{src}': {exc}")
        stats.errors += 1


def iter_children(path: Path) -> Iterable[Path]:
    """Safely list directory children, logging failures.

    Args:
        path: Directory to iterate.

    Returns:
        An iterable of immediate children; empty list on failure.
    """
    try:
        return list(path.iterdir())
    except OSError as exc:
        logger.error(f"Failed to scan '{path}': {exc}")
        return []


def link_config(path: Path, root: Path, args: argparse.Namespace, stats: Stats) -> None:
    """Recursively walk configuration directories and schedule links.

    Args:
        path: Current file or directory under the config root.
        root: Top-level configuration directory used to derive destinations.
        args: Parsed CLI arguments controlling behavior.
        stats: Mutable Stats struct used to track outcomes.
    """
    if path.name == ".DS_Store":
        return

    if path.is_dir():
        for child in iter_children(path):
            link_config(child, root, args, stats)
        return

    if path.is_symlink() and not path.exists():
        logger.warning(f"Skipping broken symlink '{path}'")
        stats.skipped += 1
        return

    if not path.exists():
        logger.warning(f"Skipping missing entry '{path}'")
        stats.skipped += 1
        return

    try:
        relative = path.relative_to(root)
    except ValueError:
        logger.error(f"Unable to derive destination for '{path}'")
        stats.errors += 1
        return

    # Mirror the relative layout from the repo under $HOME.
    destination = HOME / relative
    create_link(path, destination, args, stats)


def configure_logging(verbose: bool) -> None:
    """Initialize basic logging with optional debug output.

    Args:
        verbose: When True, enable DEBUG logging.
    """
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(level=level, format="%(levelname)s: %(message)s")


if __name__ == "__main__":
    HOME = Path(os.environ.get("HOME", str(Path.home()))).expanduser()

    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--remove-symlinks",
        "-r",
        action="store_true",
        help="Remove mismatched destination symlinks.",
    )
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output.")
    parser.add_argument(
        "--dotfiles",
        "-d",
        default=HOME / "dotfiles",
        help="Path to dotfiles directory.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print intended operations without modifying the filesystem.",
    )
    parser.add_argument(
        "--force",
        "-f",
        action="store_true",
        help="Allow removing or backing up conflicting files.",
    )
    ARGS = parser.parse_args()

    configure_logging(ARGS.verbose)

    config_dir = Path(ARGS.dotfiles).expanduser()
    if not config_dir.exists():
        raise SystemExit(f"Dotfiles directory '{config_dir}' not found")

    stats = Stats()

    for entry in iter_children(config_dir):
        if not entry.is_dir():
            logger.warning(f"Skipping non-directory '{entry}'")
            stats.skipped += 1
            continue

        if entry.name == ".git":
            continue

        for child in iter_children(entry):
            link_config(child, entry, ARGS, stats)

    logger.info(
        "Summary: created=%s skipped=%s backups=%s removed=%s errors=%s",
        stats.created,
        stats.skipped,
        stats.backups,
        stats.removed,
        stats.errors,
    )
