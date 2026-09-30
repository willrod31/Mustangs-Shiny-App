"""Helpers so the Python report scripts save straight into the Shiny app's folders.

Standard library only. The Shiny app never runs Python. It only shows the files
these helpers put in place:

    from report_paths import box_score_path, pitcher_report_path, add_scouting_report

    fig.savefig(box_score_path("2027-06-04", "OPP_ONE"))                 # whole team
    fig.savefig(pitcher_report_path("2027-06-04", "OPP_ONE"))            # coaches only
    fig.savefig(pitcher_report_path("2027-06-04", "OPP_ONE", player="Hollis, Jace"))
    add_scouting_report("opp_three.pdf", "OPP_THREE advance", "Advance scouting",
                        opponent="OPP_THREE")

A game's files live in three places (same as the app):

    reports/games/<game_id>/                  whole team sees it (box scores)
    reports/games/<game_id>/coaches/          coaches and admin only
    reports/games/<game_id>/players/<slug>/   that one player, plus coaches and admin

Spell the opponent exactly as it is in data/schedule.csv.
"""

import csv
import datetime
import re
import shutil
from pathlib import Path

APP_DIR = Path(__file__).resolve().parent.parent  # repo root = Shiny app folder

INDEX_COLS = ["id", "title", "category", "opponent", "player", "date", "visibility",
              "file", "notes", "uploaded_by", "uploaded_at"]
VISIBILITY = ("Team", "Coaches")
_DATE_FORMATS = ("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y")
_FALLBACK_CATEGORIES = [
    "Advance scouting", "Opponent hitters", "Opponent pitchers",
    "Postgame (written)", "Player development", "Other",
]


def _iso_date(date):
    """Returns YYYY-MM-DD for a date, datetime or a string in a schedule format."""
    if isinstance(date, (datetime.date, datetime.datetime)):
        return date.strftime("%Y-%m-%d")
    text = str(date).strip()
    for fmt in _DATE_FORMATS:
        try:
            return datetime.datetime.strptime(text, fmt).strftime("%Y-%m-%d")
        except ValueError:
            pass
    raise ValueError(f"Can't read the date {date!r}. Use YYYY-MM-DD.")


def game_id(date, opponent, game_num=1):
    """Must match make_game_ids() in R/schedule.R exactly."""
    gid = f"{_iso_date(date)}_{re.sub(r'[^A-Za-z0-9]+', '', opponent)}"
    return gid + (f"_G{game_num}" if game_num > 1 else "")


def report_categories():
    """REPORT_CATEGORIES read from R/config.R so the two never drift apart."""
    config = APP_DIR / "R" / "config.R"
    try:
        text = config.read_text(encoding="utf-8")
        block = re.search(r"REPORT_CATEGORIES\s*<-\s*c\((.*?)\)\s*\n", text, re.S).group(1)
        cats = re.findall(r'"([^"]*)"', block)
        if cats:
            return cats
    except (OSError, AttributeError):
        pass
    return list(_FALLBACK_CATEGORIES)


def schedule_game_ids():
    """Every game_id in data/schedule.csv, using the same rule as the app."""
    path = APP_DIR / "data" / "schedule.csv"
    if not path.exists():
        return set()
    ids = set()
    games_on_date = {}
    with open(path, newline="", encoding="utf-8-sig") as f:
        for row in csv.DictReader(f):
            opponent = (row.get("opponent") or "").strip()
            try:
                date = _iso_date(row.get("date") or "")
            except ValueError:
                continue
            if not opponent:
                continue
            games_on_date[date] = games_on_date.get(date, 0) + 1
            own_id = (row.get("game_id") or "").strip()
            ids.add(own_id or game_id(date, opponent, games_on_date[date]))
    return ids


def player_slug(name):
    """Must match player_slug() in R/schedule.R. "Hollis, Jace" -> "hollis_jace"."""
    slug = re.sub(r"[^a-z0-9]+", "_", str(name).strip().lower())
    return slug.strip("_")


def _check_game(gid):
    known = schedule_game_ids()
    if known and gid not in known:
        print(f"WARNING: {gid} is not in data/schedule.csv. Check the opponent spelling "
              f"and the date. The folder is created anyway, but the app will not show "
              f"it until the schedule has a matching game.")


def _audience_folder(gid, audience):
    """audience is "team", "coaches" or a player name."""
    folder = APP_DIR / "reports" / "games" / gid
    if audience == "coaches":
        folder = folder / "coaches"
    elif audience != "team":
        slug = player_slug(audience)
        if not slug:
            raise ValueError(f"Can't make a folder name from the player {audience!r}.")
        folder = folder / "players" / slug
    return folder


def game_folder(date, opponent, game_num=1, coaches_only=False, player=None):
    """The game's folder for the whole team, coaches/ or players/<slug>/."""
    gid = game_id(date, opponent, game_num)
    _check_game(gid)
    audience = player if player else ("coaches" if coaches_only else "team")
    folder = _audience_folder(gid, audience)
    folder.mkdir(parents=True, exist_ok=True)
    return folder


def _fresh_report_path(date, opponent, game_num, stem, ext, audience):
    """Same "one per type" rule as post_game_file() in R/admin.R.

    Deletes the old <stem>.<any ext> and returns the new path. For the team or
    coaches it clears both the game root and coaches/, so there is one
    team-wide copy. For a player it clears only that player's folder.
    """
    gid = game_id(date, opponent, game_num)
    _check_game(gid)
    folder = _audience_folder(gid, audience)
    folder.mkdir(parents=True, exist_ok=True)
    if audience in ("team", "coaches"):
        clear = [_audience_folder(gid, "team"), _audience_folder(gid, "coaches")]
    else:
        clear = [folder]
    pattern = re.compile(rf"{re.escape(stem)}(\.[^.]*)?", re.I)
    for d in clear:
        if not d.is_dir():
            continue
        for old in d.iterdir():
            if old.is_file() and pattern.fullmatch(old.name):
                old.unlink()
    return folder / f"{stem}.{ext.lstrip('.')}"


def box_score_path(date, opponent, game_num=1, ext="pdf"):
    """Box score in the game root, so the whole team sees it."""
    return _fresh_report_path(date, opponent, game_num, "box_score", ext, "team")


def _staff_or_player(player):
    return player if player else "coaches"


def pitcher_report_path(date, opponent, game_num=1, ext="pdf", player=None):
    """player=None: the staff report in coaches/. player="Hollis, Jace": his own copy."""
    return _fresh_report_path(date, opponent, game_num, "pitcher_report", ext, _staff_or_player(player))


def hitter_report_path(date, opponent, game_num=1, ext="pdf", player=None):
    """player=None: the staff report in coaches/. player="Hollis, Jace": his own copy."""
    return _fresh_report_path(date, opponent, game_num, "hitter_report", ext, _staff_or_player(player))


def umpire_report_path(date, opponent, game_num=1, ext="pdf", player=None):
    """player=None: the staff report in coaches/. player="Hollis, Jace": his own copy."""
    return _fresh_report_path(date, opponent, game_num, "umpire_report", ext, _staff_or_player(player))


def _sanitize_name(name):
    """Same rule as sanitize_name() in R/schedule.R."""
    name = re.sub(r"[^A-Za-z0-9._-]+", "_", Path(name).name)
    name = re.sub(r"_+", "_", name)
    return name if re.sub(r"[._]", "", name) else "file"


def add_scouting_report(src_file, title, category, opponent="", player="",
                        date="", visibility="Team", notes=""):
    """Copy a finished scouting report into the library and add its index row.

    Naming a player makes a Team report visible only to that player and the
    staff. Leave player blank for reports the whole team should see.
    """
    src = Path(src_file)
    if not src.is_file():
        raise FileNotFoundError(f"Report file not found: {src}")
    if not str(title).strip():
        raise ValueError("title is required.")
    categories = report_categories()
    if category not in categories:
        raise ValueError(f"category must be one of {categories}, not {category!r}.")
    if visibility not in VISIBILITY:
        raise ValueError(f"visibility must be 'Team' or 'Coaches', not {visibility!r}.")
    if date:
        date = _iso_date(date)

    files_dir = APP_DIR / "reports" / "files"
    index_path = APP_DIR / "reports" / "index.csv"
    files_dir.mkdir(parents=True, exist_ok=True)

    existing = set()
    if index_path.exists():
        with open(index_path, newline="", encoding="utf-8-sig") as f:
            existing = {row.get("id", "") for row in csv.DictReader(f)}

    now = datetime.datetime.now()
    report_id = now.strftime("%Y%m%d%H%M%S")
    while report_id in existing:
        report_id = str(int(report_id) + 1)

    dest = files_dir / f"{report_id}_{_sanitize_name(src.name)}"
    shutil.copy2(src, dest)

    row = {
        "id": report_id, "title": str(title).strip(), "category": category,
        "opponent": opponent.strip(), "player": player.strip(), "date": date,
        "visibility": visibility, "file": dest.name, "notes": notes.strip(),
        "uploaded_by": "python", "uploaded_at": now.strftime("%Y-%m-%d %H:%M:%S"),
    }
    new_file = not index_path.exists() or index_path.stat().st_size == 0
    if not new_file:
        # make sure the last row ends with a newline before appending
        with open(index_path, "rb") as f:
            f.seek(-1, 2)
            needs_newline = f.read(1) not in (b"\n", b"\r")
    with open(index_path, "a", newline="", encoding="utf-8") as f:
        if not new_file and needs_newline:
            f.write("\n")
        writer = csv.DictWriter(f, fieldnames=INDEX_COLS)
        if new_file:
            writer.writeheader()
        writer.writerow(row)
    return dest
