"""Tests for report_paths.py. Run with plain python from the repo root:

    python python/test_report_paths.py

Needs Rscript on the PATH for the game_id check against R/schedule.R.
"""

import csv
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import report_paths as rp  # noqa: E402

REPO = Path(__file__).resolve().parent.parent

# (date, opponent) rows sent to make_game_ids() in R, and the matching Python calls
CASES = [
    ("2027-06-04", "OPP_ONE", 1),                       # normal game
    ("2027-06-10", "St. Mary's Blue-Sox (VA)", 1),      # spaces and punctuation
    ("2027-06-12", "River City", 1),                    # doubleheader game 1
    ("2027-06-12", "River City", 2),                    # doubleheader game 2 -> _G2
]


def r_game_ids():
    dates = ", ".join(f'"{d}"' for d, _, _ in CASES)
    opps = ", ".join('"' + o.replace('"', '\\"') + '"' for _, o, _ in CASES)
    expr = (f'source("R/schedule.R"); '
            f'cat(make_game_ids(c({dates}), c({opps})), sep = "\\n")')
    out = subprocess.run(["Rscript", "-e", expr], cwd=REPO, capture_output=True, text=True, check=True)
    return out.stdout.strip().splitlines()


def test_game_id_matches_r():
    expected = r_game_ids()
    got = [rp.game_id(d, o, n) for d, o, n in CASES]
    assert got == expected, f"Python {got} != R {expected}"
    assert got[1] == "2027-06-10_StMarysBlueSoxVA"
    assert got[3] == "2027-06-12_RiverCity_G2"


def with_temp_app(fn):
    """Runs fn against a throwaway copy of the app folders."""
    def wrapper():
        real = rp.APP_DIR
        with tempfile.TemporaryDirectory() as tmp:
            tmp = Path(tmp)
            (tmp / "R").mkdir()
            shutil.copy(REPO / "R" / "config.R", tmp / "R" / "config.R")
            (tmp / "data").mkdir()
            (tmp / "data" / "schedule.csv").write_text(
                "date,time,opponent,home_away,location,result\n"
                "2027-06-04,7:00 PM,OPP_ONE,Home,Hooker Field,W 6-3\n"
            )
            rp.APP_DIR = tmp
            try:
                fn(tmp)
            finally:
                rp.APP_DIR = real
    wrapper.__name__ = fn.__name__
    return wrapper


@with_temp_app
def test_one_report_per_type(tmp):
    folder = rp.game_folder("2027-06-04", "OPP_ONE")
    coaches = folder / "coaches"
    coaches.mkdir()
    (folder / "umpire_report.pdf").write_text("old team copy")
    (coaches / "umpire_report.txt").write_text("old coaches copy")
    (folder / "umpire_zone_notes.pdf").write_text("keep me")
    path = rp.umpire_report_path("2027-06-04", "OPP_ONE", ext="png")
    path.write_text("new")
    # the staff report goes to coaches/ and clears the team-wide copies
    assert path == coaches / "umpire_report.png", path
    assert sorted(p.name for p in folder.iterdir() if p.is_file()) == ["umpire_zone_notes.pdf"]
    assert sorted(p.name for p in coaches.iterdir()) == ["umpire_report.png"]

    box = rp.box_score_path("2027-06-04", "OPP_ONE")
    assert box == folder / "box_score.pdf", box


@with_temp_app
def test_player_reports(tmp):
    team = rp.hitter_report_path("2027-06-04", "OPP_ONE")
    team.write_text("staff")
    mine = rp.hitter_report_path("2027-06-04", "OPP_ONE", player="Hollis, Jace")
    assert mine == tmp / "reports" / "games" / "2027-06-04_OPPONE" / "players" / "hollis_jace" / "hitter_report.pdf"
    mine.write_text("v1")
    again = rp.hitter_report_path("2027-06-04", "OPP_ONE", ext="txt", player="Hollis, Jace")
    again.write_text("v2")
    # a player's new report replaces only his own old one
    assert sorted(p.name for p in mine.parent.iterdir()) == ["hitter_report.txt"]
    assert team.exists()


SLUG_CASES = ["Hollis, Jace", "O'Neil Jr., T.J.", " Smith  John "]


def test_player_slug_matches_r():
    names = ", ".join('"' + n.replace('"', '\\"') + '"' for n in SLUG_CASES)
    expr = f'source("R/schedule.R"); cat(player_slug(c({names})), sep = "\\n")'
    out = subprocess.run(["Rscript", "-e", expr], cwd=REPO, capture_output=True, text=True, check=True)
    expected = out.stdout.strip().splitlines()
    got = [rp.player_slug(n) for n in SLUG_CASES]
    assert got == expected, f"Python {got} != R {expected}"
    assert got == ["hollis_jace", "o_neil_jr_t_j", "smith_john"], got


@with_temp_app
def test_schedule_warning(tmp):
    import contextlib
    import io
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rp.game_folder("2027-06-04", "OPP_ONE")
    assert "WARNING" not in buf.getvalue()
    with contextlib.redirect_stdout(buf):
        folder = rp.game_folder("2027-06-04", "Opp One")
    assert "WARNING" in buf.getvalue()
    assert folder.exists()


@with_temp_app
def test_add_scouting_report(tmp):
    src = tmp / "My Report (v2).pdf"
    src.write_bytes(b"%PDF-1.4 test")
    dest = rp.add_scouting_report(src, "OPP_TWO advance", "Advance scouting",
                                  opponent="OPP_TWO", date="06/05/2027", visibility="Coaches")
    assert dest.exists() and dest.name.endswith("_My_Report_v2_.pdf"), dest.name
    rp.add_scouting_report(src, "Second", "Other")
    with open(tmp / "reports" / "index.csv", newline="") as f:
        rows = list(csv.DictReader(f))
    assert list(rows[0].keys()) == rp.INDEX_COLS
    assert len(rows) == 2 and rows[0]["id"] != rows[1]["id"]
    assert rows[0]["date"] == "2027-06-05" and rows[0]["uploaded_by"] == "python"

    for bad in [dict(category="Nope"), dict(category="Other", visibility="Everyone")]:
        try:
            rp.add_scouting_report(src, "Bad", **bad)
        except ValueError:
            pass
        else:
            raise AssertionError(f"expected ValueError for {bad}")


if __name__ == "__main__":
    tests = [test_game_id_matches_r, test_player_slug_matches_r, test_one_report_per_type,
             test_player_reports, test_schedule_warning, test_add_scouting_report]
    failed = 0
    for t in tests:
        try:
            t()
            print(f"PASS {t.__name__}")
        except Exception as e:  # noqa: BLE001
            failed += 1
            print(f"FAIL {t.__name__}: {e!r}")
    print(f"{len(tests) - failed}/{len(tests)} passed")
    sys.exit(1 if failed else 0)
