"""EXAMPLE ONLY. Shows how a report script saves into the app's folders.

This is not one of the real report scripts. For the demo game on 2027-06-04
against OPP_ONE it makes a throwaway matplotlib box score (whole team), a staff
pitcher report (coaches only) and one pitcher's own report, then adds one
scouting report to the library. Run it after scripts/make_demo_data.R:

    python python/example_usage.py

Needs matplotlib (pip install matplotlib).
"""

import tempfile
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
from matplotlib.backends.backend_pdf import PdfPages  # noqa: E402

from report_paths import (  # noqa: E402
    add_scouting_report,
    box_score_path,
    pitcher_report_path,
)


def usage_chart(path, title, usage):
    with PdfPages(path) as pdf:
        fig, ax = plt.subplots(figsize=(8.5, 11))
        ax.set_title(title)
        ax.bar(list(usage), list(usage.values()), color=["#D22D49", "#EEE716", "#1DBE3A"])
        ax.set_ylabel("Usage %")
        pdf.savefig(fig)
        plt.close(fig)
    print(f"Saved {path}")


# 1. Box score. Saved in the game folder itself, so the whole team sees it.
#    Each *_path() helper removes the old file of that type first, so there
#    is only ever one.
out = box_score_path("2027-06-04", "OPP_ONE")
with PdfPages(out) as pdf:
    fig, ax = plt.subplots(figsize=(8.5, 11))
    ax.axis("off")
    ax.text(0.5, 0.9, "EXAMPLE box score: vs OPP_ONE, Jun 04", ha="center", fontsize=18)
    ax.text(0.5, 0.8, "Mustangs 6, OPP_ONE 3", ha="center", fontsize=14)
    pdf.savefig(fig)
    plt.close(fig)
print(f"Saved {out}")

# 2. The staff pitcher report for the whole staff. With no player it goes to
#    coaches/, so only coaches and the admin see it.
usage_chart(pitcher_report_path("2027-06-04", "OPP_ONE"),
            "EXAMPLE staff pitcher report: vs OPP_ONE, Jun 04",
            {"Fastball": 53, "Slider": 18, "ChangeUp": 28})

# 3. One pitcher's own report. Use his name as TrackMan spells it (the
#    "Name in TrackMan" on his login). Only he and the coaches see it.
usage_chart(pitcher_report_path("2027-06-04", "OPP_ONE", player="Eli Carver"),
            "EXAMPLE pitcher report for Eli Carver: vs OPP_ONE, Jun 04",
            {"Fastball": 58, "Slider": 25, "ChangeUp": 17})

# 4. A scouting report that isn't tied to one game.
with tempfile.TemporaryDirectory() as tmp:
    src = Path(tmp) / "OPP_FOUR hitters.pdf"
    fig, ax = plt.subplots(figsize=(8.5, 11))
    ax.axis("off")
    ax.text(0.5, 0.9, "EXAMPLE: OPP_FOUR hitters", ha="center", fontsize=20)
    fig.savefig(src)
    plt.close(fig)
    dest = add_scouting_report(
        src,
        title="OPP_FOUR hitters (example)",
        category="Opponent hitters",
        opponent="OPP_FOUR",
        date="2027-06-09",
        visibility="Team",
        notes="Made by python/example_usage.py",
    )
print(f"Added {dest}")
