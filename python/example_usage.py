"""EXAMPLE ONLY. Shows how a report script saves into the app's folders.

This is not one of the real report scripts. It makes a throwaway matplotlib
pitcher report for the demo game on 2027-06-04 against OPP_ONE and adds one
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

from report_paths import add_scouting_report, pitcher_report_path  # noqa: E402

# 1. Pitcher report for one game. pitcher_report_path() removes the old
#    pitcher report in that game folder first, so there is only ever one.
out = pitcher_report_path("2027-06-04", "OPP_ONE")
with PdfPages(out) as pdf:
    fig, ax = plt.subplots(figsize=(8.5, 11))
    ax.set_title("EXAMPLE pitcher report: vs OPP_ONE, Jun 04")
    ax.bar(["Fastball", "Slider", "ChangeUp"], [53, 18, 28], color=["#D22D49", "#EEE716", "#1DBE3A"])
    ax.set_ylabel("Usage %")
    pdf.savefig(fig)
    plt.close(fig)
print(f"Saved {out}")

# 2. A scouting report that isn't tied to one game.
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
