# Mustangs Report Hub

A private R Shiny site for the Martinsville Mustangs. Players and coaches log in
to find each game's pitcher, hitter and umpire reports, TrackMan postgame stats
and the scouting library. It is built to work well on phones.

Keep this GitHub repo **private**. It holds team data.

## What each tab does

| Tab | What it shows | Who sees it |
| --- | --- | --- |
| Home | Welcome, the 5 latest games and the 5 newest library reports | Everyone |
| Schedule | The season. Pick a game to open its page with Pitcher, Hitter, Umpire and Other report tabs, a Download button and a preview for each file, and a TrackMan stats button when a TrackMan file exists for that date | Everyone. Coaches also see coaches-only files and the "Post a report to this game" card |
| TrackMan stats | Postgame pitcher and hitter reports built from each game's TrackMan CSV: stat line, pitch mix, movement, location, velocity, pitch log, hitter summary, pitches seen and batted balls | Everyone |
| Scouting library | Advance and opponent reports that aren't tied to one game. Filter by category, opponent or text | Everyone. Players only see items marked Team |
| Upload | Add a report to the library or delete one | Coaches only |

Players never see coaches-only files anywhere, not even in counts, and never
see upload or delete controls.

## Setup

1. Install R (4.1 or newer) and these packages:

   ```r
   install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "DT", "readr", "sodium", "rsconnect"))
   ```

2. Open a real TrackMan CSV from one of our games and look at the
   `PitcherTeam` and `BatterTeam` columns. Put every code our team uses in
   `TEAM_CODES` in `R/config.R`. Home and road files sometimes use different
   codes, so it can hold more than one:

   ```r
   TEAM_CODES <- c("MAR_MUS")
   ```

## Try it with demo data

```r
# from the app folder
system("Rscript scripts/make_demo_data.R")
shiny::runApp()
```

Log in as `coach` / `coach123` or `player` / `player123`. The demo makes two
fake TrackMan games, a fake schedule, demo report PDFs and one library item. All
names are fictional.

## Adding users

Passwords are stored as sodium hashes in `data/users.csv`. That file is in
`.gitignore` and must never be committed.

```r
source("scripts/add_user.R")
add_user("jsmith", "John Smith", "player", "a-strong-password")  # replaces an existing jsmith
add_user("coachb", "Coach Brown", "coach", "another-password")
remove_user("jsmith")

# Whole roster at once. roster.csv has columns username,name,role,password
add_roster("roster.csv")
```

Delete the plain-text `roster.csv` after importing it. It is also in
`.gitignore`. Role is `coach` or `player`. Usernames are not case sensitive.

## The schedule and game folders

Fill in `data/schedule.csv`, one row per game:

```
date,time,opponent,home_away,location,result
2027-06-04,7:00 PM,Opponent Name,Home,Hooker Field,
2027-06-06,6:30 PM,Other Team,Away,Their Park,
```

- `home_away` is Home or Away.
- Leave `result` blank until the game is played, then type something like `W 6-3`.
- Each game gets a `game_id` like `2027-06-04_OpponentName` (non-letters and
  non-digits are stripped from the opponent). A second game on the same date
  gets `_G2`. You can set your own `game_id` column if you want.

Then create a folder for every game:

```
Rscript scripts/make_game_folders.R
```

It prints a table of date, game and folder so you know where each file goes. It
never deletes anything, so it is safe to run again after adding games.

### File naming rule

Put report files in `reports/games/<game_id>/`. **The file name decides which
tab it shows under:**

| Name contains | Tab |
| --- | --- |
| `ump` (checked first) | Umpire |
| `pitch` | Pitcher |
| `hit` or `bat` | Hitter |
| anything else | Other |

Files in `reports/games/<game_id>/coaches/` are coaches-only.

When a coach posts a pitcher, hitter or umpire report through the site, it is
saved as `pitcher_report.<ext>`, `hitter_report.<ext>` or `umpire_report.<ext>`
and replaces the earlier file with that name in that folder, whatever its
extension. Files with other names are never deleted by the app.

PDF is the best format: it previews in the app and opens cleanly on phones.
HTML and TXT also preview, PNG and JPG show as images, and everything else can
be downloaded. On iPhones the inline PDF preview may show only the first page,
so players should tap Download to see the whole report.

## After each game

1. Copy the TrackMan CSV into `data/trackman/`.
2. Put the pitcher, hitter and umpire report PDFs into the game's folder.
3. Type the result into `data/schedule.csv`.
4. Deploy: `source("deploy.R")`

## Plugging in the Python reports

The pitcher, hitter, umpire and scouting reports are made by separate Python
scripts. The Shiny app never runs Python. It only shows the files the scripts
produce. `python/report_paths.py` (standard library only) gives the scripts the
right place to save:

```python
import sys
sys.path.insert(0, "path/to/Mustangs-Shiny-App/python")
from report_paths import (pitcher_report_path, hitter_report_path,
                          umpire_report_path, add_scouting_report)

fig.savefig(pitcher_report_path("2027-06-04", "OPP_ONE"))           # team can see it
fig.savefig(hitter_report_path("2027-06-12", "River City", game_num=2))  # 2nd game of a doubleheader
fig.savefig(umpire_report_path("2027-06-04", "OPP_ONE", coaches_only=True))

add_scouting_report("opp_three.pdf", title="OPP_THREE advance",
                    category="Advance scouting", opponent="OPP_THREE",
                    date="2027-06-07", visibility="Team")
```

- Each `*_report_path()` deletes the old report of that type in that folder
  (whatever its extension) and returns the new path, so there is only ever one.
- `add_scouting_report()` copies the file into `reports/files/` and adds its row
  to `reports/index.csv`. `category` must be one of the library categories in
  `R/config.R` and `visibility` must be `Team` or `Coaches`.
- **Spell the opponent exactly as in `schedule.csv`.** If the game is not in the
  schedule the helper prints a warning (the folder is still created, but the
  app will not show it).
- **Save as PDF when possible.** PDFs preview in the app and open cleanly on
  phones. HTML (for example Plotly) also previews, and PNG shows as an image.

`python/example_usage.py` is a working example that saves a matplotlib PDF with
`PdfPages(pitcher_report_path("2027-06-04", "OPP_ONE"))` and adds one library
item. Run `python python/test_report_paths.py` to check that Python and R still
build the same game ids.

With the Python reports plugged in, the after-game routine becomes:

1. Run the Python reports.
2. Copy the TrackMan CSV into `data/trackman/`.
3. Type the result into `data/schedule.csv`.
4. Deploy: `source("deploy.R")`

## Deploying to shinyapps.io

One time only: log in at shinyapps.io, open Account > Tokens, click Show and run
the `rsconnect::setAccountInfo(...)` line it gives you in the R console. Don't
save the token in the repo. After that, `source("deploy.R")` pushes the app,
the data and the reports.

**Hosting caveat.** shinyapps.io resets its disk whenever the app restarts or is
redeployed, so reports uploaded through the live site can disappear. Add
reports locally (in the folders above) and then deploy. If coaches need to
upload from the road, the upgrade path is storing reports in Google Drive or
Dropbox instead of the app folder. Also check shinyapps.io's active-hours limit
for your plan. A roster of 30+ people opening the site every day can use up the
free and starter tiers.

## Clearing the demo data before the season

Delete these, then add real users and the real schedule:

```
data/trackman/demo_20270604.csv
data/trackman/demo_20270606.csv
data/schedule.csv                (replace with the real schedule)
reports/games/2027-*             (the demo game folders)
reports/files/20270601120000_OPP_THREE_advance.pdf
reports/index.csv                (or remove the demo row)
```

Then remove the demo logins:

```r
source("scripts/add_user.R")
remove_user("coach")
remove_user("player")
```

## Project layout

```
app.R                     UI + server
R/config.R                settings (team codes, folders, colors, categories)
R/auth.R                  login helpers + login screen UI
R/data.R                  TrackMan loading, game list, scouting library index, secure previews
R/postgame.R              TrackMan stat calculations + ggplot charts
R/schedule.R              schedule loading + per-game report folders
scripts/add_user.R        add/remove logins, bulk roster import
scripts/make_game_folders.R   creates one folder per game from the schedule
scripts/make_demo_data.R  fake games, schedule, reports and demo logins
python/report_paths.py    helpers so the Python report scripts save into the right folders
python/example_usage.py   example report script (not a real report)
python/test_report_paths.py   checks Python and R game ids match
deploy.R                  pushes to shinyapps.io
data/schedule.csv         the season
data/users.csv            logins (hashed, never committed)
data/trackman/            one TrackMan CSV per game
reports/games/<game_id>/  pitcher/hitter/umpire reports per game
reports/files/            scouting library files
reports/index.csv         scouting library metadata
```

## Security notes

- Everything loads only after login. The login screen is an overlay, so every
  output on the server checks that someone is logged in.
- Report folders are never published as public web folders. Previews and
  downloads go through the logged-in session, so a copied link does not work
  for anyone else.

## Ideas for version 2

- A season-to-date tab for each player.
- Add the Stuff+ model as a column in the pitch mix table.
- PDF export of the TrackMan postgame report.
- A player view that shows their own reports first.
