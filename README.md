# Mustangs Analytics Hub

A private R Shiny site for the Martinsville Mustangs. Players and coaches log in
to find each game's box score and pitcher, hitter and umpire reports, TrackMan
postgame and season stats, and the scouting library. It is built to work well
on phones.

Keep this GitHub repo **private**. It holds team data.

## Roles

Every login in `data/users.csv` has one of three roles:

| Role | Can do |
| --- | --- |
| admin (you: `willrod31`) | Everything a coach can, plus manage logins and publish to the website |
| `coach` | See everything; add, edit and delete reports, box scores, TrackMan files, results and the schedule |
| `player` | See only his own reports and stats, plus team-wide files like box scores |

The owner login `willrod31` is created automatically the first time the app
starts (only its password hash is stored). It is always an admin, and nobody
else can change or remove it. The owner can change his own password in
**Add & manage > Logins** by saving `willrod31` with a new password.

All of the filtering happens on the server. A player's page never receives a
file, a link or a stat row that isn't his, not even in counts on the schedule.

## What each tab does

| Tab | What it shows | Who sees it |
| --- | --- | --- |
| Home | Welcome, the 5 latest games and the 5 newest library reports | Everyone |
| Schedule | The season. Pick a game to open its page with Box score, Pitcher, Hitter, Umpire and Other tabs, a Download button and a preview for each file, and a TrackMan stats button when there's TrackMan data for that game | Everyone. Players see whole-team files and their own. Coaches see everything, with a badge showing who each file is for, and an "Add files to this game" button |
| TrackMan stats | Postgame pitcher and hitter reports built from each game's TrackMan CSV: stat line, pitch mix, movement, location, velocity, pitch log, hitter summary, pitches seen and batted balls | Everyone. A player only sees games he played in and only "My pitching" / "My hitting" |
| Season stats | Running hitting and pitching lines for the whole team from every TrackMan file, with a game log and trend chart per player | Everyone. A player only sees his own row. Coaches can download a CSV |
| Scouting library | Advance and opponent reports that aren't tied to one game. Filter by category, opponent or text | Everyone. Players see Team items that name no player or name them |
| Add & manage | Add and manage everything from inside the app (see below) | Coaches and admin |

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

3. Start the app once. It adds the owner login `willrod31` to `data/users.csv`.

## Try it with demo data

```r
# from the app folder
system("Rscript scripts/make_demo_data.R")
shiny::runApp()
```

| Login | Password | Who |
| --- | --- | --- |
| `admin` | `admin123` | Admin |
| `coach` | `coach123` | Coach |
| `player` | `player123` | Jace Hollis, a hitter |
| `pitcher` | `pitcher123` | Eli Carver, a pitcher |

The demo makes two fake TrackMan games, a fake schedule, a box score, staff
reports and one report each for Jace and Eli in the first two game folders, and
two library items (a team advance report and Jace's development plan). All
names are fictional. Log in as each one to see what that role gets.

## Adding users

The easiest way is **Add & manage > Logins** (admin only). You can also use the
script. Passwords are stored as sodium hashes in `data/users.csv`. That file is
in `.gitignore` and must never be committed.

```r
source("scripts/add_user.R")
add_user("will", "Will Rodriguez", "admin", "a-strong-password")
add_user("coachb", "Coach Brown", "coach", "another-password")
add_user("jhollis", "Jace Hollis", "player", "a-strong-password", tm_name = "Hollis, Jace")
remove_user("jhollis")

# Whole roster at once. roster.csv has columns username,name,role,password
# and an optional tm_name column
add_roster("roster.csv")
```

Delete the plain-text `roster.csv` after importing it. It is also in
`.gitignore`. Usernames are not case sensitive. Saving an existing username
replaces that login, which is how you reset a password.

### Name in TrackMan

A player's reports and stats are found by the name TrackMan uses for him, which
is often `Last, First` (for example `Hollis, Jace`). Set it in the **Name in
TrackMan** field on the Logins sub-tab (or `tm_name` in the script). If it's
blank, the full name is used. Copy it exactly from the `Pitcher` or `Batter`
column of a TrackMan file. If the name isn't in any loaded TrackMan file yet,
the login still saves but you get a warning to check the spelling.

The same name decides his report folder: it is lowercased and every run of
non-letters becomes `_`, so `Hollis, Jace` uses `players/hollis_jace/`.

## Where game files live

Each game has a folder, `reports/games/<game_id>/`, and a file can go in one of
three places inside it:

```
reports/games/<game_id>/                  whole team sees it (box scores)
reports/games/<game_id>/coaches/          coaches and admin only
reports/games/<game_id>/players/<slug>/   that one player, plus coaches and admin
```

The `game_id` looks like `2027-06-04_OpponentName` (non-letters and non-digits
are stripped from the opponent). A second game on the same date gets `_G2`.

**The file name decides which tab it shows under:**

| Name contains | Tab |
| --- | --- |
| `box` (checked first) | Box score |
| `ump` | Umpire |
| `pitch` | Pitcher |
| `hit` or `bat` | Hitter |
| anything else | Other |

Box scores and pitcher, hitter and umpire reports added through the app are
saved as `box_score.<ext>`, `pitcher_report.<ext>`, `hitter_report.<ext>` or
`umpire_report.<ext>`. A new one replaces the old one for the same audience,
whatever its extension:

- Whole team or coaches only: the old copy in the game folder and in
  `coaches/` is removed, so there is one team-wide copy.
- One player: only that player's old copy is removed.

Files with other names are never deleted by the app except through Delete.

PDF is the best format: it previews in the app and opens cleanly on phones.
HTML and TXT also preview, PNG and JPG show as images, and everything else can
be downloaded. On iPhones the inline PDF preview may show only the first page,
so players should tap Download to see the whole report.

## The Add & manage tab

Coaches and the admin get this tab. At the top is a bar that tells you where
your changes go:

- **On your laptop (blue):** changes save to this computer. Publish when you're
  done. The admin gets a **Publish to website** button once rsconnect is set
  up (see Deploying below).
- **On the website (yellow):** changes made there can be wiped when the site
  restarts. Add things on the laptop and publish from there instead.

The sub-tabs:

- **Game files.** Pick the game, what it is (Box score, Pitcher, Hitter, Umpire
  or Other) and who it's for (Whole team, Coaches only or One player), choose
  the file, and type the result (for example `W 6-3`). Save posts the file and
  the result together. Box scores default to Whole team and everything else to
  Coaches only. The table on the right lists what's already posted for that
  game and who sees each file, with a Delete button.
- **Scouting library.** Add a report that isn't tied to one game (title,
  category, opponent, player, date, visibility, notes, file) or delete one.
  Naming a player makes a Whole team report visible only to that player and
  the coaches.
- **TrackMan files.** Upload one or more TrackMan CSVs and click Add to season.
  Each file's header is checked first; a file missing a needed column is
  skipped and named in an error. A file with the same name replaces the old
  one. The table lists every loaded file with its date, game and pitch count.
- **Schedule.** Add a game, double-click a Time, Location or Result cell to
  edit it (Enter saves), remove a game (its report files stay on disk), or
  replace the whole schedule from a CSV with at least `date`, `opponent` and
  `home_away` columns.
- **Logins** (admin only). Add a login or reset a password, set a player's
  Name in TrackMan, or remove a login. You can't remove your own.

## Season stats

Built from every TrackMan file for our team. Pick Hitting or Pitching, a date
range and a minimum PA / BF. Click a player to see his game log and a trend
chart (fastball velo by outing for pitchers, exit velo by game for hitters with
a dashed line at 95 mph).

- **Hitting:** G, PA, AB, H, 2B, 3B, HR, BB, K, HBP, AVG, OBP, SLG, OPS, K%, BB%,
  Whiff%, Chase%, Avg EV, Max EV, Hard% (share of balls in play at 95+ mph).
- **Pitching:** G, GS, IP, BF, H, R, HR, BB, K, HBP, R/9, WHIP, K%, BB%, K-BB%,
  Pitches, Strike%, Whiff%, CSW%, FB Velo, Max Velo.

**R/9 is runs allowed per nine innings, not ERA.** TrackMan records the runs
scored on each play but can't tell earned from unearned runs, so R/9 counts
every run. Outs come from TrackMan's `OutsOnPlay` (or 1 for an in-play out when
it's blank) plus strikeouts. All of these depend on the TrackMan operator's
tagging, so they won't always match the official book.

## After each game

1. **Add & manage > TrackMan files:** upload the game's TrackMan CSV.
2. **Add & manage > Game files:** pick the game and add
   - the box score (Whole team),
   - the staff pitcher, hitter and umpire reports (Coaches only),
   - each player's own report (One player),
   - and type the result.
3. **Publish:** click Publish to website (admin), or run `source("deploy.R")`.

## Plugging in the Python reports

The reports are made by separate Python scripts. The Shiny app never runs
Python. It only shows the files the scripts produce. `python/report_paths.py`
(standard library only) gives the scripts the right place to save:

```python
import sys
sys.path.insert(0, "path/to/Mustangs-Shiny-App/python")
from report_paths import (box_score_path, pitcher_report_path, hitter_report_path,
                          umpire_report_path, add_scouting_report)

fig.savefig(box_score_path("2027-06-04", "OPP_ONE"))                     # whole team
fig.savefig(pitcher_report_path("2027-06-04", "OPP_ONE"))                # coaches only
fig.savefig(pitcher_report_path("2027-06-04", "OPP_ONE", player="Hollis, Jace"))  # Jace + coaches
fig.savefig(hitter_report_path("2027-06-12", "River City", game_num=2))  # 2nd game of a doubleheader

add_scouting_report("opp_three.pdf", title="OPP_THREE advance",
                    category="Advance scouting", opponent="OPP_THREE",
                    date="2027-06-07", visibility="Team")
add_scouting_report("hollis_plan.pdf", title="Jace Hollis plan",
                    category="Player development", player="Jace Hollis")  # Jace + coaches
```

- `box_score_path()` saves in the game folder, so the whole team sees it.
- `pitcher_report_path()`, `hitter_report_path()` and `umpire_report_path()`
  save the staff report in `coaches/` when `player` is left out, and in
  `players/<slug>/` when you pass `player` (use his Name in TrackMan).
- Each one deletes the old report of that type first, with the same rule as
  the app: team and coaches copies replace each other, and a player's copy
  only replaces his own.
- `add_scouting_report()` copies the file into `reports/files/` and adds its row
  to `reports/index.csv`. `category` must be one of the library categories in
  `R/config.R` and `visibility` must be `Team` or `Coaches`. Naming a `player`
  makes a Team report visible only to him and the staff.
- **Spell the opponent exactly as in the schedule.** If the game is not in the
  schedule the helper prints a warning (the folder is still created, but the
  app will not show it).

`python/example_usage.py` is a working example that saves a box score, a staff
pitcher report and one pitcher's own report, and adds one library item. Run
`python python/test_report_paths.py` to check that Python and R still build the
same game ids and player folder names.

With the Python reports plugged in, the after-game routine becomes:

1. Upload the TrackMan CSV (Add & manage > TrackMan files).
2. Run the Python reports.
3. Type the result (Add & manage > Game files).
4. Publish.

## Deploying to shinyapps.io

One time only: log in at shinyapps.io, open Account > Tokens, click Show and run
the `rsconnect::setAccountInfo(...)` line it gives you in the R console. Don't
save the token in the repo. After that, the admin's **Publish to website**
button appears on the Add & manage tab, or you can run `source("deploy.R")`.
Either one pushes the app, the data and the reports.

**Hosting caveat.** shinyapps.io resets its disk whenever the app restarts or is
redeployed, so anything added through the live site can disappear. Add things
on the laptop and publish. Also check shinyapps.io's active-hours limit for
your plan. A roster of 30+ people opening the site every day can use up the
free and starter tiers.

## Clearing the demo data before the season

Delete these, then add real users and the real schedule:

```
data/trackman/demo_20270604.csv
data/trackman/demo_20270606.csv
data/schedule.csv                (replace with the real schedule)
reports/games/2027-*             (the demo game folders)
reports/files/20270601120000_OPP_THREE_advance.pdf
reports/files/20270602120000_Jace_Hollis_plan.pdf
reports/index.csv                (or remove the demo rows)
```

Then remove the demo logins (your `willrod31` owner login stays):

```r
source("scripts/add_user.R")
remove_user("admin")
remove_user("coach")
remove_user("player")
remove_user("pitcher")
```

## Project layout

```
app.R                     UI + server
R/config.R                settings (team codes, folders, colors, categories, report types)
R/admin.R                 roles, posting game files, schedule, TrackMan file and login helpers
R/auth.R                  login helpers + login screen UI
R/data.R                  TrackMan loading, game list, scouting library index, secure previews
R/manage.R                the Add & manage tab (UI + server)
R/postgame.R              TrackMan stat calculations + ggplot charts
R/schedule.R              schedule loading + per-game report folders and who sees them
R/season.R                the Season stats tab (hitting and pitching lines, game log, trend)
scripts/add_user.R        add/remove logins, bulk roster import
scripts/make_game_folders.R   creates one folder per game from the schedule
scripts/make_demo_data.R  fake games, schedule, reports and demo logins
python/report_paths.py    helpers so the Python report scripts save into the right folders
python/example_usage.py   example report script (not a real report)
python/test_report_paths.py   checks Python and R game ids and player folders match
deploy.R                  pushes to shinyapps.io (app.R, R, data, reports and www)
www/logo.png              the team logo (top bar, login screen, Home, phone home-screen icon)
www/favicon.png           the browser tab icon
www/field.jpg             Hooker Field photo (Home banner and login background)
data/schedule.csv         the season
data/users.csv            logins (hashed, never committed)
data/trackman/            one TrackMan CSV per game
reports/games/<game_id>/  box scores and reports per game (see Where game files live)
reports/files/            scouting library files
reports/index.csv         scouting library metadata
```

## Logo and colors

The logo lives in `www/logo.png` and the browser tab icon in `www/favicon.png`.
If the team logo ever changes, replace those two files (keep the names) and
publish. `www/field.jpg` is the Hooker Field photo behind the login screen and
at the top of Home; swap in another wide photo with the same name to change it. The team colors come from the logo and are set once in `BRAND` in
`R/config.R`; the pitch-type colors in `PITCH_COLORS` stay the standard ones.

## Security notes

- Everything loads only after login. The login screen is an overlay, so every
  output on the server checks that someone is logged in.
- Report folders are never published as public web folders. Previews and
  downloads go through the logged-in session, so a copied link does not work
  for anyone else.
- A player's TrackMan data is filtered once, on the server, before any tab
  uses it. Picking another player's name from the browser console shows
  nothing.
- The Add & manage tab is only added to the page for coaches and the admin,
  and every action in it checks the role again on the server, because inputs
  can be sent from the browser console.

## Ideas for version 3

- Add the Stuff+ model as a column in the pitch mix table.
- PDF export of the TrackMan postgame report.
- Store reports in Google Drive or Dropbox if coaches need to upload from the road.
