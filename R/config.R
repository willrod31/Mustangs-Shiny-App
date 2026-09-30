# Settings for the Mustangs Report Hub.
# Every path is relative to the app folder so it works on shinyapps.io.

APP_TITLE <- "Mustangs Report Hub"

# How our team shows up in TrackMan's PitcherTeam / BatterTeam columns.
# Home and road files sometimes use different codes, so this is a vector.
# TODO: open a real TrackMan CSV from this season, check the PitcherTeam and
# BatterTeam values for our side and update this list to match.
TEAM_CODES <- c("MAR_MUS")

# Folders and files
TRACKMAN_DIR <- "data/trackman"
SCHEDULE_FILE <- "data/schedule.csv"
USERS_FILE <- "data/users.csv"
GAMES_DIR <- "reports/games"
REPORTS_DIR <- "reports/files"
REPORTS_IDX <- "reports/index.csv"

# Colors
COLOR_PRIMARY <- "#7A1F2B"   # maroon
COLOR_SECONDARY <- "#1F2A44" # navy

# Scouting library categories
REPORT_CATEGORIES <- c(
  "Advance scouting",
  "Opponent hitters",
  "Opponent pitchers",
  "Postgame (written)",
  "Player development",
  "Other"
)

# Report types on a game page, in display order
GAME_REPORT_TYPES <- c("Pitcher", "Hitter", "Umpire", "Other")

# Strike zone in feet, catcher view
ZONE <- list(x = c(-0.83, 0.83), z = c(1.5, 3.5))

# Colors for TrackMan TaggedPitchType values
PITCH_COLORS <- c(
  Fastball = "#D22D49",
  FourSeamFastBall = "#D22D49",
  Sinker = "#FE9D00",
  TwoSeamFastBall = "#FE9D00",
  Cutter = "#933F2C",
  Slider = "#EEE716",
  Sweeper = "#DDB33A",
  Curveball = "#00D1ED",
  ChangeUp = "#1DBE3A",
  Changeup = "#1DBE3A",
  Splitter = "#3BACAC",
  Knuckleball = "#867A08",
  Other = "#9C8975",
  Undefined = "#9C8975"
)
