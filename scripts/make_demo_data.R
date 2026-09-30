# Creates fake data so the app can be tested before the season.
# All names are fictional. Run from the app folder:
#   Rscript scripts/make_demo_data.R
#
# Creates:
#   data/trackman/demo_20270604.csv and demo_20270606.csv
#   data/schedule.csv (only if missing or already the demo schedule)
#   in the first two game folders: box_score.pdf (whole team), staff pitcher,
#   hitter and umpire reports in coaches/, and players/jace_hollis/ and
#   players/eli_carver/ reports
#   two scouting library PDFs (a team advance report and a player plan)
#   logins admin/admin123, coach/coach123, player/player123 (Jace Hollis,
#   a hitter) and pitcher/pitcher123 (Eli Carver, a pitcher)
#
# See the README for how to clear the demo data before the season.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source("R/config.R")
source("R/auth.R")
source("R/data.R")
source("R/schedule.R")

set.seed(2027)

# TrackMan games ------------------------------------------------------------

arsenal <- function(...) {
  a <- list(...)
  do.call(rbind, lapply(a, as.data.frame))
}

# type, usage weight, velo, spin, IVB, HB (RHP numbers, flipped for LHP)
ARSENALS <- list(
  power = arsenal(
    list(type = "Fastball", w = 0.55, velo = 92, spin = 2350, ivb = 17, hb = 9),
    list(type = "Slider", w = 0.25, velo = 83, spin = 2500, ivb = 2, hb = -6),
    list(type = "ChangeUp", w = 0.20, velo = 84, spin = 1700, ivb = 7, hb = 14)
  ),
  sinker = arsenal(
    list(type = "Sinker", w = 0.50, velo = 89, spin = 2150, ivb = 7, hb = 16),
    list(type = "Sweeper", w = 0.30, velo = 79, spin = 2600, ivb = 0, hb = -15),
    list(type = "Changeup", w = 0.20, velo = 81, spin = 1600, ivb = 5, hb = 13)
  ),
  crafty = arsenal(
    list(type = "Fastball", w = 0.45, velo = 87, spin = 2200, ivb = 15, hb = 7),
    list(type = "Cutter", w = 0.20, velo = 83, spin = 2400, ivb = 9, hb = -2),
    list(type = "Curveball", w = 0.20, velo = 74, spin = 2650, ivb = -12, hb = -8),
    list(type = "Splitter", w = 0.15, velo = 80, spin = 1300, ivb = 3, hb = 8)
  )
)

make_pitcher <- function(name, id, throws, style) {
  list(name = name, id = id, throws = throws, arsenal = ARSENALS[[style]])
}

make_lineup <- function(names, id_start) {
  sides <- sample(c("Right", "Left"), length(names), replace = TRUE, prob = c(0.6, 0.4))
  data.frame(name = names, id = id_start + seq_along(names), side = sides, stringsAsFactors = FALSE)
}

# Our names are "First Last" so the demo logins can use them as their
# Name in TrackMan. Real TrackMan files often use "Last, First".
OUR_PITCHERS <- list(
  make_pitcher("Eli Carver", 1001, "Right", "power"),
  make_pitcher("Mateo Cruz", 1002, "Left", "sinker"),
  make_pitcher("Owen Lind", 1003, "Right", "crafty")
)
OUR_LINEUP <- make_lineup(c(
  "Nico Reyes", "Jace Hollis", "Tyler Moss", "Drew Mabry", "Adrian Soto",
  "Logan Frey", "Carter Banks", "Micah Ward", "Dylan Ortiz"
), 2000)

opp_players <- function(tag) {
  pitchers <- list(
    make_pitcher(paste0("Stone, Kade (", tag, ")"), 3001, "Right", "power"),
    make_pitcher(paste0("Pratt, Cole (", tag, ")"), 3002, "Left", "crafty")
  )
  lineup <- make_lineup(paste0(c(
    "Hayes, Ryan", "Knox, Aaron", "Vega, Luis", "Holt, Mason", "Diaz, Evan",
    "Rowe, Chase", "Nash, Wyatt", "Lowe, Seth", "Kerr, Blake"
  ), " (", tag, ")"), 4000)
  list(pitchers = pitchers, lineup = lineup)
}

sim_pitch <- function(p, batter_side, balls, strikes) {
  a <- p$arsenal
  i <- sample(nrow(a), 1, prob = a$w)
  flip <- if (p$throws == "Left") -1 else 1
  type <- a$type[i]
  side <- rnorm(1, 0, 0.75)
  height <- rnorm(1, 2.45, 0.7)
  if (strikes == 2 && type != a$type[1]) height <- height - 0.35 # chase pitches low
  list(
    TaggedPitchType = type,
    RelSpeed = round(rnorm(1, a$velo[i], 1.1), 1),
    SpinRate = round(rnorm(1, a$spin[i], 70)),
    InducedVertBreak = round(rnorm(1, a$ivb[i], 1.8), 1),
    HorzBreak = round(flip * rnorm(1, a$hb[i], 1.8), 1),
    RelHeight = round(rnorm(1, 5.8, 0.08), 2),
    RelSide = round(flip * rnorm(1, 1.9, 0.08), 2),
    Extension = round(rnorm(1, 6.2, 0.15), 2),
    PlateLocSide = round(side, 2),
    PlateLocHeight = round(height, 2)
  )
}

sim_ball_in_play <- function() {
  ev <- round(max(45, rnorm(1, 86, 11)), 1)
  la <- round(rnorm(1, 12, 24), 1)
  dist <- round(max(5, if (la < 5) ev * 1.1 else ev * 3.6 * sin(pi * min(la, 45) / 90) * 1.25))
  p_hit <- if (ev >= 95 && la > 8 && la < 32) 0.6 else if (la > 45) 0.05 else 0.27
  if (ev >= 100 && la >= 22 && la <= 35 && dist > 360) {
    result <- "HomeRun"
  } else if (runif(1) < p_hit) {
    result <- sample(c("Single", "Double", "Triple"), 1, prob = c(0.72, 0.24, 0.04))
  } else {
    result <- sample(c("Out", "Error", "FieldersChoice", "Sacrifice"), 1, prob = c(0.93, 0.03, 0.03, 0.01))
  }
  hit_type <- if (la < 10) "GroundBall" else if (la < 25) "LineDrive" else if (la < 50) "FlyBall" else "Popup"
  # One out on every play the sim counts as an out (Out, Sacrifice,
  # FieldersChoice), so our pitchers' outs add up to 27 a game.
  outs_on_play <- as.numeric(result %in% c("Out", "Sacrifice", "FieldersChoice"))
  runs <- if (result == "HomeRun") {
    sample(1:2, 1)
  } else if (result %in% c("Single", "Double") && runif(1) < 0.3) {
    1
  } else {
    0
  }
  list(ExitSpeed = ev, Angle = la, Distance = dist, PlayResult = result, TaggedHitType = hit_type,
       OutsOnPlay = outs_on_play, RunsScored = runs)
}

sim_game <- function(date_str, home, away, our_side, opp, file) {
  rows <- list()
  pitch_no <- 0
  lineup_pos <- c(top = 1, bottom = 1)

  for (inning in 1:9) {
    for (half in c("Top", "Bottom")) {
      we_pitch <- (half == "Top") == (our_side == "home")
      if (we_pitch) {
        pitcher <- OUR_PITCHERS[[if (inning <= 5) 1 else if (inning <= 7) 2 else 3]]
        lineup <- opp$lineup
        pteam <- TEAM_CODES[1]
        bteam <- if (our_side == "home") away else home
      } else {
        pitcher <- opp$pitchers[[if (inning <= 6) 1 else 2]]
        lineup <- OUR_LINEUP
        pteam <- if (our_side == "home") away else home
        bteam <- TEAM_CODES[1]
      }
      key <- tolower(half)
      outs <- 0
      pa_of_inning <- 0
      # Every half inning ends on its third out, so outs add up to 27 a game
      while (outs < 3) {
        pa_of_inning <- pa_of_inning + 1
        b <- lineup[lineup_pos[key], ]
        lineup_pos[key] <- lineup_pos[key] %% 9 + 1
        balls <- 0
        strikes <- 0
        pitch_of_pa <- 0
        repeat {
          pitch_no <- pitch_no + 1
          pitch_of_pa <- pitch_of_pa + 1
          p <- sim_pitch(pitcher, b$side, balls, strikes)
          in_zone <- abs(p$PlateLocSide) <= ZONE$x[2] && p$PlateLocHeight >= ZONE$z[1] && p$PlateLocHeight <= ZONE$z[2]
          swing <- runif(1) < (if (in_zone) 0.66 else 0.28) + 0.08 * (strikes == 2)
          bip <- list(ExitSpeed = NA, Angle = NA, Distance = NA, PlayResult = "Undefined",
                      TaggedHitType = "Undefined", OutsOnPlay = 0, RunsScored = 0)
          korbb <- "Undefined"
          done <- FALSE
          if (runif(1) < 0.006) {
            call <- "HitByPitch"
            done <- TRUE
          } else if (swing) {
            if (runif(1) < (if (in_zone) 0.84 else 0.58)) {
              if (runif(1) < 0.47) {
                call <- sample(c("FoulBall", "FoulBallNotFieldable", "FoulBallFieldable"), 1, prob = c(0.5, 0.4, 0.1))
              } else {
                call <- "InPlay"
                bip <- sim_ball_in_play()
                done <- TRUE
              }
            } else {
              call <- "StrikeSwinging"
            }
          } else {
            call <- if (in_zone) (if (runif(1) < 0.9) "StrikeCalled" else "BallCalled") else (if (runif(1) < 0.93) "BallCalled" else "StrikeCalled")
          }

          new_balls <- balls
          new_strikes <- strikes
          if (call %in% c("BallCalled")) new_balls <- balls + 1
          if (call %in% c("StrikeCalled", "StrikeSwinging")) new_strikes <- strikes + 1
          if (grepl("^Foul", call) && strikes < 2) new_strikes <- strikes + 1
          if (new_strikes == 3) { korbb <- "Strikeout"; done <- TRUE }
          if (new_balls == 4) { korbb <- "Walk"; done <- TRUE }

          tagged <- p$TaggedPitchType
          if (runif(1) < 0.03) tagged <- "Undefined" # AutoPitchType fallback
          rows[[length(rows) + 1]] <- data.frame(
            PitchNo = pitch_no, Date = date_str, Time = sprintf("19:%02d:%02d", (pitch_no %/% 3) %% 60, pitch_no %% 60),
            PAofInning = pa_of_inning, PitchofPA = pitch_of_pa,
            Pitcher = pitcher$name, PitcherId = pitcher$id, PitcherThrows = pitcher$throws, PitcherTeam = pteam,
            Batter = b$name, BatterId = b$id, BatterSide = b$side, BatterTeam = bteam,
            Inning = inning, `Top/Bottom` = half, Outs = outs, Balls = balls, Strikes = strikes,
            TaggedPitchType = tagged, AutoPitchType = p$TaggedPitchType,
            PitchCall = call, KorBB = korbb, TaggedHitType = bip$TaggedHitType, PlayResult = bip$PlayResult,
            RelSpeed = p$RelSpeed, SpinRate = p$SpinRate, RelHeight = p$RelHeight, RelSide = p$RelSide,
            Extension = p$Extension, InducedVertBreak = p$InducedVertBreak, HorzBreak = p$HorzBreak,
            PlateLocHeight = p$PlateLocHeight, PlateLocSide = p$PlateLocSide,
            ExitSpeed = bip$ExitSpeed, Angle = bip$Angle, Distance = bip$Distance,
            OutsOnPlay = bip$OutsOnPlay, RunsScored = bip$RunsScored,
            HomeTeam = home, AwayTeam = away, Stadium = if (our_side == "home") "Hooker Field" else paste(home, "Park"),
            Level = "Other", League = "Demo League",
            check.names = FALSE, stringsAsFactors = FALSE
          )

          balls <- new_balls
          strikes <- new_strikes
          if (done) break
        }
        if (korbb == "Strikeout") outs <- outs + 1
        if (bip$PlayResult %in% c("Out", "Sacrifice", "FieldersChoice")) outs <- outs + 1
      }
    }
  }
  out <- bind_rows(rows)
  dir.create(TRACKMAN_DIR, recursive = TRUE, showWarnings = FALSE)
  write_csv(out, file.path(TRACKMAN_DIR, file), na = "")
  message("Wrote ", file.path(TRACKMAN_DIR, file), " (", nrow(out), " pitches)")
}

sim_game("2027-06-04", home = TEAM_CODES[1], away = "OPP_ONE", our_side = "home",
         opp = opp_players("OPP_ONE"), file = "demo_20270604.csv")
sim_game("06/06/2027", home = "OPP_TWO", away = TEAM_CODES[1], our_side = "away",
         opp = opp_players("OPP_TWO"), file = "demo_20270606.csv")

# Schedule --------------------------------------------------------------------

dates <- seq(as.Date("2027-06-04"), as.Date("2027-08-08"), by = 2)
opps <- paste0("OPP_", c("ONE", "TWO", "THREE", "FOUR", "FIVE"))
home_away <- rep(c("Home", "Away"), length.out = length(dates))
opponent <- rep(opps, length.out = length(dates))
schedule <- data.frame(
  date = format(dates, "%Y-%m-%d"),
  time = "7:00 PM",
  opponent = opponent,
  home_away = home_away,
  location = ifelse(home_away == "Home", "Hooker Field", paste(opponent, "Park")),
  result = c("W 6-3", "L 2-4", rep("", length(dates) - 2)),
  stringsAsFactors = FALSE
)

is_demo_schedule <- function(path) {
  if (!file.exists(path)) return(TRUE)
  any(grepl("OPP_ONE", readLines(path, warn = FALSE)))
}
if (is_demo_schedule(SCHEDULE_FILE)) {
  write_csv(schedule, SCHEDULE_FILE, na = "")
  message("Wrote ", SCHEDULE_FILE, " (", nrow(schedule), " games)")
} else {
  message("Left ", SCHEDULE_FILE, " alone because it is not the demo schedule.")
}

# Demo report PDFs ------------------------------------------------------------

demo_pdf <- function(path, title, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(path, width = 8.5, height = 11)
  graphics::plot.new()
  graphics::text(0.5, 0.95, title, cex = 1.8, font = 2, col = COLOR_PRIMARY)
  graphics::text(0.5, 0.9, "DEMO REPORT (fake data)", cex = 1, col = "grey40")
  for (i in seq_along(lines)) graphics::text(0.05, 0.8 - i * 0.05, lines[i], adj = 0)
  grDevices::dev.off()
  message("Wrote ", path)
}

ids <- make_game_ids(dates[1:2], opponent[1:2])
for (i in 1:2) {
  gid <- ids[i]
  # Start clean: one report per type, whatever an earlier test posted
  for (d in c(game_folder(gid), game_folder(gid, coaches_only = TRUE))) {
    unlink(list.files(d, pattern = "^(box_score|pitcher_report|hitter_report|umpire_report)(\\.[^.]*)?$",
                      full.names = TRUE))
  }
  unlink(file.path(game_folder(gid), "players"), recursive = TRUE)

  demo_pdf(file.path(game_folder(gid), "box_score.pdf"), paste("Box score", gid),
           c("Mustangs 6, Opponent 3", "Hollis 2-4, 2B, RBI", "Carver 5 IP, 6 K, 1 BB"))
  coaches <- game_folder(gid, coaches_only = TRUE)
  demo_pdf(file.path(coaches, "pitcher_report.pdf"), paste("Staff pitcher report", gid),
           c("Starter: 5 IP, 6 K, 1 BB", "Fastball averaged 92 mph", "Slider whiff rate 38%"))
  demo_pdf(file.path(coaches, "hitter_report.pdf"), paste("Team hitter report", gid),
           c("Team: 9 H, 3 BB, 7 K", "Hard hit balls: 8", "Chase rate 24%"))
  demo_pdf(file.path(coaches, "umpire_report.pdf"), paste("Umpire report", gid),
           c("Called strike accuracy: 91%", "Missed calls: 11", "Zone ran slightly wide"))
  demo_pdf(file.path(game_folder(gid), "players", "jace_hollis", "hitter_report.pdf"),
           paste("Jace Hollis hitter report", gid),
           c("2 hard-hit balls", "Chased 2 sliders away", "Good takes on 3-1"))
  demo_pdf(file.path(game_folder(gid), "players", "eli_carver", "pitcher_report.pdf"),
           paste("Eli Carver pitcher report", gid),
           c("Fastball 92-94", "Slider: 5 whiffs", "Fell behind 2-0 four times"))
}
demo_pdf(file.path(game_folder(ids[1], coaches_only = TRUE), "coaches_notes.pdf"),
         "Coaches notes", c("Bullpen availability for the weekend", "Lineup ideas vs lefties"))

# Scouting library item -------------------------------------------------------

dir.create(REPORTS_DIR, recursive = TRUE, showWarnings = FALSE)
lib_id <- "20270601120000"
lib_file <- paste0(lib_id, "_OPP_THREE_advance.pdf")
demo_pdf(file.path(REPORTS_DIR, lib_file), "Advance report: OPP_THREE",
         c("Lineup is heavy on lefties", "Starters pitch backward early in counts", "Steal a lot with two outs"))
plan_id <- "20270602120000"
plan_file <- paste0(plan_id, "_Jace_Hollis_plan.pdf")
demo_pdf(file.path(REPORTS_DIR, plan_file), "Development plan: Jace Hollis",
         c("Cut chase rate on sliders away", "Tee work: middle-away line drives", "Check in every two weeks"))

idx_cols <- c("id", "title", "category", "opponent", "player", "date", "visibility",
              "file", "notes", "uploaded_by", "uploaded_at")
idx <- if (file.exists(REPORTS_IDX)) {
  read_csv(REPORTS_IDX, col_types = cols(.default = "c"), progress = FALSE)
} else {
  as_tibble(setNames(replicate(length(idx_cols), character(), simplify = FALSE), idx_cols))
}
idx <- idx |> filter(!id %in% c(lib_id, plan_id))
now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
idx <- bind_rows(idx, tibble(
  id = c(lib_id, plan_id),
  title = c("OPP_THREE advance report", "Jace Hollis development plan"),
  category = c("Advance scouting", "Player development"),
  opponent = c("OPP_THREE", ""), player = c("", "Jace Hollis"),
  date = c("2027-06-01", "2027-06-02"), visibility = "Team",
  file = c(lib_file, plan_file),
  notes = c("Demo scouting report", "Demo player plan: only Jace and the staff see it"),
  uploaded_by = "demo", uploaded_at = now
))
write_csv(idx, REPORTS_IDX, na = "")
message("Wrote ", REPORTS_IDX)

# Demo logins -----------------------------------------------------------------

source("scripts/add_user.R")
add_user("admin", "Demo Admin", "admin", "admin123")
add_user("coach", "Demo Coach", "coach", "coach123")
add_user("player", "Jace Hollis", "player", "player123", tm_name = "Jace Hollis")
add_user("pitcher", "Eli Carver", "player", "pitcher123", tm_name = "Eli Carver")

message("Demo data ready. Start the app with shiny::runApp()")
