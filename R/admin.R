# Roles and the admin helpers used by the Add & manage tab.
#
# Roles in data/users.csv:
#   admin   everything a coach can, plus manage logins and publish
#   coach   sees everything; adds, edits and deletes reports, files and games
#   player  sees only his own reports and stats, plus team-wide files

ROLES <- c("admin", "coach", "player")

# TRUE for coaches and the admin. Use this for every "can see everything" check.
is_staff <- function(u) {
  !is.null(u) && isTRUE(u$role %in% c("coach", "admin"))
}

is_admin <- function(u) {
  !is.null(u) && identical(u$role, "admin")
}

# TRUE when the app is running on the hosting site instead of the laptop.
on_server <- function() {
  Sys.getenv("R_CONFIG_ACTIVE") %in% c("shinyapps", "rsconnect")
}

# Game files -------------------------------------------------------------------

# Folder for one audience of a game: "team", "coaches" or a player name.
# Built in steps: file.path(dir, if (FALSE) "coaches") returns character(0)
# and breaks dir.create.
audience_dir <- function(game_id, audience) {
  if (!grepl("^[A-Za-z0-9_-]+$", game_id)) stop("Bad game id.")
  dir <- file.path(GAMES_DIR, game_id)
  if (identical(audience, "coaches")) {
    dir <- file.path(dir, "coaches")
  } else if (!identical(audience, "team")) {
    slug <- player_slug(audience)
    if (!nzchar(slug)) stop("Pick a player.")
    dir <- file.path(dir, "players", slug)
  }
  dir
}

# Deletes <stem>.<any ext> in each folder
clear_report_type <- function(dirs, type) {
  stem <- type_stem[[type]]
  for (d in dirs[dir.exists(dirs)]) {
    old <- list.files(d, pattern = paste0("^", stem, "(\\.[^.]*)?$"),
                      full.names = TRUE, ignore.case = TRUE)
    unlink(old)
  }
}

# Saves an uploaded file for a game. audience is "team", "coaches" or a player
# name. Box scores and Pitcher/Hitter/Umpire reports are saved as
# box_score.<ext>, pitcher_report.<ext> etc. and replace the old one of that
# type: for team or coaches, in both the game root and coaches/ so there is one
# team-wide copy; for a player, only in that player's folder.
post_game_file <- function(game_id, type, audience, src_path, orig_name) {
  if (!type %in% REPORT_TYPES) stop("Unknown report type: ", type)
  dir <- audience_dir(game_id, audience)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  ext <- tolower(tools::file_ext(orig_name))
  if (type %in% names(type_stem)) {
    clear <- if (audience %in% c("team", "coaches")) {
      c(audience_dir(game_id, "team"), audience_dir(game_id, "coaches"))
    } else {
      dir
    }
    clear_report_type(clear, type)
    dest_name <- type_stem[[type]]
    if (nzchar(ext)) dest_name <- paste0(dest_name, ".", ext)
  } else {
    dest_name <- sanitize_name(orig_name)
    # The name decides the tab, so "hitting_notes.pdf" would land on Hitter.
    # Swap out those words so it stays on Other.
    if (grepl("box|ump|pitch|hit|bat", dest_name, ignore.case = TRUE)) {
      dest_name <- paste0("file_", gsub("box|ump|pitch|hit|bat", "x", dest_name, ignore.case = TRUE))
    }
    if (file.exists(file.path(dir, dest_name))) {
      dest_name <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", dest_name)
    }
  }
  dest <- file.path(dir, dest_name)
  if (!file.copy(src_path, dest, overwrite = TRUE)) stop("Could not save the file.")
  dest
}

# Schedule ---------------------------------------------------------------------

SCHEDULE_COLS <- c("game_id", "date", "time", "opponent", "home_away", "location", "result")

# Writes the schedule sorted by date. Writing game_id keeps ids stable when
# games are added or removed later.
save_schedule <- function(s, path = SCHEDULE_FILE) {
  out <- s |>
    arrange(date) |>
    mutate(date = format(as.Date(date), "%Y-%m-%d"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(out[, SCHEDULE_COLS], path, na = "")
  invisible(out)
}

# Id for a new game: same rule as make_game_ids() (the nth game on a date gets
# _Gn), then _G2, _G3 and so on until it is unused.
new_game_id <- function(date, opponent, existing_ids, existing_dates) {
  n <- sum(as.Date(existing_dates) == as.Date(date), na.rm = TRUE) + 1
  base <- make_game_ids(date, opponent)
  id <- if (n > 1) paste0(base, "_G", n) else base
  while (id %in% existing_ids) {
    n <- n + 1
    id <- paste0(base, "_G", n)
  }
  id
}

add_game <- function(date, opponent, home_away, time = "", location = "") {
  opponent <- trimws(opponent)
  if (!length(date) || is.na(as.Date(date))) stop("Pick a date.")
  if (!nzchar(opponent)) stop("Type the opponent.")
  s <- load_schedule()
  gid <- new_game_id(date, opponent, s$game_id, s$date)
  row <- tibble::tibble(
    game_id = gid, date = as.Date(date), time = trimws(time), opponent = opponent,
    home_away = normalize_home_away(home_away), location = trimws(location), result = ""
  )
  save_schedule(dplyr::bind_rows(s[, SCHEDULE_COLS], row))
  dir.create(audience_dir(gid, "coaches"), recursive = TRUE, showWarnings = FALSE)
  gid
}

# Changes one field (time, location or result) of one game
update_game <- function(game_id, field, value) {
  if (!field %in% c("time", "location", "result")) stop("That column can't be edited.")
  s <- load_schedule()
  i <- which(s$game_id == game_id)
  if (length(i) != 1) stop("Game not found.")
  s[[field]][i] <- trimws(as.character(value))
  save_schedule(s[, SCHEDULE_COLS])
}

remove_game <- function(game_id) {
  s <- load_schedule()
  save_schedule(s[s$game_id != game_id, SCHEDULE_COLS])
}

# Replaces the schedule with an uploaded CSV. Needs date, opponent, home_away.
# Returns the number of games.
replace_schedule <- function(csv_path) {
  raw <- readr::read_csv(csv_path, col_types = readr::cols(.default = "c"),
                         progress = FALSE, show_col_types = FALSE)
  names(raw) <- tolower(trimws(names(raw)))
  missing <- setdiff(c("date", "opponent", "home_away"), names(raw))
  if (length(missing)) stop("The CSV is missing: ", paste(missing, collapse = ", "))
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(raw, tmp, na = "")
  s <- load_schedule(tmp)
  if (!nrow(s)) stop("No games with a readable date and an opponent.")
  save_schedule(s[, SCHEDULE_COLS])
  for (gid in s$game_id) {
    dir.create(audience_dir(gid, "coaches"), recursive = TRUE, showWarnings = FALSE)
  }
  nrow(s)
}

# TrackMan files ---------------------------------------------------------------

TM_REQUIRED <- c("Date", "Pitcher", "PitcherTeam", "Batter", "BatterTeam", "PitchCall",
                 "RelSpeed", "PlateLocSide", "PlateLocHeight")

# Required columns missing from a CSV's header (all of them if it can't be read)
trackman_missing_cols <- function(path) {
  header <- tryCatch(
    names(readr::read_csv(path, n_max = 0, col_types = readr::cols(.default = "c"),
                          progress = FALSE, show_col_types = FALSE)),
    error = function(e) character()
  )
  setdiff(TM_REQUIRED, header)
}

# One row per loaded file: File, Date, Game, Pitches
trackman_file_table <- function(tm, files = trackman_files()) {
  names <- basename(files)
  info <- tm |>
    group_by(SourceFile) |>
    summarise(
      Date = if (all(is.na(Date))) "" else format(min(Date, na.rm = TRUE)),
      Game = paste(AwayTeam[1], "at", HomeTeam[1]),
      Pitches = n(),
      .groups = "drop"
    )
  tibble::tibble(File = names) |>
    left_join(info, by = c(File = "SourceFile")) |>
    mutate(Pitches = ifelse(is.na(Pitches), 0L, Pitches)) |>
    arrange(desc(Date), File)
}

# Every name on our team in the TrackMan data
our_trackman_names <- function(tm) {
  sort(unique(c(
    tm$Pitcher[tm$PitcherTeam %in% TEAM_CODES],
    tm$Batter[tm$BatterTeam %in% TEAM_CODES]
  )))
}

# Logins -----------------------------------------------------------------------

write_users <- function(users, path = USERS_FILE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(users[, c("username", "name", "role", "tm_name", "hash")], path, na = "")
}

# Adds a login or, for an existing username, replaces it (resets the password)
save_login <- function(username, name, role, password, tm_name = "") {
  username <- trimws(username)
  name <- trimws(name)
  tm_name <- trimws(tm_name)
  if (!grepl("^[A-Za-z0-9._-]+$", username)) {
    stop("Username can only use letters, numbers, dots, dashes and underscores.")
  }
  if (!nzchar(name)) stop("Type the full name.")
  if (!role %in% ROLES) stop("Pick a role.")
  if (is.null(password) || nchar(password) < 6) stop("Password must be at least 6 characters.")
  if (!nzchar(tm_name)) tm_name <- name

  users <- load_users()
  existed <- tolower(username) %in% tolower(users$username)
  users <- users[tolower(users$username) != tolower(username), , drop = FALSE]
  users <- rbind(users[, c("username", "name", "role", "tm_name", "hash")], data.frame(
    username = username, name = name, role = role, tm_name = tm_name,
    hash = sodium::password_store(password), stringsAsFactors = FALSE
  ))
  write_users(users)
  invisible(existed)
}

remove_login <- function(username) {
  users <- load_users()
  write_users(users[tolower(users$username) != tolower(username), , drop = FALSE])
}
