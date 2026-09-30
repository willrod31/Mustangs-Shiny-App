# Schedule loading and the per-game report folders.
#
# data/schedule.csv columns: game_id, date, time, opponent, home_away, location,
# result (game_id is optional). A game's files can live in three places:
#
#   reports/games/<game_id>/                  whole team sees it (box scores)
#   reports/games/<game_id>/coaches/          coaches and admin only
#   reports/games/<game_id>/players/<slug>/   that one player, plus coaches and admin

# game_id = YYYY-MM-DD_Opponent with non-alphanumeric characters stripped.
# The second game on the same date gets _G2 (third _G3 and so on).
# python/report_paths.py game_id() must give the same result.
make_game_ids <- function(date, opponent) {
  date <- format(as.Date(date), "%Y-%m-%d")
  base <- paste0(date, "_", gsub("[^A-Za-z0-9]+", "", opponent))
  game_num <- stats::ave(seq_along(date), date, FUN = seq_along)
  ifelse(game_num > 1, paste0(base, "_G", game_num), base)
}

normalize_home_away <- function(x) {
  x <- tolower(trimws(x))
  ifelse(x %in% c("a", "away", "@", "road", "at"), "Away", "Home")
}

empty_schedule <- function() {
  tibble::tibble(
    date = as.Date(character()), time = character(), opponent = character(),
    home_away = character(), location = character(), result = character(),
    game_id = character(), matchup = character(), played = logical()
  )
}

load_schedule <- function(path = SCHEDULE_FILE) {
  if (!file.exists(path)) return(empty_schedule())
  s <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                       progress = FALSE, show_col_types = FALSE)
  if (!nrow(s)) return(empty_schedule())

  for (n in setdiff(c("date", "time", "opponent", "home_away", "location", "result", "game_id"), names(s))) {
    s[[n]] <- NA_character_
  }

  s <- s |>
    mutate(
      date = parse_tm_date(date),
      across(c(time, opponent, location, result, game_id), \(x) trimws(ifelse(is.na(x), "", x))),
      home_away = normalize_home_away(home_away)
    ) |>
    filter(!is.na(date), opponent != "")

  auto_id <- make_game_ids(s$date, s$opponent)
  s |>
    mutate(
      game_id = ifelse(game_id == "", auto_id, game_id),
      matchup = paste(ifelse(home_away == "Home", "vs", "@"), opponent),
      played = result != ""
    ) |>
    select(date, time, opponent, home_away, location, result, game_id, matchup, played)
}

# The file name decides the report type. "box" is checked first so a file like
# box_score_hitters.pdf stays a box score, then "ump" so umpire_pitch_calls.pdf
# stays an umpire report.
classify_report <- function(name) {
  n <- tolower(name)
  ifelse(grepl("box", n), "Box score",
         ifelse(grepl("ump", n), "Umpire report",
                ifelse(grepl("pitch", n), "Pitcher report",
                       ifelse(grepl("hit|bat", n), "Hitter report", "Other"))))
}

# Short labels for the schedule table and tab names on the game page
type_short <- c(`Box score` = "Box", `Pitcher report` = "Pitch",
                `Hitter report` = "Hit", `Umpire report` = "Ump")
type_tab <- c(`Box score` = "Box score", `Pitcher report` = "Pitcher",
              `Hitter report` = "Hitter", `Umpire report` = "Umpire", Other = "Other")

# Saved file name (without extension) for the one-per-folder types
type_stem <- c(`Box score` = "box_score", `Pitcher report` = "pitcher_report",
               `Hitter report` = "hitter_report", `Umpire report` = "umpire_report")

# Folder-safe player key. "Hollis, Jace" becomes "hollis_jace".
# python/report_paths.py player_slug() must give the same result.
player_slug <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

sanitize_name <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", basename(x))
  x <- gsub("_+", "_", x)
  if (!nzchar(gsub("[._]", "", x))) x <- "file"
  x
}

# Every report file under reports/games, one row per file. audience is "team",
# "coaches" or "player"; slug is the player's folder name for "player" files.
scan_game_files <- function(dir = GAMES_DIR) {
  empty <- tibble::tibble(game_id = character(), path = character(), name = character(),
                          type = character(), audience = character(), slug = character())
  if (!dir.exists(dir)) return(empty)
  rel <- list.files(dir, recursive = TRUE)
  rel <- rel[!grepl("(^|/)\\.", rel)] # skip .gitkeep and hidden files
  parts <- strsplit(rel, "/", fixed = TRUE)
  depth <- lengths(parts)
  second <- vapply(parts, \(p) if (length(p) >= 2) p[2] else "", character(1))
  # <game_id>/<file>, <game_id>/coaches/<file> or <game_id>/players/<slug>/<file>
  keep <- depth == 2 | (depth == 3 & second == "coaches") | (depth == 4 & second == "players")
  if (!any(keep)) return(empty)
  parts <- parts[keep]
  depth <- depth[keep]
  name <- vapply(parts, \(p) p[length(p)], character(1))
  tibble::tibble(
    game_id = vapply(parts, \(p) p[1], character(1)),
    path = file.path(dir, rel[keep]),
    name = name,
    type = classify_report(name),
    audience = c("team", "coaches", "player")[depth - 1],
    slug = ifelse(depth == 4, vapply(parts, \(p) p[min(3, length(p))], character(1)), "")
  )
}

# "jace_hollis" back to "Jace Hollis"
slug_to_name <- function(slug) {
  gsub("(^|\\s)([a-z])", "\\1\\U\\2", gsub("_", " ", slug), perl = TRUE)
}

# The rows of a listing this user may see. Staff see everything. A player sees
# team files plus his own players/<slug>/ folder, never coaches/ or anyone else's.
visible_game_listing <- function(listing, user) {
  if (is_staff(user)) return(listing)
  slug <- user$slug %||% ""
  listing |> filter(audience == "team" | (audience == "player" & nzchar(slug) & slug == !!slug))
}

# Visible files for one game with a "who sees it" label, ordered by
# REPORT_TYPES, then Everyone first, then who, then name.
game_files <- function(game_id, user, listing = scan_game_files()) {
  gid <- game_id
  out <- visible_game_listing(listing, user) |> filter(game_id == gid)
  staff <- is_staff(user)
  out |>
    mutate(
      who = case_when(
        audience == "team" ~ "Everyone",
        audience == "coaches" ~ "Coaches only",
        staff ~ slug_to_name(slug),
        TRUE ~ "Just you"
      ),
      coaches = who != "Everyone"
    ) |>
    arrange(match(type, REPORT_TYPES), coaches, who, name) |>
    select(path, name, type, who, coaches)
}

# Short label per game, like "Box, Pitch, Hit, Ump, +1". Counts only what
# this user may see.
report_status <- function(game_ids, user, listing = scan_game_files()) {
  listing <- visible_game_listing(listing, user)
  vapply(game_ids, function(gid) {
    types <- listing$type[listing$game_id == gid]
    if (!length(types)) return("")
    main <- unname(type_short[intersect(names(type_short), types)])
    n_other <- sum(types == "Other") + sum(duplicated(types[types != "Other"]))
    paste(c(main, if (n_other > 0) paste0("+", n_other)), collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
}

# Folder for a game. Built in two steps: file.path(dir, if (FALSE) "coaches")
# returns character(0) and breaks dir.create.
game_folder <- function(game_id, coaches_only = FALSE) {
  folder <- file.path(GAMES_DIR, game_id)
  if (isTRUE(coaches_only)) folder <- file.path(folder, "coaches")
  folder
}

# Matches a schedule game to a TrackMan GameKey by date. For doubleheaders the
# _G2 suffix picks the second TrackMan file from that date.
match_trackman <- function(game_id, date, games) {
  same_day <- games |> filter(date == !!date) |> arrange(GameKey)
  if (!nrow(same_day)) return(NULL)
  n <- suppressWarnings(as.integer(sub(".*_G(\\d+)$", "\\1", game_id)))
  if (is.na(n)) n <- 1
  if (n > nrow(same_day)) return(NULL)
  same_day$GameKey[n]
}

# Returns a reactive that changes whenever any of the files from files_fun()
# are added, removed or modified. Used to refresh the schedule, the game
# folders and the library index without restarting the app.
change_signal <- function(session, interval_ms, files_fun) {
  reactivePoll(
    interval_ms, session,
    checkFunc = function() {
      f <- files_fun()
      info <- file.info(f)
      paste(f, info$mtime, info$size, collapse = "|")
    },
    valueFunc = function() Sys.time()
  )
}

# Schedule tab UI. Two columns on desktop, stacked on phones.
schedule_ui <- function() {
  layout_columns(
    col_widths = breakpoints(sm = c(12, 12), md = c(5, 7)),
    card(
      fill = FALSE,
      card_header("Season"),
      card_body(
        fillable = FALSE,
        radioButtons("sched_mode", NULL, inline = TRUE, choices = c(
          "Games with reports" = "reports", "Full schedule" = "full"
        )),
        DTOutput("sched_table", height = "auto")
      )
    ),
    div(id = "game_page", uiOutput("game_page"))
  )
}
