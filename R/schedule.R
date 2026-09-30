# Schedule loading and the per-game report folders.
#
# data/schedule.csv columns: date, time, opponent, home_away, location, result
# (game_id is optional). Reports for a game live in reports/games/<game_id>/ and
# coaches-only files for that game go in reports/games/<game_id>/coaches/.

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

# The file name decides the report type. "ump" is checked first.
report_type <- function(name) {
  n <- tolower(name)
  ifelse(grepl("ump", n), "Umpire",
         ifelse(grepl("pitch", n), "Pitcher",
                ifelse(grepl("hit|bat", n), "Hitter", "Other")))
}

type_short <- c(Pitcher = "Pitch", Hitter = "Hit", Umpire = "Ump")

sanitize_name <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", basename(x))
  x <- gsub("_+", "_", x)
  if (!nzchar(gsub("[._]", "", x))) x <- "file"
  x
}

# Every report file under reports/games, one row per file.
scan_game_files <- function(dir = GAMES_DIR) {
  empty <- tibble::tibble(game_id = character(), path = character(), name = character(),
                          type = character(), coaches = logical())
  if (!dir.exists(dir)) return(empty)
  rel <- list.files(dir, recursive = TRUE)
  rel <- rel[!grepl("(^|/)\\.", rel)] # skip .gitkeep and hidden files
  parts <- strsplit(rel, "/", fixed = TRUE)
  depth <- lengths(parts)
  # <game_id>/<file> or <game_id>/coaches/<file>
  keep <- depth == 2 | (depth == 3 & vapply(parts, \(p) p[2] == "coaches", logical(1)))
  if (!any(keep)) return(empty)
  parts <- parts[keep]
  rel <- rel[keep]
  name <- vapply(parts, \(p) p[length(p)], character(1))
  tibble::tibble(
    game_id = vapply(parts, \(p) p[1], character(1)),
    path = file.path(dir, rel),
    name = name,
    type = report_type(name),
    coaches = lengths(parts) == 3
  )
}

# Visible files for one game, ordered Pitcher, Hitter, Umpire, Other.
# Players never see files from the coaches/ subfolder.
game_files <- function(game_id, user, listing = scan_game_files()) {
  gid <- game_id
  out <- listing |> filter(game_id == gid)
  if (!is_staff(user)) out <- out |> filter(!coaches)
  out |>
    arrange(match(type, GAME_REPORT_TYPES), coaches, name) |>
    select(path, name, type, coaches)
}

# Short label per game, like "Pitch, Hit, Ump, +1".
report_status <- function(game_ids, user, listing = scan_game_files()) {
  if (!is_staff(user)) listing <- listing |> filter(!coaches)
  vapply(game_ids, function(gid) {
    types <- listing$type[listing$game_id == gid]
    if (!length(types)) return("")
    main <- unname(type_short[intersect(c("Pitcher", "Hitter", "Umpire"), types)])
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

# Saves an uploaded file into a game folder. Pitcher/Hitter/Umpire reports are
# saved as pitcher_report.<ext> etc. and replace the old one of that type in
# that folder, whatever its extension, so there is only ever one.
save_game_report <- function(game_id, type, coaches_only, src_path, original_name) {
  folder <- game_folder(game_id, coaches_only)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  ext <- tolower(tools::file_ext(original_name))
  if (type %in% c("Pitcher", "Hitter", "Umpire")) {
    stem <- paste0(tolower(type), "_report")
    old <- list.files(folder, pattern = paste0("^", stem, "(\\.[^.]*)?$"),
                      full.names = TRUE, ignore.case = TRUE)
    unlink(old)
    dest_name <- if (nzchar(ext)) paste0(stem, ".", ext) else stem
  } else {
    dest_name <- sanitize_name(original_name)
    # The name decides the type, so a name like "hitting_notes.pdf" would land
    # under Hitter. Give it a neutral name instead.
    if (report_type(dest_name) != "Other") {
      dest_name <- paste0("other_report", if (nzchar(ext)) paste0(".", ext))
    }
    if (file.exists(file.path(folder, dest_name))) {
      dest_name <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", dest_name)
    }
  }
  dest <- file.path(folder, dest_name)
  ok <- file.copy(src_path, dest, overwrite = TRUE)
  if (!ok) stop("Could not save the file.")
  dest
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
    div(
      id = "game_page",
      uiOutput("game_page"),
      uiOutput("post_card")
    )
  )
}
