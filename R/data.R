# TrackMan loading, the game list and the scouting library index.

TM_NUMERIC <- c(
  "RelSpeed", "SpinRate", "InducedVertBreak", "HorzBreak", "RelHeight", "RelSide",
  "Extension", "PlateLocHeight", "PlateLocSide", "ExitSpeed", "Angle", "Distance",
  "Inning", "Balls", "Strikes", "PitchNo"
)

TM_TEXT <- c(
  "Date", "Pitcher", "PitcherTeam", "Batter", "BatterTeam", "TaggedPitchType",
  "AutoPitchType", "PitchCall", "KorBB", "PlayResult", "HomeTeam", "AwayTeam"
)

# Accepts 2027-06-04, 06/04/2027 and 06/04/27
parse_tm_date <- function(x) {
  x <- trimws(x)
  out <- as.Date(x, format = "%Y-%m-%d")
  long <- is.na(out) & grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x)
  out[long] <- as.Date(x[long], format = "%m/%d/%Y")
  short <- is.na(out) & grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", x)
  out[short] <- as.Date(x[short], format = "%m/%d/%y")
  out
}

trackman_files <- function(dir = TRACKMAN_DIR) {
  list.files(dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
}

empty_trackman <- function() {
  cols <- c(TM_TEXT, TM_NUMERIC, "SourceFile", "GameKey", "PitchType")
  df <- as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols))
  for (n in TM_NUMERIC) df[[n]] <- numeric()
  df$Date <- as.Date(character())
  tibble::as_tibble(df)
}

load_trackman <- function(dir = TRACKMAN_DIR) {
  files <- trackman_files(dir)
  if (!length(files)) return(empty_trackman())

  tm <- lapply(files, function(f) {
    d <- tryCatch(
      readr::read_csv(f, col_types = readr::cols(.default = readr::col_character()),
                      progress = FALSE, show_col_types = FALSE),
      error = function(e) NULL
    )
    if (is.null(d) || !nrow(d)) return(NULL)
    d$SourceFile <- basename(f)
    d
  })
  tm <- dplyr::bind_rows(tm)
  if (!nrow(tm)) return(empty_trackman())

  # Make sure every column we use exists
  for (n in setdiff(c(TM_TEXT, TM_NUMERIC), names(tm))) tm[[n]] <- NA_character_
  for (n in TM_NUMERIC) tm[[n]] <- suppressWarnings(as.numeric(tm[[n]]))

  tm |>
    mutate(
      Date = parse_tm_date(Date),
      GameKey = paste(Date, AwayTeam, "at", HomeTeam, SourceFile),
      PitchType = ifelse(
        is.na(TaggedPitchType) | TaggedPitchType %in% c("", "Undefined"),
        AutoPitchType, TaggedPitchType
      ),
      PitchType = ifelse(is.na(PitchType) | PitchType == "", "Undefined", PitchType)
    )
}

# One row per game, newest first
game_list <- function(tm) {
  if (!nrow(tm)) {
    return(tibble::tibble(GameKey = character(), date = as.Date(character()),
                          home = character(), away = character(),
                          opponent = character(), label = character()))
  }
  tm |>
    distinct(GameKey, Date, HomeTeam, AwayTeam, SourceFile) |>
    group_by(GameKey) |>
    slice(1) |>
    ungroup() |>
    mutate(
      we_are_home = HomeTeam %in% TEAM_CODES,
      opponent = ifelse(we_are_home, AwayTeam, HomeTeam),
      label = paste0(format(Date, "%b %d"), "  ", ifelse(we_are_home, "vs ", "@ "), opponent)
    ) |>
    arrange(desc(Date), SourceFile) |>
    transmute(GameKey, date = Date, home = HomeTeam, away = AwayTeam, opponent, label)
}

# Reloads TrackMan data whenever a CSV is added, changed or removed.
trackman_poll <- function(session, dir = TRACKMAN_DIR) {
  reactivePoll(
    10000, session,
    checkFunc = function() {
      f <- trackman_files(dir)
      paste(f, file.info(f)$mtime, collapse = "|")
    },
    valueFunc = function() load_trackman(dir)
  )
}
