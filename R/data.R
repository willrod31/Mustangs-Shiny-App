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

# Secure file preview ----------------------------------------------------------
# Report folders are never exposed with addResourcePath (that would make every
# file public to anyone with the URL). Previews are served through
# session$registerDataObj, which only works inside a logged-in session.

mime_for <- function(path) {
  switch(tolower(tools::file_ext(path)),
    pdf = "application/pdf",
    png = "image/png",
    jpg = , jpeg = "image/jpeg",
    html = , htm = "text/html; charset=utf-8",
    txt = "text/plain; charset=utf-8",
    csv = "text/csv; charset=utf-8",
    xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "application/octet-stream"
  )
}

# Inline preview for one file. name is a unique key for this preview slot.
preview_ui <- function(session, path, name) {
  ext <- tolower(tools::file_ext(path))
  frame_types <- c("pdf", "html", "htm", "txt")
  image_types <- c("png", "jpg", "jpeg")
  if (!ext %in% c(frame_types, image_types) || !file.exists(path)) {
    return(p(class = "text-muted mb-0", "No preview for this file type. Use Download."))
  }
  url <- session$registerDataObj(name, path, function(data, req) {
    shiny::httpResponse(200, mime_for(data), readBin(data, "raw", file.info(data)$size))
  })
  frame_style <- "width:100%; height:75vh; border:1px solid #dee2e6; border-radius:6px; background:#fff;"
  if (ext %in% image_types) {
    tags$img(src = url, class = "img-fluid border rounded", alt = basename(path))
  } else if (ext %in% c("html", "htm")) {
    # Scripts run (for Plotly) but the page cannot reach the app
    tags$iframe(src = url, style = frame_style, sandbox = "allow-scripts allow-popups")
  } else {
    tags$iframe(src = url, style = frame_style)
  }
}
