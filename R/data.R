# TrackMan loading, the game list and the scouting library index.

TM_NUMERIC <- c(
  "RelSpeed", "SpinRate", "InducedVertBreak", "HorzBreak", "RelHeight", "RelSide",
  "Extension", "PlateLocHeight", "PlateLocSide", "ExitSpeed", "Angle", "Distance",
  "Inning", "Balls", "Strikes", "PitchNo", "OutsOnPlay", "RunsScored"
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
  cols <- c(TM_TEXT, TM_NUMERIC, "SourceFile", "GameKey", "PitchType", "TeamFirstPitch")
  df <- as.data.frame(setNames(replicate(length(cols), character(), simplify = FALSE), cols))
  for (n in TM_NUMERIC) df[[n]] <- numeric()
  df$Date <- as.Date(character())
  df$TeamFirstPitch <- logical()
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
    ) |>
    mark_team_first_pitch()
}

# TRUE on the first pitch our team threw in each game (sorted by GameKey,
# Inning, PitchNo). Marked here, before a player's data is filtered down to his
# own pitches, so games started (GS) stay right.
mark_team_first_pitch <- function(tm) {
  ours <- which(tm$PitcherTeam %in% TEAM_CODES)
  tm$TeamFirstPitch <- FALSE
  if (!length(ours)) return(tm)
  o <- ours[order(tm$GameKey[ours], tm$Inning[ours], tm$PitchNo[ours])]
  tm$TeamFirstPitch[o[!duplicated(tm$GameKey[o])]] <- TRUE
  tm
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

# Scouting library index -------------------------------------------------------
# reports/index.csv has one row per file in reports/files/.
# python/report_paths.py writes the same columns.

INDEX_COLS <- c("id", "title", "category", "opponent", "player", "date", "visibility",
                "file", "notes", "uploaded_by", "uploaded_at")

read_index <- function(path = REPORTS_IDX) {
  empty <- tibble::as_tibble(setNames(
    replicate(length(INDEX_COLS), character(), simplify = FALSE), INDEX_COLS
  ))
  if (!file.exists(path)) return(empty)
  idx <- tryCatch(
    readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                    progress = FALSE, show_col_types = FALSE),
    error = function(e) empty
  )
  for (n in setdiff(INDEX_COLS, names(idx))) idx[[n]] <- NA_character_
  idx |>
    select(all_of(INDEX_COLS)) |>
    mutate(across(everything(), \(x) ifelse(is.na(x), "", x)))
}

write_index <- function(idx, path = REPORTS_IDX) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(idx[, INDEX_COLS], path, na = "")
}

# Staff see every row. Players see Team rows that either name no player or
# name them (so a development plan is only for that player and the staff).
visible_reports <- function(idx, user) {
  if (is_staff(user)) return(idx)
  who <- trimws(idx$player)
  mine <- nzchar(who) & (
    player_slug(who) == (user$slug %||% "") |
      tolower(who) == tolower(trimws(user$name %||% ""))
  )
  idx[idx$visibility == "Team" & (!nzchar(who) | mine), ]
}

library_path <- function(file) file.path(REPORTS_DIR, file)

# Copies a file into the library and adds its index row. Returns the new id.
add_library_item <- function(src_path, original_name, title, category, opponent = "",
                             player = "", date = "", visibility = "Team", notes = "",
                             uploaded_by = "") {
  if (!nzchar(trimws(title))) stop("Title is required.")
  if (!category %in% REPORT_CATEGORIES) stop("Unknown category: ", category)
  if (!visibility %in% c("Team", "Coaches")) stop("Visibility must be Team or Coaches.")

  idx <- read_index()
  id <- format(Sys.time(), "%Y%m%d%H%M%S")
  while (id %in% idx$id) id <- as.character(as.numeric(id) + 1)

  dir.create(REPORTS_DIR, recursive = TRUE, showWarnings = FALSE)
  file <- paste0(id, "_", sanitize_name(original_name))
  if (!file.copy(src_path, library_path(file))) stop("Could not save the file.")

  row <- tibble::tibble(
    id = id, title = trimws(title), category = category, opponent = trimws(opponent),
    player = trimws(player), date = as.character(date), visibility = visibility,
    file = file, notes = trimws(notes), uploaded_by = uploaded_by,
    uploaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  write_index(dplyr::bind_rows(idx, row))
  id
}

# Removes the file and its index row
delete_library_item <- function(id) {
  idx <- read_index()
  row <- idx[idx$id == id, ]
  if (!nrow(row)) return(invisible(FALSE))
  f <- library_path(row$file[1])
  if (nzchar(row$file[1]) && file.exists(f)) unlink(f)
  write_index(idx[idx$id != id, ])
  invisible(TRUE)
}

# UI ---------------------------------------------------------------------------

library_ui <- function() {
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      open = list(desktop = "open", mobile = "always-above"),
      selectInput("lib_cat", "Category", choices = c("All" = "", REPORT_CATEGORIES)),
      selectInput("lib_opp", "Opponent", choices = c("All" = "")),
      textInput("lib_search", "Search", placeholder = "Title, player, notes")
    ),
    table_card("Reports", "lib_table"),
    uiOutput("lib_preview")
  )
}
