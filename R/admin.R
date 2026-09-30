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
