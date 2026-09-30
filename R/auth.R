# Login helpers and the login screen.
# Passwords are stored as sodium::password_store() hashes in data/users.csv.

# All columns are read as character. tm_name (the player's name in TrackMan)
# defaults to name when the column is missing or blank, so old files still work.
load_users <- function(path = USERS_FILE) {
  empty <- data.frame(
    username = character(), name = character(), role = character(),
    tm_name = character(), hash = character(), stringsAsFactors = FALSE
  )
  if (!file.exists(path)) return(empty)
  users <- readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
  users <- as.data.frame(users, stringsAsFactors = FALSE)
  for (n in setdiff(names(empty), names(users))) users[[n]] <- rep(NA_character_, nrow(users))
  users$name[is.na(users$name)] <- ""
  blank <- is.na(users$tm_name) | !nzchar(trimws(users$tm_name))
  users$tm_name[blank] <- users$name[blank]
  users$tm_name <- trimws(users$tm_name)
  users
}

is_owner_name <- function(username) {
  !is.null(username) && length(username) == 1 && !is.na(username) &&
    identical(tolower(trimws(username)), tolower(OWNER$username))
}

# Adds the owner login (role admin, OWNER$hash) when data/users.csv doesn't
# have it, creating the file if needed. Runs once when the app starts.
ensure_owner <- function(path = USERS_FILE) {
  users <- load_users(path)
  if (any(vapply(users$username, is_owner_name, logical(1)))) return(invisible(FALSE))
  owner <- data.frame(username = OWNER$username, name = OWNER$name, role = "admin",
                      tm_name = OWNER$name, hash = OWNER$hash, stringsAsFactors = FALSE)
  users <- rbind(users[, names(owner)], owner)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(users, path, na = "")
  invisible(TRUE)
}

# Returns list(username, name, role, tm_name, slug) on success, NULL otherwise.
# The owner is always an admin, even if his row in the CSV was edited.
check_login <- function(username, password, path = USERS_FILE) {
  if (is.null(username) || is.null(password)) return(NULL)
  username <- trimws(username)
  if (!nzchar(username) || !nzchar(password)) return(NULL)

  users <- load_users(path)
  row <- users[tolower(users$username) == tolower(username), , drop = FALSE]
  if (nrow(row) != 1) return(NULL)

  ok <- tryCatch(
    sodium::password_verify(row$hash[1], password),
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) return(NULL)

  role <- if (is_owner_name(row$username[1])) "admin" else row$role[1]
  list(username = row$username[1], name = row$name[1], role = role,
       tm_name = row$tm_name[1], slug = player_slug(row$tm_name[1]))
}

login_ui <- function() {
  div(
    id = "login-overlay",
    style = paste0(
      "position:fixed; inset:0; z-index:2000; background:", COLOR_SECONDARY, ";",
      "display:flex; align-items:center; justify-content:center; padding:16px;"
    ),
    div(
      class = "card shadow",
      style = "width:100%; max-width:360px;",
      div(
        class = "card-body p-4",
        h4(APP_TITLE, class = "mb-1", style = paste0("color:", COLOR_PRIMARY, ";")),
        p("Sign in to see reports.", class = "text-muted mb-3"),
        textInput("login_user", "Username", width = "100%"),
        passwordInput("login_pass", "Password", width = "100%"),
        actionButton("login_btn", "Sign in", class = "btn-primary w-100 mt-2"),
        div(textOutput("login_error"), class = "text-danger mt-2", style = "min-height:1.5em;")
      )
    ),
    # Enter in the password box signs in
    tags$script(HTML(
      "$(document).on('keyup', '#login_pass', function(e) {
         if (e.key === 'Enter') { $('#login_btn').click(); }
       });"
    ))
  )
}
