# Login helpers and the login screen.
# Passwords are stored as sodium::password_store() hashes in data/users.csv.

read_users <- function(path = USERS_FILE) {
  empty <- data.frame(
    username = character(), name = character(),
    role = character(), hash = character(), stringsAsFactors = FALSE
  )
  if (!file.exists(path)) return(empty)
  users <- readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
  as.data.frame(users, stringsAsFactors = FALSE)
}

# Returns list(username, name, role) on success, NULL otherwise.
check_login <- function(username, password, path = USERS_FILE) {
  if (is.null(username) || is.null(password)) return(NULL)
  username <- trimws(username)
  if (!nzchar(username) || !nzchar(password)) return(NULL)

  users <- read_users(path)
  row <- users[tolower(users$username) == tolower(username), , drop = FALSE]
  if (nrow(row) != 1) return(NULL)

  ok <- tryCatch(
    sodium::password_verify(row$hash[1], password),
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) return(NULL)

  list(username = row$username[1], name = row$name[1], role = row$role[1])
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
