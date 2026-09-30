# Manage logins in data/users.csv.
# Run from the app folder:
#   source("scripts/add_user.R")
#   add_user("jsmith", "John Smith", "player", "a-strong-password")
#   add_roster("roster.csv")   # columns: username,name,role,password
#   remove_user("jsmith")
#
# Passwords are hashed with sodium before they are saved.
# Never commit data/users.csv or a plain-text roster file.

library(readr)
library(sodium)

USERS_FILE <- "data/users.csv"

.read_users <- function() {
  if (!file.exists(USERS_FILE)) {
    return(data.frame(username = character(), name = character(),
                      role = character(), hash = character(),
                      stringsAsFactors = FALSE))
  }
  as.data.frame(read_csv(USERS_FILE, col_types = cols(.default = "c"), progress = FALSE),
                stringsAsFactors = FALSE)
}

.write_users <- function(users) {
  dir.create(dirname(USERS_FILE), showWarnings = FALSE, recursive = TRUE)
  write_csv(users, USERS_FILE, na = "")
}

.add_one <- function(users, username, name, role, password) {
  username <- trimws(username)
  role <- tolower(trimws(role))
  if (!nzchar(username)) stop("Username is blank.")
  if (!role %in% c("coach", "player")) stop("Role must be 'coach' or 'player', not '", role, "'.")
  if (is.na(password) || nchar(password) < 6) stop("Password for ", username, " must be at least 6 characters.")

  users <- users[tolower(users$username) != tolower(username), , drop = FALSE]
  rbind(users, data.frame(
    username = username, name = name, role = role,
    hash = password_store(password), stringsAsFactors = FALSE
  ))
}

# Adds a login. If the username already exists it is replaced.
add_user <- function(username, name, role, password) {
  users <- .add_one(.read_users(), username, name, role, password)
  .write_users(users)
  message("Saved login for ", username, " (", tolower(role), ").")
  invisible(TRUE)
}

# Bulk import. The CSV needs columns username,name,role,password.
add_roster <- function(csv_path) {
  roster <- read_csv(csv_path, col_types = cols(.default = "c"), progress = FALSE)
  need <- c("username", "name", "role", "password")
  missing <- setdiff(need, names(roster))
  if (length(missing)) stop("Roster is missing columns: ", paste(missing, collapse = ", "))

  users <- .read_users()
  for (i in seq_len(nrow(roster))) {
    users <- .add_one(users, roster$username[i], roster$name[i], roster$role[i], roster$password[i])
  }
  .write_users(users)
  message("Saved ", nrow(roster), " logins. Delete the plain-text roster file now.")
  invisible(TRUE)
}

remove_user <- function(username) {
  users <- .read_users()
  keep <- tolower(users$username) != tolower(trimws(username))
  if (all(keep)) {
    message("No login named ", username, ".")
    return(invisible(FALSE))
  }
  .write_users(users[keep, , drop = FALSE])
  message("Removed ", username, ".")
  invisible(TRUE)
}
