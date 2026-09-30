# Manage logins in data/users.csv.
# Run from the app folder:
#   source("scripts/add_user.R")
#   add_user("jsmith", "John Smith", "player", "a-strong-password")
#   add_user("will", "Will Rodriguez", "admin", "a-strong-password")
#   add_user("jhollis", "Jace Hollis", "player", "a-strong-password", tm_name = "Hollis, Jace")
#   add_roster("roster.csv")   # columns: username,name,role,password (tm_name optional)
#   remove_user("jsmith")
#
# The owner login (willrod31, admin) is created automatically by the app the
# first time it starts, so it doesn't need to be added here.
#
# Passwords are hashed with sodium before they are saved.
# Never commit data/users.csv or a plain-text roster file.

library(readr)
library(sodium)

USERS_FILE <- "data/users.csv"

.read_users <- function() {
  if (!file.exists(USERS_FILE)) {
    return(data.frame(username = character(), name = character(),
                      role = character(), tm_name = character(), hash = character(),
                      stringsAsFactors = FALSE))
  }
  users <- as.data.frame(read_csv(USERS_FILE, col_types = cols(.default = "c"), progress = FALSE),
                         stringsAsFactors = FALSE)
  if (!"tm_name" %in% names(users)) users$tm_name <- users$name
  users[, c("username", "name", "role", "tm_name", "hash")]
}

.write_users <- function(users) {
  dir.create(dirname(USERS_FILE), showWarnings = FALSE, recursive = TRUE)
  write_csv(users, USERS_FILE, na = "")
}

.add_one <- function(users, username, name, role, password, tm_name = name) {
  username <- trimws(username)
  role <- tolower(trimws(role))
  if (!nzchar(username)) stop("Username is blank.")
  if (is.na(tm_name) || !nzchar(trimws(tm_name))) tm_name <- name
  role <- match.arg(role, c("admin", "coach", "player"))
  if (is.na(password) || nchar(password) < 6) stop("Password for ", username, " must be at least 6 characters.")

  users <- users[tolower(users$username) != tolower(username), , drop = FALSE]
  rbind(users, data.frame(
    username = username, name = name, role = role, tm_name = trimws(tm_name),
    hash = password_store(password), stringsAsFactors = FALSE
  ))
}

# Adds a login. If the username already exists it is replaced.
# tm_name is how TrackMan spells the player, often "Last, First".
add_user <- function(username, name, role, password, tm_name = name) {
  users <- .add_one(.read_users(), username, name, role, password, tm_name)
  .write_users(users)
  message("Saved login for ", username, " (", tolower(role), ").")
  invisible(TRUE)
}

# Bulk import. The CSV needs columns username,name,role,password.
# An optional tm_name column sets each player's TrackMan name.
add_roster <- function(csv_path) {
  roster <- read_csv(csv_path, col_types = cols(.default = "c"), progress = FALSE)
  need <- c("username", "name", "role", "password")
  missing <- setdiff(need, names(roster))
  if (length(missing)) stop("Roster is missing columns: ", paste(missing, collapse = ", "))

  if (!"tm_name" %in% names(roster)) roster$tm_name <- NA_character_
  users <- .read_users()
  for (i in seq_len(nrow(roster))) {
    users <- .add_one(users, roster$username[i], roster$name[i], roster$role[i],
                      roster$password[i], roster$tm_name[i])
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
