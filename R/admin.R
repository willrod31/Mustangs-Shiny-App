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
