# Deploys the app to shinyapps.io. Run from the app folder: source("deploy.R")
#
# One-time setup: log in at https://www.shinyapps.io, open Account > Tokens,
# click "Show", copy the rsconnect::setAccountInfo(name = ..., token = ...,
# secret = ...) line and run it once in the R console. Do not save the token
# in this file or anywhere else in the repo.

rsconnect::deployApp(
  appDir = ".",
  appName = "mustangs-report-hub",
  appFiles = c("app.R", "R", "data", "reports"),
  forceUpdate = TRUE
)
