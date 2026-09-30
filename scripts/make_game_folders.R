# Creates reports/games/<game_id>/coaches/ for every game in data/schedule.csv.
# Never deletes anything. Run from the app folder after filling in the schedule:
#   Rscript scripts/make_game_folders.R

suppressPackageStartupMessages(library(dplyr))
source("R/config.R")
source("R/auth.R")
source("R/data.R")
source("R/schedule.R")

schedule <- load_schedule()
if (!nrow(schedule)) stop("No games found in ", SCHEDULE_FILE)

for (gid in schedule$game_id) {
  dir.create(game_folder(gid, coaches_only = TRUE), recursive = TRUE, showWarnings = FALSE)
}

out <- data.frame(
  date = format(schedule$date, "%Y-%m-%d"),
  game = schedule$matchup,
  folder = paste0(game_folder(schedule$game_id), "/")
)
print(out, row.names = FALSE, right = FALSE)
cat("\nPut team reports in the folder. Coaches-only files go in its coaches/ subfolder.\n")
cat("File names decide the tab: 'ump' = Umpire, 'pitch' = Pitcher, 'hit' or 'bat' = Hitter.\n")
