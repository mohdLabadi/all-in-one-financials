# Run this file to start the Daily Market app (same pattern as 06_alphavantage_ai_report.R).
# In RStudio: open this file, then click "Source".
# In R console: setwd("path/to/5381tool1"); source("run_market.R")

# 1) Rscript / VS Code Code Runner: working directory is often NOT this folder — use --file=
args_all <- commandArgs(trailingOnly = FALSE)
file_line <- grep("^--file=", args_all, value = TRUE)
if (length(file_line) > 0L) {
  script_path <- sub("^--file=", "", file_line[1])
  script_path <- normalizePath(script_path, winslash = "/", mustWork = FALSE)
  if (!is.na(script_path) && nzchar(script_path) && file.exists(script_path)) setwd(dirname(script_path))
}

# 2) If still not in app folder (e.g. sourced from elsewhere), try common locations relative to getwd()
if (!file.exists("app_market.R")) {
  candidates <- c(
    file.path(getwd(), "5381tool1"),
    file.path(getwd(), "tool 1"),
    file.path(dirname(getwd()), "5381tool1"),
    file.path(dirname(getwd()), "tool 1"),
    "tool 1"
  )
  for (p in candidates) {
    p2 <- tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
    if (!is.na(p2) && nzchar(p2) && file.exists(file.path(p2, "app_market.R"))) {
      setwd(p2)
      break
    }
  }
}

# 3) Legacy absolute path (optional)
if (!file.exists("app_market.R")) {
  app_path <- "C:/Users/10543/OneDrive/Desktop/cornell_courses/5831/tool 1"
  if (dir.exists(app_path)) setwd(app_path)
}

if (!file.exists("app_market.R")) {
  stop("app_market.R not found. Run from 5381tool1 or open run_market.R and Source so wd can be set.")
}
if (!file.exists(".env")) message("Warning: .env not found. Create .env with ALPHAVANTAGE_API_KEY=yourkey")

message("Working directory: ", getwd())
message("Starting Daily Market app...")
shiny::runApp("app_market.R", launch.browser = TRUE)
