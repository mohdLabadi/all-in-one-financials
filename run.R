# Bootstrap script for Docker: run Shiny app on host 0.0.0.0 and platform PORT
port <- as.numeric(Sys.getenv("PORT", 3838))
shiny::runApp("/app/app_market.R", host = "0.0.0.0", port = port, launch.browser = FALSE)
