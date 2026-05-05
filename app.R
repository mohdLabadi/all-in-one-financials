# Deployment entrypoint for platforms that require app.R.
# Load the existing app definitions, then return a standard Shiny app object.
source("app_market.R", local = TRUE)
shiny::shinyApp(ui = ui, server = server)
