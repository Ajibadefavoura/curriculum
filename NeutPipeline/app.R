# ============================================================
# NeutPipeline — app.R
# ============================================================
# This entrypoint sources global.R, ui.R and server.R so the
# project can be launched with `shiny::runApp("NeutPipeline")`
# from the workspace root, or `shiny::runApp(".")` from inside
# the NeutPipeline folder.

source("global.R", local = FALSE)
source("ui.R",     local = FALSE)
source("server.R", local = FALSE)

shiny::shinyApp(ui = ui, server = server)
