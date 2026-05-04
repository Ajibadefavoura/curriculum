# ============================================================
# NeutPipeline — modules/mod_ic50table.R
# ============================================================

mod_ic50table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title = "IC50 Results", width = 12, status = "primary", solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(4, shiny::radioButtons(ns("table_view"), "Table Layout",
                                             choices = c("Wide Cross Serotype Matrix" = "wide",
                                                         "Long One Row Per Result"     = "long"),
                                             selected = "wide")),
        shiny::column(4, shiny::checkboxGroupInput(ns("show_cols"), "Columns to Show",
                                                   choices = c("IC50"="ic50",
                                                               "1 / IC50"="inv_ic50",
                                                               "Hill Slope"="hill_slope",
                                                               "R Squared"="r_squared",
                                                               "95% CI"="ci",
                                                               "QC Status"="qc_status",
                                                               "IC50 Type"="ic50_type",
                                                               "Flags"="flags"),
                                                   selected = c("ic50","inv_ic50","qc_status","ic50_type"))),
        shiny::column(4, tags$br(),
                      shiny::downloadButton(ns("dl_long"), "Download Long XLSX"),
                      tags$br(), tags$br(),
                      shiny::downloadButton(ns("dl_wide"), "Download Matrix XLSX"))
      ),
      DT::DTOutput(ns("ic50_table"))
    )
  )
}

mod_ic50table_server <- function(id, summary_data, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    display_table <- reactive({
      req(summary_data())
      d <- summary_data()
      base_cols  <- c("sample_id", "serotype", "plate", "ic50_report")
      extra_cols <- c()
      if ("ic50" %in% input$show_cols) extra_cols <- c(extra_cols, "ic50_display")
      if ("inv_ic50" %in% input$show_cols) extra_cols <- c(extra_cols, "inv_ic50_display")
      if ("hill_slope" %in% input$show_cols) extra_cols <- c(extra_cols, "hill_slope")
      if ("r_squared" %in% input$show_cols) extra_cols <- c(extra_cols, "r_squared")
      if ("ci" %in% input$show_cols) extra_cols <- c(extra_cols, "ci_lower", "ci_upper")
      if ("qc_status" %in% input$show_cols) extra_cols <- c(extra_cols, "qc_status")
      if ("ic50_type" %in% input$show_cols) extra_cols <- c(extra_cols, "ic50_type", "ic50_classification")
      if ("flags" %in% input$show_cols) extra_cols <- c(extra_cols, "flags")

      d <- d %>% dplyr::select(dplyr::any_of(c(base_cols, extra_cols)))
      if (input$table_view == "wide") {
        pivot_ic50_wide(summary_data())
      } else {
        d
      }
    })

    output$ic50_table <- DT::renderDT({
      req(display_table())
      DT::datatable(prettify_colnames(display_table()), filter = "top", rownames = FALSE)
    })

    # ── TASK 1 — XLSX downloads, ASCII-clean, bold Calibri ──
    output$dl_long <- shiny::downloadHandler(
      filename = function() glue::glue("ic50-long-{Sys.Date()}.xlsx"),
      content  = function(file) {
        write_neut_xlsx(summary_data(), file, default_sheet = "IC50 Long")
      }
    )
    output$dl_wide <- shiny::downloadHandler(
      filename = function() glue::glue("ic50-matrix-{Sys.Date()}.xlsx"),
      content  = function(file) {
        write_neut_xlsx(
          pivot_ic50_wide(summary_data()),
          file,
          default_sheet = "IC50 Matrix"
        )
      }
    )
  })
}
