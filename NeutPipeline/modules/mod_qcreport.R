# ============================================================
# NeutPipeline — modules/mod_qcreport.R
# ============================================================

mod_qcreport_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::valueBoxOutput(outputId = ns("box_pass"), width = 3),
      shinydashboard::valueBoxOutput(outputId = ns("box_fail"), width = 3),
      shinydashboard::valueBoxOutput(outputId = ns("box_ambig"), width = 3),
      shinydashboard::valueBoxOutput(outputId = ns("box_inact"), width = 3)
    ),
    shinydashboard::box(
      title = "QC Status Matrix — All Samples Across All Serotypes",
      width = 12, status = "primary", solidHeader = TRUE,
      shiny::fluidRow(shiny::column(4, shiny::downloadButton(outputId = ns("dl_matrix"), label = "Download QC Matrix XLSX"))),
      tags$br(), DT::DTOutput(outputId = ns("qc_matrix"))
    ),
    shinydashboard::box(title = "Plate Level Warnings", width = 12,
                        status = "warning", solidHeader = TRUE,
                        DT::DTOutput(outputId = ns("plate_warnings"))),
    shinydashboard::box(
      title = "Full QC Detail — All Results", width = 12, status = "info", solidHeader = TRUE,
      shiny::fluidRow(shiny::column(4, shiny::downloadButton(outputId = ns("dl_full"), label = "Download Full QC XLSX"))),
      tags$br(), DT::DTOutput(outputId = ns("qc_detail"))
    )
  )
}

mod_qcreport_server <- function(id, qc_data, neut_data, experiment_name, analyst_name) {
  shiny::moduleServer(id, function(input, output, session) {
    output$box_pass  <- shinydashboard::renderValueBox({
      req(qc_data()); n <- sum(grepl("PASS", qc_data()$qc_status))
      shinydashboard::valueBox(value = n, subtitle = "PASS",
                               icon = shiny::icon("check-circle"), color = "green")
    })
    output$box_fail  <- shinydashboard::renderValueBox({
      req(qc_data()); n <- sum(grepl("FAIL", qc_data()$qc_status))
      shinydashboard::valueBox(value = n, subtitle = "FAIL",
                               icon = shiny::icon("times-circle"), color = "red")
    })
    output$box_ambig <- shinydashboard::renderValueBox({
      req(qc_data()); n <- sum(grepl("Ambiguous", qc_data()$qc_status))
      shinydashboard::valueBox(value = n, subtitle = "Ambiguous",
                               icon = shiny::icon("exclamation-triangle"), color = "yellow")
    })
    output$box_inact <- shinydashboard::renderValueBox({
      req(qc_data()); n <- sum(grepl("Inactive", qc_data()$qc_status))
      shinydashboard::valueBox(value = n, subtitle = "Inactive",
                               icon = shiny::icon("minus-circle"), color = "black")
    })

    qc_matrix_data <- reactive({
      req(qc_data())
      qc_data() %>%
        dplyr::mutate(cell_label = dplyr::case_when(
          grepl("Inactive",  qc_status) ~ "Inactive",
          grepl("FAIL",      qc_status) &
            ic50_classification == "Solver pathology" ~ "ND (fit error)",
          grepl("FAIL",      qc_status) ~ paste0(ic50_censored, " \u274c"),
          grepl("Ambiguous", qc_status) ~ paste0("~", ic50_censored, " \u26a0"),
          TRUE ~ as.character(ic50_censored)
        )) %>%
        dplyr::select(sample_id, serotype, cell_label) %>%
        tidyr::pivot_wider(names_from = serotype, values_from = cell_label) %>%
        dplyr::arrange(sample_id)
    })

    output$qc_matrix <- DT::renderDT({
      req(qc_matrix_data())
      DT::datatable(prettify_colnames(qc_matrix_data()), rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE, pageLength = 50))
    })

    output$plate_warnings <- DT::renderDT({
      req(neut_data())
      warnings_df <- neut_data() %>%
        dplyr::distinct(serotype, plate, vc_avg, vc_cv_pct, mock_avg, plate_vc_flag, plate_mock_flag) %>%
        dplyr::filter(plate_vc_flag != "OK" | plate_mock_flag != "OK") %>%
        dplyr::mutate(vc_avg = round(vc_avg, 1),
                      vc_cv_pct = round(vc_cv_pct, 1),
                      mock_avg = round(mock_avg, 1)) %>%
        dplyr::rename(Serotype = serotype, Plate = plate,
                      "VC Mean" = vc_avg,
                      "VC CV (%)" = vc_cv_pct,
                      "Mock Mean" = mock_avg,
                      "VC Warning" = plate_vc_flag,
                      "Mock Warning" = plate_mock_flag) %>%
        dplyr::arrange(Serotype, Plate)
      if (nrow(warnings_df) == 0) {
        warnings_df <- data.frame(Message = "All plates passed plate level QC. No VC or Mock warnings.")
      }
      DT::datatable(warnings_df, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE))
    })

    output$qc_detail <- DT::renderDT({
      req(qc_data())
      d <- qc_data()
      have_class <- "ic50_classification" %in% names(d)
      have_rep   <- "ic50_report" %in% names(d)
      cols_pick <- c("sample_id", "serotype", "plate",
                     if (have_rep) "ic50_report",
                     "ic50_display",
                     if (have_class) "ic50_classification",
                     "hill_slope", "r_squared", "ci_lower", "ci_upper",
                     "fit_status", "ic50_type",
                     "qc_status", "qc_detail", "flags")
      detail_df <- d %>%
        dplyr::select(dplyr::any_of(cols_pick)) %>%
        dplyr::mutate(hill_slope = round(hill_slope, 3),
                      r_squared  = round(r_squared, 3),
                      ci_lower   = round(ci_lower, 2),
                      ci_upper   = round(ci_upper, 2)) %>%
        dplyr::arrange(serotype, sample_id)
      rename_map <- c("Sample" = "sample_id", "Serotype" = "serotype",
                      "Plate" = "plate", "IC50 Reported" = "ic50_report",
                      "IC50" = "ic50_display",
                      "IC50 Classification" = "ic50_classification",
                      "Hill Slope" = "hill_slope",
                      "R Squared" = "r_squared",
                      "CI Lower" = "ci_lower", "CI Upper" = "ci_upper",
                      "Fit Status" = "fit_status", "IC50 Type" = "ic50_type",
                      "QC Status" = "qc_status", "QC Detail" = "qc_detail",
                      "Flags" = "flags")
      detail_df <- detail_df %>%
        dplyr::rename(dplyr::any_of(rename_map))
      DT::datatable(detail_df, filter = "top", rownames = FALSE,
                    options = list(pageLength = 25, scrollX = TRUE)) %>%
        DT::formatStyle(columns = "QC Status",
                        backgroundColor = DT::styleEqual(
                          c("\u2705 PASS", "\u274c FAIL", "\u26a0 Ambiguous", "\u26ab Inactive"),
                          c("#d4edda", "#f8d7da", "#fff3cd", "#e2e3e5")))
    })

    # ── TASK 1 — XLSX downloads, ASCII-clean, bold Calibri ──
    output$dl_matrix <- shiny::downloadHandler(
      filename = function() {
        exp <- gsub("[^A-Za-z0-9-]+", "-", experiment_name() %||% "")
        glue::glue("qc-matrix-{exp}-{Sys.Date()}.xlsx")
      },
      content  = function(file) {
        write_neut_xlsx(qc_matrix_data(), file, default_sheet = "QC Matrix")
      }
    )
    output$dl_full <- shiny::downloadHandler(
      filename = function() {
        exp <- gsub("[^A-Za-z0-9-]+", "-", experiment_name() %||% "")
        glue::glue("qc-full-detail-{exp}-{Sys.Date()}.xlsx")
      },
      content  = function(file) {
        write_neut_xlsx(qc_data(), file, default_sheet = "QC Detail")
      }
    )
  })
}
