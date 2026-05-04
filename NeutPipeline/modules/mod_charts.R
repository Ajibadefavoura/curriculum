# ============================================================
# NeutPipeline — modules/mod_charts.R
# ============================================================

mod_charts_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title = "Chart Controls", width = 12, status = "primary", solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(inputId = ns("sel_serotype"), label = "Serotypes to Include", choices = NULL, multiple = TRUE)),
        shiny::column(3, shiny::radioButtons(inputId = ns("group_by"), label = "Group Bars By",
                                             choices = c("Sample ID" = "sample_id", "Serotype" = "serotype"),
                                             selected = "sample_id")),
        shiny::column(3, shiny::radioButtons(inputId = ns("fill_by"), label = "Color Bars By",
                                             choices = c("Serotype" = "serotype", "QC Status" = "qc_status"),
                                             selected = "serotype"),
                      shiny::checkboxInput(inputId = ns("show_errorbars"), label = "Show 95% CI Error Bars", value = TRUE)),
        shiny::column(3,
                      tags$br(),
                      shiny::downloadButton(outputId = ns("dl_layout1"), label = "Export Layout 1 (PNG)"),
                      tags$br(), tags$br(),
                      shiny::downloadButton(outputId = ns("dl_layout2"), label = "Export Layout 2 (PNG)"),
                      tags$br(), tags$br(),
                      shiny::downloadButton(outputId = ns("dl_layout2_potency"), label = "Export Potency Bar Chart (PNG)"))
      )
    ),
    shinydashboard::box(
      title = "Layout 1 — IC50 (ng/mL) | Taller Bar = Higher IC50 = Less Potent",
      width = 12, status = "info", solidHeader = TRUE,
      plotly::plotlyOutput(outputId = ns("layout1_plot"), height = "400px")
    ),
    shinydashboard::box(
      title = "Layout 2 — 1 / IC50 Potency | Taller Bar = Lower IC50 = More Potent",
      width = 12, status = "success", solidHeader = TRUE,
      plotly::plotlyOutput(outputId = ns("layout2_plot"), height = "400px")
    ),
    # ── TASK 3 — Layout 2 Prism-style potency bar chart ────
    shinydashboard::box(
      title = "Layout 2 — 1 / IC50 Potency (Prism Style)",
      width = 12, status = "success", solidHeader = TRUE,
      shiny::plotOutput(outputId = ns("layout2_potency_plot"), height = "520px")
    )
  )
}

mod_charts_server <- function(id, summary_data, conc_units, ic50_cap) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      req(summary_data())
      serotypes <- sort(unique(summary_data()$serotype))
      shiny::updateSelectInput(session = session, inputId = "sel_serotype",
                               choices = serotypes, selected = serotypes)
    })

    chart_data <- reactive({
      req(summary_data(), input$sel_serotype)
      summary_data() %>% dplyr::filter(serotype %in% input$sel_serotype)
    })

    layout1_gg <- reactive({
      req(chart_data())
      d <- chart_data()
      ggplot2::ggplot(
        data = d,
        ggplot2::aes(
          x = reorder(.data[[input$group_by]], ic50_display),
          y = ic50_display,
          fill = .data[[input$fill_by]],
          text = glue::glue("Sample: {sample_id}\nSerotype: {serotype}\nIC50: {ic50_display} {conc_units()}\nQC: {qc_status}")
        )
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
        { if (input$show_errorbars && !all(is.na(d$ci_lower))) {
            ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                                   position = ggplot2::position_dodge(width = 0.8),
                                   width = 0.25, linewidth = 0.5, color = "grey30")
        } } +
        ggplot2::geom_hline(yintercept = ic50_cap(), linetype = "dashed",
                            color = "red", linewidth = 0.6) +
        ggplot2::annotate(geom = "text", x = -Inf, y = ic50_cap() * 1.04,
                          label = glue::glue("Cap: {ic50_cap()} {conc_units()}"),
                          color = "red", hjust = -0.1, size = 3) +
        ggplot2::scale_y_continuous(labels = scales::comma) +
        ggplot2::labs(x = NULL,
                      y = glue::glue("IC50 ({conc_units()})"),
                      fill = if (input$fill_by == "serotype") "Serotype" else "QC Status") +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
                       legend.position = "bottom")
    })

    output$layout1_plot <- plotly::renderPlotly({
      req(layout1_gg())
      plotly::ggplotly(layout1_gg(), tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.35, x = 0.5, xanchor = "center"))
    })

    layout2_gg <- reactive({
      req(chart_data())
      d <- chart_data()
      ggplot2::ggplot(
        data = d,
        ggplot2::aes(
          x = reorder(.data[[input$group_by]], -inv_ic50_display),
          y = inv_ic50_display,
          fill = .data[[input$fill_by]],
          text = glue::glue("Sample: {sample_id}\nSerotype: {serotype}\n1/IC50: {round(inv_ic50_display, 6)}\nIC50: {ic50_display} {conc_units()}\nQC: {qc_status}")
        )
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
        ggplot2::labs(x = NULL, y = "1 / IC50 (Potency)",
                      fill = if (input$fill_by == "serotype") "Serotype" else "QC Status") +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
                       legend.position = "bottom")
    })

    output$layout2_plot <- plotly::renderPlotly({
      req(layout2_gg())
      plotly::ggplotly(layout2_gg(), tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.35, x = 0.5, xanchor = "center"))
    })

    # ── TASK 3 — Prism-style 1/IC50 potency bar chart ───────
    layout2_potency_gg <- reactive({
      req(chart_data())

      d <- chart_data() %>%
        # Drop VC and Mock controls; we only care about real samples.
        dplyr::filter(
          !is.na(sample_id),
          !grepl("^vc$",   sample_id, ignore.case = TRUE),
          !grepl("^mock$", sample_id, ignore.case = TRUE)
        ) %>%
        dplyr::mutate(
          # Strip every non-numeric symbol so "> 20000", "20,000",
          # "✗ 20000 ng/mL" etc. all become a clean number.
          ic50_clean   = gsub("[^[:digit:].eE+-]", "", as.character(ic50_display)),
          numeric_ic50 = suppressWarnings(as.numeric(ic50_clean))
        ) %>%
        dplyr::mutate(
          # Inactive / NA / failed samples get capped to ic50_cap
          # so they appear as zero-potency bars instead of disappearing.
          numeric_ic50 = dplyr::if_else(
            is.na(numeric_ic50) |
              grepl("Inactive", as.character(qc_status), ignore.case = TRUE) |
              grepl("FAIL",     as.character(qc_status), ignore.case = TRUE),
            as.numeric(ic50_cap()),
            numeric_ic50
          ),
          numeric_ic50 = dplyr::if_else(numeric_ic50 <= 0, as.numeric(ic50_cap()), numeric_ic50),
          potency      = 1 / numeric_ic50
        )

      shiny::validate(shiny::need(nrow(d) > 0, "No sample data available for the potency chart."))

      ggplot2::ggplot(
        data = d,
        ggplot2::aes(
          x    = reorder(sample_id, -potency),
          y    = potency,
          fill = serotype
        )
      ) +
        ggplot2::geom_col(
          position = ggplot2::position_dodge(width = 0.8),
          width    = 0.72,
          color    = "black",
          linewidth = 0.4
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::label_scientific(digits = 2),
          expand = ggplot2::expansion(mult = c(0, 0.08))
        ) +
        ggplot2::labs(
          x     = NULL,
          y     = "1 / IC50  (Potency)",
          fill  = "Serotype",
          title = "Antibody Potency by Serotype"
        ) +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(face = "bold", hjust = 0),
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1,
                                                   face = "bold", color = "black"),
          axis.text.y      = ggplot2::element_text(face = "bold", color = "black"),
          axis.title.y     = ggplot2::element_text(face = "bold"),
          axis.line        = ggplot2::element_line(color = "black", linewidth = 0.6),
          axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.5),
          legend.position  = "right",
          legend.title     = ggplot2::element_text(face = "bold")
        )
    })

    # ── TASK 9 — High resolution renderPlot ──────────────────
    output$layout2_potency_plot <- shiny::renderPlot({
      req(layout2_potency_gg())
      layout2_potency_gg()
    }, res = 300)

    # ── TASK 9 — High resolution downloads ───────────────────
    output$dl_layout1 <- shiny::downloadHandler(
      filename = function() { glue::glue("layout1_IC50_{Sys.Date()}.png") },
      content  = function(file) {
        ggplot2::ggsave(filename = file, plot = layout1_gg(),
                        width = 14, height = 7, dpi = 300, bg = "white")
      }
    )
    output$dl_layout2 <- shiny::downloadHandler(
      filename = function() { glue::glue("layout2_invIC50_{Sys.Date()}.png") },
      content  = function(file) {
        ggplot2::ggsave(filename = file, plot = layout2_gg(),
                        width = 14, height = 7, dpi = 300, bg = "white")
      }
    )
    output$dl_layout2_potency <- shiny::downloadHandler(
      filename = function() { glue::glue("layout2_potency_prism_{Sys.Date()}.png") },
      content  = function(file) {
        ggplot2::ggsave(filename = file, plot = layout2_potency_gg(),
                        width = 12, height = 7, dpi = 300, bg = "white")
      }
    )
  })
}
