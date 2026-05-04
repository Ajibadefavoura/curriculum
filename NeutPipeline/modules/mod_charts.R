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
      d <- chart_data() %>%
        dplyr::mutate(
          ic50_plot = pmax(.Machine$double.eps,
                           pmin(as.numeric(ic50_display),
                                as.numeric(ic50_cap())))
        )

      # Use a log10 Y axis so capped (20 000) and quantified
      # samples (e.g. 4 ng/mL) are simultaneously visible
      # instead of compressing every bar onto the cap line.
      y_min <- max(.Machine$double.eps,
                   min(d$ic50_plot[d$ic50_plot > 0], na.rm = TRUE) / 3)
      y_max <- as.numeric(ic50_cap()) * 1.4

      ggplot2::ggplot(
        data = d,
        ggplot2::aes(
          x = reorder(.data[[input$group_by]], ic50_plot),
          y = ic50_plot,
          fill = .data[[input$fill_by]],
          text = glue::glue("Sample: {sample_id}\nSerotype: {serotype}\nIC50: {ic50_display} {conc_units()}\nQC: {qc_status}")
        )
      ) +
        ggplot2::geom_col(
          position  = ggplot2::position_dodge(width = 0.8, preserve = "single"),
          width     = 0.72, alpha = 0.92,
          color     = "black", linewidth = 0.35
        ) +
        ggplot2::geom_hline(
          yintercept = ic50_cap(),
          linetype   = "dashed", color = "#DC2626", linewidth = 0.5
        ) +
        ggplot2::annotate(
          geom = "text", x = 0.6, y = ic50_cap() * 1.05,
          label = glue::glue("Cap: {ic50_cap()} {conc_units()}"),
          color = "#DC2626", hjust = 0, size = 3.2, fontface = "bold"
        ) +
        ggplot2::scale_y_log10(
          labels = scales::label_number(big.mark = ",",
                                        scale_cut = scales::cut_short_scale()),
          limits = c(y_min, y_max),
          expand = ggplot2::expansion(mult = c(0, 0.05))
        ) +
        ggplot2::labs(
          x    = NULL,
          y    = glue::glue("IC50 ({conc_units()}) - log scale"),
          fill = if (input$fill_by == "serotype") "Serotype" else "QC Status"
        ) +
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                              size = 10, color = "black"),
          axis.text.y = ggplot2::element_text(color = "black"),
          axis.line   = ggplot2::element_line(color = "black", linewidth = 0.5),
          panel.grid  = ggplot2::element_blank(),
          legend.position = "bottom"
        )
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
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8, preserve = "single"),
                          width = 0.7, alpha = 0.9, color = "black", linewidth = 0.3) +
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
          numeric_ic50 = suppressWarnings(as.numeric(ic50)),
          # Capped, censored, and solver-pathology rows all become
          # the cap value so that "low potency" still shows up as
          # a small bar instead of vanishing from the plot.
          numeric_ic50 = dplyr::if_else(
            !is.finite(numeric_ic50) |
              grepl("Inactive", as.character(qc_status), ignore.case = TRUE) |
              grepl("FAIL",     as.character(qc_status), ignore.case = TRUE),
            as.numeric(ic50_cap()),
            numeric_ic50
          ),
          numeric_ic50 = dplyr::if_else(numeric_ic50 <= 0,
                                        as.numeric(ic50_cap()),
                                        numeric_ic50),
          potency      = 1 / numeric_ic50
        )

      shiny::validate(shiny::need(nrow(d) > 0, "No sample data available for the potency chart."))

      # Establish a consistent sample ordering: EDE / control samples
      # first (if present), then samples in their natural input
      # order. EDE C8/C10 broadly-neutralizing controls anchor the
      # leftmost group, matching the publication-style reference
      # layout.
      sample_levels <- d %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(
          max_potency = max(potency, na.rm = TRUE),
          is_ctrl     = any(grepl("^ede|c8|c10", sample_id, ignore.case = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(is_ctrl), sample_id) %>%
        dplyr::pull(sample_id)

      # Reference lines on the 1/IC50 axis correspond to round IC50
      # values at 100, 1,000 and 10,000 ng/mL — matching the
      # publication-style "IC50 Summary (Y = 1/IC50)" reference
      # panel.
      ref_lines <- data.frame(
        ic50  = c(100, 1000, 10000),
        label = c("IC50 = 100", "IC50 = 1,000", "IC50 = 10,000")
      )
      ref_lines$y <- 1 / ref_lines$ic50

      # Compute axis range *from the data* so that every bar is
      # visible — capped samples (potency = 1/20000 = 5e-5) used to
      # be clipped because the lower limit was hard-coded at 1e-4.
      pos_potency <- d$potency[is.finite(d$potency) & d$potency > 0]
      y_min <- if (length(pos_potency) > 0) {
        min(c(pos_potency, 1 / as.numeric(ic50_cap()))) * 0.5
      } else {
        1e-6
      }
      y_max <- if (length(pos_potency) > 0) max(pos_potency) * 1.5 else 1
      log_breaks <- 10 ^ seq(floor(log10(y_min)), ceiling(log10(y_max)))

      ggplot2::ggplot(
        data = d,
        ggplot2::aes(
          x    = factor(sample_id, levels = sample_levels),
          y    = potency,
          fill = serotype
        )
      ) +
        ggplot2::geom_col(
          position  = ggplot2::position_dodge(width = 0.8, preserve = "single"),
          width     = 0.72,
          color     = "black",
          linewidth = 0.4
        ) +
        ggplot2::geom_hline(
          yintercept = ref_lines$y,
          linetype   = "dashed",
          color      = "grey30",
          linewidth  = 0.4
        ) +
        ggplot2::annotate(
          "text",
          x = 0.7, y = ref_lines$y, label = ref_lines$label,
          hjust = 0, vjust = -0.4, size = 3.4, color = "grey25",
          fontface = "bold"
        ) +
        ggplot2::scale_y_log10(
          labels = scales::label_number(),
          breaks = log_breaks,
          limits = c(y_min, y_max),
          expand = ggplot2::expansion(mult = c(0, 0.05))
        ) +
        ggplot2::labs(
          x     = NULL,
          y     = "1 / IC50  (Potency)",
          fill  = "Serotype",
          title = "IC50 Summary  (Y = 1 / IC50)"
        ) +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5, size = 15),
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1,
                                                   face = "bold", color = "black"),
          axis.text.y      = ggplot2::element_text(face = "bold", color = "black"),
          axis.title.y     = ggplot2::element_text(face = "bold"),
          axis.line        = ggplot2::element_line(color = "black", linewidth = 0.6),
          axis.ticks       = ggplot2::element_line(color = "black", linewidth = 0.5),
          panel.grid       = ggplot2::element_blank(),
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
      filename = function() { glue::glue("layout-1-ic50-{Sys.Date()}.png") },
      content  = function(file) {
        ggplot2::ggsave(filename = file, plot = layout1_gg(),
                        width = 14, height = 7, dpi = 300, bg = "white")
      }
    )
    output$dl_layout2 <- shiny::downloadHandler(
      filename = function() { glue::glue("layout-2-inverse-ic50-{Sys.Date()}.png") },
      content  = function(file) {
        ggplot2::ggsave(filename = file, plot = layout2_gg(),
                        width = 14, height = 7, dpi = 300, bg = "white")
      }
    )
    output$dl_layout2_potency <- shiny::downloadHandler(
      filename = function() { glue::glue("ic50-summary-potency-{Sys.Date()}.png") },
      content  = function(file) {
        ggplot2::ggsave(filename = file, plot = layout2_potency_gg(),
                        width = 12, height = 7, dpi = 300, bg = "white")
      }
    )
  })
}
