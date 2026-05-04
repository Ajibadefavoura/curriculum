# ============================================================
# NeutPipeline — modules/mod_curves.R
# ============================================================

mod_curves_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title = "Dose Response Curves", width = 12,
      status = "primary", solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(3,
          shiny::selectInput(ns("sel_serotype"), "Serotypes",
                             choices = NULL, multiple = TRUE),
          shiny::selectInput(ns("sel_sample"), "Samples",
                             choices = NULL, multiple = TRUE),
          shiny::radioButtons(ns("x_scale"), "X Axis Scale",
                              choices  = c("Log10"  = "log",
                                           "Linear" = "linear"),
                              selected = "log"),
          shiny::radioButtons(ns("color_by"), "Color Curves By",
                              choices  = c("Serotype"  = "serotype",
                                           "Sample ID" = "sample_id"),
                              selected = "serotype"),
          shiny::checkboxInput(ns("show_ic50_line"),
                               "Show IC50 Marker Line", value = TRUE),
          shiny::checkboxInput(ns("show_50pct"),
                               "Show 50% Reference Line", value = TRUE),
          shiny::checkboxInput(ns("show_reps"),
                               "Show Individual Replicates", value = FALSE),
          shiny::downloadButton(ns("dl_curves"), "Export Curves")
        ),
        shiny::column(9,
          plotly::plotlyOutput(ns("curve_plot"), height = "580px")
        )
      )
    ),
    # ── TASK 10 — GraphPad Prism style panel ──────────────
    shinydashboard::box(
      title = "Prism Style 4PL Curves", width = 12, status = "info", solidHeader = TRUE,
      shiny::plotOutput(ns("prism_curve_plot"), height = "640px")
    )
  )
}

mod_curves_server <- function(id, avg_data, concs, conc_units, cfg, fit_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      req(avg_data())
      shiny::updateSelectInput(
        session, "sel_serotype",
        choices  = sort(unique(avg_data()$serotype)),
        selected = sort(unique(avg_data()$serotype))
      )
      shiny::updateSelectInput(
        session, "sel_sample",
        choices  = sort(unique(avg_data()$sample_id)),
        selected = sort(unique(avg_data()$sample_id))
      )
    })

    filtered <- reactive({
      req(avg_data(), input$sel_serotype, input$sel_sample)
      avg_data() %>%
        dplyr::filter(serotype %in% input$sel_serotype,
                      sample_id %in% input$sel_sample)
    })

    smooth_curves <- reactive({
      req(filtered())
      x_range <- 10^seq(log10(min(concs()) * 0.3),
                        log10(max(concs()) * 3),
                        length.out = 300)
      filtered() %>%
        dplyr::group_by(serotype, sample_id) %>%
        dplyr::group_modify(~ {
          x <- .x$concentration
          y <- .x$pct_neut_avg
          keep <- is.finite(x) & x > 0 & is.finite(y)
          x <- x[keep]; y <- y[keep]

          fit <- if (length(x) >= 3) {
            tryCatch({
              invisible(capture.output(
                capture.output(
                  m <- suppressMessages(suppressWarnings(
                    drc::drm(
                      y ~ x,
                      fct  = drc::LL.2(upper = 100),
                      data = data.frame(x = x, y = y),
                      lowerl  = c(-20, min(x) / 10),
                      upperl  = c(20,  max(x) * 10),
                      control = drc::drmc(noMessage = TRUE)
                    )
                  )),
                  type = "message"
                ),
                type = "output"
              ))
              m
            },
              error = function(e) NULL
            )
          } else {
            NULL
          }

          if (!is.null(fit)) {
            data.frame(x_smooth = x_range,
                       y_smooth = predict(fit,
                                          newdata = data.frame(x = x_range)))
          } else {
            data.frame(x_smooth = x_range, y_smooth = NA_real_)
          }
        }) %>%
        dplyr::ungroup()
    })

    # ── TASK 9 — High-resolution interactive curve plot ─────
    output$curve_plot <- plotly::renderPlotly({
      req(filtered(), smooth_curves())
      color_var <- input$color_by

      p <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = filtered(),
          ggplot2::aes(
            x     = concentration,
            y     = pct_neut_avg,
            color = .data[[color_var]],
            text  = glue::glue(
              "Sample: {sample_id}\nSerotype: {serotype}",
              "\nConc: {round(concentration, 2)} {conc_units()}",
              "\n%Neut: {round(pct_neut_avg, 1)}%"
            )
          ),
          size = 2.5, alpha = 0.9
        ) +
        ggplot2::geom_line(
          data = smooth_curves() %>% dplyr::filter(!is.na(y_smooth)),
          ggplot2::aes(x = x_smooth, y = y_smooth,
                       color = .data[[color_var]]),
          linewidth = 0.9
        ) +
        { if (input$show_50pct)
            ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
                                color = "grey40", linewidth = 0.5) } +
        ggplot2::scale_y_continuous(limits = c(-5, 108),
                                   name = "% Neutralization") +
        ggplot2::labs(
          x     = glue::glue("Concentration ({conc_units()})"),
          color = stringr::str_to_title(gsub("_", " ", color_var))
        ) +
        ggplot2::facet_wrap(~ serotype) +
        ggplot2::theme_classic(base_size = 12)

      if (input$x_scale == "log") {
        p <- p + ggplot2::scale_x_log10(
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        )
      }

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2))
    })

    # ── TASK 10 — GraphPad Prism aesthetic dose-response ────
    # Curve fitting math is unchanged (same drc::LL.2 pipeline
    # used by smooth_curves / fit_data); ONLY ggplot styling
    # is overridden here to mimic Prism.
    prism_curve_gg <- reactive({
      req(filtered(), smooth_curves())

      pts   <- filtered()
      lines <- smooth_curves() %>% dplyr::filter(!is.na(y_smooth))

      ic50_marks <- if (!is.null(fit_data) && !is.null(fit_data())) {
        fit_data() %>%
          dplyr::filter(serotype %in% input$sel_serotype,
                        sample_id %in% input$sel_sample,
                        is.finite(ic50)) %>%
          dplyr::distinct(serotype, sample_id, ic50)
      } else {
        NULL
      }

      p <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = concentration, y = pct_neut_avg,
                       color = sample_id, shape = sample_id),
          size = 2.6, stroke = 0.7
        ) +
        ggplot2::geom_line(
          data = lines,
          ggplot2::aes(x = x_smooth, y = y_smooth, color = sample_id),
          linewidth = 1
        ) +
        ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
                            color = "black", linewidth = 0.5)

      if (!is.null(ic50_marks) && nrow(ic50_marks) > 0) {
        p <- p +
          ggplot2::geom_segment(
            data = ic50_marks,
            ggplot2::aes(x = ic50, xend = ic50, y = 0, yend = 50,
                         color = sample_id),
            linetype = "dashed", linewidth = 0.5,
            inherit.aes = FALSE,
            show.legend = FALSE
          )
      }

      p +
        ggplot2::scale_x_log10(
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) +
        ggplot2::scale_y_continuous(
          breaks = c(0, 20, 40, 60, 80, 100),
          limits = c(0, 110)
        ) +
        ggplot2::facet_wrap(~ serotype) +
        ggplot2::labs(
          x     = "ng/mL, purified mAb",
          y     = "% Neutralized",
          color = "Sample",
          shape = "Sample"
        ) +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::theme(
          axis.line   = ggplot2::element_line(color = "black", linewidth = 0.7),
          axis.ticks  = ggplot2::element_line(color = "black", linewidth = 0.5),
          axis.text   = ggplot2::element_text(face = "bold", color = "black"),
          axis.title  = ggplot2::element_text(face = "bold", color = "black"),
          strip.background = ggplot2::element_blank(),
          strip.text  = ggplot2::element_text(face = "bold", size = 13),
          legend.position = "right",
          legend.title    = ggplot2::element_text(face = "bold")
        )
    })

    output$prism_curve_plot <- shiny::renderPlot({
      req(prism_curve_gg())
      prism_curve_gg()
    }, res = 300)

    # ── TASK 9 — High-res download for the Prism panel ──────
    output$dl_curves <- shiny::downloadHandler(
      filename = function() glue::glue("dose_response_prism_{Sys.Date()}.png"),
      content  = function(file) {
        ggplot2::ggsave(file, plot = prism_curve_gg(),
                        width = 12, height = 8, dpi = 300, bg = "white")
      }
    )
  })
}
