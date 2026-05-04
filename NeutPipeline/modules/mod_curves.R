# ============================================================
# NeutPipeline — modules/mod_curves.R
# ============================================================

mod_curves_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # ── Interactive panel ───────────────────────────────────
    shinydashboard::box(
      title  = "Dose Response Curves (Interactive)",
      width  = 12, status = "primary", solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(3,
          shiny::selectInput(ns("sel_serotype"), "Serotypes",
                             choices = NULL, multiple = TRUE),
          shiny::selectInput(ns("sel_sample"), "Samples",
                             choices = NULL, multiple = TRUE),
          shiny::radioButtons(ns("facet_mode"), "Facet By",
                              choices  = c(
                                "Sample"      = "by_sample",
                                "Plate"       = "by_plate",
                                "Serotype"    = "by_serotype",
                                "None (overlay)" = "none"
                              ),
                              selected = "by_sample"),
          shiny::checkboxInput(ns("show_50pct"),
                               "Show 50% reference line", value = TRUE),
          shiny::checkboxInput(ns("show_ic50_marks"),
                               "Show IC50 drop lines", value = TRUE),
          shiny::downloadButton(ns("dl_curves"), "Export Curves (PNG)")
        ),
        shiny::column(9,
          plotly::plotlyOutput(ns("curve_plot"), height = "640px")
        )
      )
    ),
    # ── Publication-style overall grid ──────────────────────
    shinydashboard::box(
      title = "Publication Style 4PL Curves (per Sample)", width = 12,
      status = "info", solidHeader = TRUE,
      shiny::plotOutput(ns("prism_curve_plot"), height = "780px")
    ),
    # ── Per-plate panel (always rendered) ───────────────────
    shinydashboard::box(
      title = "Curves per Plate", width = 12,
      status = "info", solidHeader = TRUE,
      shiny::plotOutput(ns("plate_curve_plot"), height = "520px"),
      shiny::downloadButton(ns("dl_plate_curves"), "Export Per-Plate (PNG)",
                            style = "margin-top:10px;")
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

      try_fit <- function(spec) {
        tryCatch({
          invisible(capture.output(
            capture.output(
              m <- suppressMessages(suppressWarnings(do.call(drc::drm, spec))),
              type = "message"
            ),
            type = "output"
          ))
          m
        }, error = function(e) NULL)
      }

      filtered() %>%
        dplyr::group_by(serotype, plate, sample_id) %>%
        dplyr::group_modify(~ {
          x <- .x$concentration
          y <- .x$pct_neut_avg
          keep <- is.finite(x) & x > 0 & is.finite(y)
          x <- x[keep]; y <- y[keep]

          if (length(x) < 3) {
            return(data.frame(x_smooth = x_range, y_smooth = NA_real_))
          }

          base_data <- data.frame(x = x, y = y)
          fit_specs <- list(
            list(formula = y ~ x, fct = drc::LL.2(upper = 100), data = base_data,
                 lowerl = c(-20, min(x) / 10), upperl = c(20, max(x) * 10),
                 control = drc::drmc(noMessage = TRUE)),
            list(formula = y ~ x,
                 fct = drc::LL.4(fixed = c(NA, NA, 100, NA)),
                 data = base_data,
                 control = drc::drmc(method = "L-BFGS-B", noMessage = TRUE)),
            list(formula = y ~ x, fct = drc::LL.4(), data = base_data,
                 control = drc::drmc(method = "Nelder-Mead", noMessage = TRUE))
          )
          fit <- NULL
          for (spec in fit_specs) {
            fit <- try_fit(spec)
            if (!is.null(fit)) break
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

    ic50_marks <- reactive({
      if (is.null(fit_data) || is.null(fit_data())) return(NULL)
      fit_data() %>%
        dplyr::filter(
          serotype  %in% input$sel_serotype,
          sample_id %in% input$sel_sample,
          is.finite(ic50), ic50 > 0,
          ic50 < cfg()$qc$ic50_cap
        ) %>%
        dplyr::distinct(serotype, sample_id, ic50)
    })

    # ── Facet helper ────────────────────────────────────────
    add_facet <- function(p, mode, ncol_v = 5) {
      switch(
        mode,
        by_sample   = p + ggplot2::facet_wrap(~ sample_id, ncol = ncol_v),
        by_plate    = p + ggplot2::facet_wrap(~ paste0("Plate ", plate),
                                              ncol = ncol_v),
        by_serotype = p + ggplot2::facet_wrap(~ serotype),
        p
      )
    }

    # ── Theme used by static publication panels ─────────────
    prism_theme <- function(base = 13) {
      ggplot2::theme_classic(base_size = base) +
        ggplot2::theme(
          axis.line   = ggplot2::element_line(color = "black", linewidth = 0.7),
          axis.ticks  = ggplot2::element_line(color = "black", linewidth = 0.5),
          axis.ticks.length = grid::unit(4, "pt"),
          axis.text   = ggplot2::element_text(face = "bold", color = "black",
                                              size = base - 2),
          axis.title  = ggplot2::element_text(face = "bold", color = "black",
                                              size = base + 1),
          panel.grid  = ggplot2::element_blank(),
          panel.spacing = grid::unit(0.7, "lines"),
          strip.background = ggplot2::element_blank(),
          strip.text  = ggplot2::element_text(face = "bold",
                                              size = base - 1, color = "black",
                                              hjust = 0),
          legend.position = "right",
          legend.title    = ggplot2::element_text(face = "bold")
        )
    }

    # ── Interactive plotly chart ────────────────────────────
    # IMPORTANT: plotly cannot render plotmath expressions, so
    # the X axis uses plain "1, 10, 100, 1k, 10k" labels rather
    # than "10^x" expressions which previously rendered as a
    # literal caret in the browser.
    output$curve_plot <- plotly::renderPlotly({
      req(filtered(), smooth_curves())

      pts   <- filtered()
      lines <- smooth_curves() %>% dplyr::filter(!is.na(y_smooth))
      color_var <- if (input$facet_mode == "by_sample") "serotype" else "sample_id"

      p <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = concentration, y = pct_neut_avg,
                       color = .data[[color_var]],
                       text  = glue::glue(
                         "Sample: {sample_id}\nSerotype: {serotype}",
                         "\nConc: {round(concentration, 2)} {conc_units()}",
                         "\n%Neut: {round(pct_neut_avg, 1)}%"
                       )),
          size = 2.2, alpha = 0.9
        ) +
        ggplot2::geom_line(
          data = lines,
          ggplot2::aes(x = x_smooth, y = y_smooth,
                       color = .data[[color_var]]),
          linewidth = 0.9
        ) +
        { if (isTRUE(input$show_50pct))
            ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
                                color = "grey40", linewidth = 0.4) } +
        ggplot2::scale_x_log10(
          labels = scales::label_number(
            scale_cut = scales::cut_short_scale(),
            accuracy = 0.1
          )
        ) +
        ggplot2::scale_y_continuous(
          breaks = c(0, 20, 40, 60, 80, 100),
          limits = c(-5, 110)
        ) +
        ggplot2::labs(
          x     = "ng/mL, purified mAb",
          y     = "% Neutralized",
          color = stringr::str_to_title(gsub("_", " ", color_var))
        ) +
        prism_theme(12)

      p <- add_facet(p, input$facet_mode, ncol_v = 5)

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(legend = list(orientation = "h", y = -0.18))
    })

    # ── Per-sample static panel (publication style) ─────────
    prism_curve_gg <- reactive({
      req(filtered(), smooth_curves())

      pts   <- filtered()
      lines <- smooth_curves() %>% dplyr::filter(!is.na(y_smooth))
      color_var <- "serotype"  # always color by serotype within a sample panel

      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = lines,
          ggplot2::aes(x = x_smooth, y = y_smooth,
                       color = .data[[color_var]]),
          linewidth = 1
        ) +
        ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = concentration, y = pct_neut_avg,
                       color = .data[[color_var]]),
          size = 2.4, stroke = 0.6
        ) +
        ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
                            color = "black", linewidth = 0.4)

      if (isTRUE(input$show_ic50_marks) &&
          !is.null(ic50_marks()) && nrow(ic50_marks()) > 0) {
        p <- p +
          ggplot2::geom_segment(
            data = ic50_marks(),
            ggplot2::aes(x = ic50, xend = ic50, y = 0, yend = 50,
                         color = .data[[color_var]]),
            linetype = "dashed", linewidth = 0.4,
            inherit.aes = FALSE, show.legend = FALSE
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
        ggplot2::labs(
          x     = "ng/mL, purified mAb",
          y     = "% Neutralized",
          color = "Serotype"
        ) +
        ggplot2::facet_wrap(~ sample_id, ncol = 5) +
        prism_theme(13)
    })

    output$prism_curve_plot <- shiny::renderPlot({
      req(prism_curve_gg())
      prism_curve_gg()
    }, res = 300)

    # ── Per-plate dose-response (always rendered) ───────────
    plate_curve_gg <- reactive({
      req(filtered(), smooth_curves())

      pts   <- filtered()
      lines <- smooth_curves() %>% dplyr::filter(!is.na(y_smooth))

      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = lines,
          ggplot2::aes(x = x_smooth, y = y_smooth,
                       color = sample_id, group = interaction(sample_id, serotype)),
          linewidth = 0.9
        ) +
        ggplot2::geom_point(
          data = pts,
          ggplot2::aes(x = concentration, y = pct_neut_avg,
                       color = sample_id),
          size = 1.8, alpha = 0.9
        ) +
        ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
                            color = "black", linewidth = 0.4) +
        ggplot2::scale_x_log10(
          labels = scales::trans_format("log10", scales::math_format(10^.x))
        ) +
        ggplot2::scale_y_continuous(
          breaks = c(0, 20, 40, 60, 80, 100),
          limits = c(0, 110)
        ) +
        ggplot2::labs(
          x     = "ng/mL, purified mAb",
          y     = "% Neutralized",
          color = "Sample"
        ) +
        ggplot2::facet_wrap(~ paste0("Plate ", plate)) +
        prism_theme(12) +
        ggplot2::theme(legend.position = "right",
                       legend.text = ggplot2::element_text(size = 10))

      p
    })

    output$plate_curve_plot <- shiny::renderPlot({
      req(plate_curve_gg())
      plate_curve_gg()
    }, res = 300)

    # ── Downloads ───────────────────────────────────────────
    output$dl_curves <- shiny::downloadHandler(
      filename = function() glue::glue("dose-response-curves-{Sys.Date()}.png"),
      content  = function(file) {
        n_panels <- length(unique(filtered()$sample_id))
        ncols    <- 5
        nrows    <- max(1, ceiling(n_panels / ncols))
        ggplot2::ggsave(file, plot = prism_curve_gg(),
                        width  = 2.6 * ncols + 2,
                        height = 2.4 * nrows + 1,
                        dpi = 300, bg = "white", limitsize = FALSE)
      }
    )
    output$dl_plate_curves <- shiny::downloadHandler(
      filename = function() glue::glue("dose-response-per-plate-{Sys.Date()}.png"),
      content  = function(file) {
        n_plates <- length(unique(filtered()$plate))
        ggplot2::ggsave(file, plot = plate_curve_gg(),
                        width  = 4.5 * min(3, n_plates) + 2,
                        height = 4 * ceiling(n_plates / 3) + 1,
                        dpi = 300, bg = "white", limitsize = FALSE)
      }
    )
  })
}
