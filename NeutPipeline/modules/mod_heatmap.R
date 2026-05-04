# ============================================================
# NeutPipeline — modules/mod_heatmap.R
# ============================================================

mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title  = "96 Well Plate FFU Spatial Heatmap", width  = 12,
      status = "primary", solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(3,
                      shiny::selectInput(ns("sel_serotype"), "Serotype", choices = NULL),
                      shiny::selectInput(ns("sel_plate"), "Plate", choices = NULL),
                      shiny::radioButtons(ns("metric"), "Heatmap Metric",
                                          choices = c("Percent Neutralization (Lab Palette)" = "neut",
                                                      "Raw FFU Counts"                       = "ffu"),
                                          selected = "neut"),
                      shiny::downloadButton(ns("dl_heatmap"), "Export Heatmap")
        ),
        shiny::column(9, plotly::plotlyOutput(ns("heatmap_plot"), height = "480px"))
      )
    ),
    # ── TASK 2 — Dynamic & collective heatmap (DENV1-4) ─────
    shinydashboard::box(
      title  = "Collective Neutralization Heatmap (All Serotypes x Samples)",
      width  = 12, status = "danger", solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(9,
                      shiny::plotOutput(ns("collective_heatmap"), height = "560px")),
        shiny::column(3,
                      tags$p("Aggregated max %neutralization per (serotype, sample). Higher = deeper red.",
                             style = "font-size:12px; color:#6E6E73;"),
                      shiny::checkboxInput(ns("show_neut_labels"),
                                           "Show numeric labels", value = TRUE),
                      shiny::downloadButton(ns("dl_collective"), "Export Collective Heatmap"))
      )
    ),
    shinydashboard::box(title = "VC and Mock Statistics for Selected Plate",
                        width = 12, status = "info", solidHeader = TRUE,
                        DT::DTOutput(ns("plate_stats")))
  )
}

mod_heatmap_server <- function(id, parsed_data, neut_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      req(parsed_data())
      serotypes <- sort(unique(parsed_data()$serotype))
      shiny::updateSelectInput(session, "sel_serotype",
                               choices = serotypes, selected = serotypes[1])
    })

    shiny::observe({
      req(input$sel_serotype, parsed_data())
      plates <- sort(unique(parsed_data()$plate[parsed_data()$serotype == input$sel_serotype]))
      shiny::updateSelectInput(session, "sel_plate", choices = plates, selected = plates[1])
    })

    plate_data <- reactive({
      req(parsed_data(), input$sel_serotype, input$sel_plate)
      parsed_data() %>%
        dplyr::filter(serotype == input$sel_serotype, plate == as.integer(input$sel_plate))
    })

    plate_with_neut <- reactive({
      req(plate_data())
      pd <- plate_data()
      if (input$metric == "neut" && !is.null(neut_data) && !is.null(neut_data())) {
        nd <- neut_data() %>%
          dplyr::filter(serotype == input$sel_serotype,
                        plate    == as.integer(input$sel_plate)) %>%
          dplyr::select(serotype, plate, well_row, well_col, pct_neut)
        pd <- pd %>%
          dplyr::left_join(nd, by = c("serotype", "plate", "well_row", "well_col"))
      }
      pd
    })

    heatmap_gg <- reactive({
      req(plate_with_neut())
      pd <- plate_with_neut()

      if (input$metric == "neut" && "pct_neut" %in% names(pd)) {
        pd <- pd %>% dplyr::mutate(fill_value = pmax(0, pmin(100, pct_neut)) / 100,
                                   fill_label = sprintf("%.0f%%", pct_neut))
        fill_scale <- ggplot2::scale_fill_gradientn(
          colours  = neutpipeline_heatmap_colors,
          values   = neutpipeline_heatmap_values,
          limits   = c(0, 1),
          breaks   = c(0, 0.25, 0.5, 0.75, 0.9, 1),
          labels   = c("0%", "25%", "50%", "75%", "90%", "100%"),
          name     = "% Neut",
          na.value = "#FFFFFF"
        )
      } else {
        max_ffu <- max(pd$ffu_count, na.rm = TRUE)
        if (!is.finite(max_ffu) || max_ffu <= 0) max_ffu <- 1
        pd <- pd %>% dplyr::mutate(fill_value = ffu_count,
                                   fill_label = as.character(ffu_count))
        fill_scale <- ggplot2::scale_fill_gradientn(
          colours  = rev(neutpipeline_heatmap_colors),
          values   = neutpipeline_heatmap_values,
          name     = "FFU",
          na.value = "#FFFFFF"
        )
      }

      ggplot2::ggplot(
        pd,
        ggplot2::aes(
          x = factor(well_col, levels = 1:12),
          y = factor(well_row, levels = rev(LETTERS[1:8])),
          fill = fill_value,
          text = glue::glue(
            "Sample: {sample_id}\nWell: {well_row}{well_col}",
            "\nConc: {round(concentration, 1)}\nFFU count: {ffu_count}"
          )
        )
      ) +
        ggplot2::geom_tile(color = "white", linewidth = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = fill_label), size = 3, color = "black") +
        fill_scale +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_discrete(drop = FALSE) +
        ggplot2::labs(
          title = glue::glue("{input$sel_serotype} \u2014 Plate {input$sel_plate} \u2014 Spatial Layout"),
          x = "Column (1-12)", y = "Row (A-H)"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    })

    # ── TASK 3 — High-resolution interactive heatmap ────────
    output$heatmap_plot <- plotly::renderPlotly({
      req(heatmap_gg())
      plotly::ggplotly(heatmap_gg(), tooltip = "text")
    })

    # ── TASK 2 — Collective neutralization heatmap ──────────
    # Aggregates the maximum %neut per (serotype, sample_id) across
    # ALL uploaded plates so DENV1-4 are visible in one panel.
    collective_gg <- reactive({
      shiny::req(neut_data, !is.null(neut_data()))
      nd <- neut_data() %>%
        dplyr::filter(
          !is.na(sample_id),
          !grepl("^vc$",   sample_id, ignore.case = TRUE),
          !grepl("^mock$", sample_id, ignore.case = TRUE)
        )

      shiny::validate(shiny::need(
        nrow(nd) > 0,
        "No neutralization data available for the collective heatmap."
      ))

      agg <- nd %>%
        dplyr::group_by(serotype, sample_id) %>%
        dplyr::summarise(
          max_pct_neut = max(pct_neut, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          fill_value = pmax(0, pmin(100, max_pct_neut)) / 100,
          fill_label = ifelse(is.finite(max_pct_neut),
                              sprintf("%.0f", max_pct_neut), "")
        )

      sample_levels <- unique(agg$sample_id)
      ctrl_priority <- grepl("^ede|c8|c10", sample_levels, ignore.case = TRUE)
      sample_levels <- c(sample_levels[ctrl_priority],
                         sort(sample_levels[!ctrl_priority]))

      p <- ggplot2::ggplot(
        agg,
        ggplot2::aes(
          x    = factor(sample_id, levels = sample_levels),
          y    = serotype,
          fill = fill_value
        )
      ) +
        ggplot2::geom_tile(color = "white", linewidth = 0.6)

      if (isTRUE(input$show_neut_labels)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = fill_label),
          size = 3.2, fontface = "bold", color = "black"
        )
      }

      p +
        ggplot2::scale_fill_gradientn(
          colours  = neutpipeline_heatmap_colors,
          values   = neutpipeline_heatmap_values,
          limits   = c(0, 1),
          breaks   = c(0, 0.25, 0.5, 0.75, 0.9, 1),
          labels   = c("0%", "25%", "50%", "75%", "90%", "100%"),
          name     = "% Neut",
          na.value = "#EBEBEB"
        ) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = "Collective % Neutralization \u2014 All Serotypes \u00d7 Samples"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title       = ggplot2::element_text(face = "bold"),
          axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1,
                                                   face = "bold", color = "black"),
          axis.text.y      = ggplot2::element_text(face = "bold", color = "black"),
          panel.grid       = ggplot2::element_blank(),
          legend.position  = "right",
          legend.title     = ggplot2::element_text(face = "bold")
        )
    })

    output$collective_heatmap <- shiny::renderPlot({
      req(collective_gg())
      collective_gg()
    }, res = 300)

    output$plate_stats <- DT::renderDT({
      req(plate_data())
      plate_data() %>%
        dplyr::distinct(serotype, plate, vc_avg, vc_sd, vc_cv_pct, mock_avg) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2))) %>%
        prettify_colnames()
    }, options = list(dom = "t"), rownames = FALSE)

    # ── TASK 3 — High-resolution heatmap exports ────────────
    output$dl_heatmap <- shiny::downloadHandler(
      filename = function() glue::glue("heatmap-{input$sel_serotype}-plate{input$sel_plate}-{Sys.Date()}.png"),
      content  = function(file) ggplot2::ggsave(file, plot = heatmap_gg(),
                                                width = 14, height = 8,
                                                dpi = 300, bg = "white")
    )

    output$dl_collective <- shiny::downloadHandler(
      filename = function() glue::glue("collective-heatmap-{Sys.Date()}.png"),
      content  = function(file) ggplot2::ggsave(file, plot = collective_gg(),
                                                width = 14, height = 8,
                                                dpi = 300, bg = "white")
    )
  })
}
