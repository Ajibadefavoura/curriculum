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
                      shiny::selectInput(ns("color_palette"), "Color Scale",
                                         choices = c("White to Red"  = "Reds",
                                                     "White to Blue" = "Blues",
                                                     "Viridis"       = "viridis",
                                                     "Yellow to Red" = "YlOrRd")),
                      shiny::downloadButton(ns("dl_heatmap"), "Export Heatmap")
        ),
        shiny::column(9, plotly::plotlyOutput(ns("heatmap_plot"), height = "480px"))
      )
    ),
    shinydashboard::box(title = "VC and Mock Statistics for Selected Plate",
                        width = 12, status = "info", solidHeader = TRUE,
                        DT::DTOutput(ns("plate_stats")))
  )
}

mod_heatmap_server <- function(id, parsed_data) {
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

    heatmap_gg <- reactive({
      req(plate_data())
      ggplot2::ggplot(plate_data(),
                      ggplot2::aes(x = factor(well_col, levels = 1:12),
                                   y = factor(well_row, levels = rev(LETTERS[1:8])),
                                   fill = ffu_count,
                                   text = glue::glue("Sample: {sample_id}\nWell: {well_row}{well_col}\nConc: {round(concentration, 1)}\nFFU count: {ffu_count}"))) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::geom_text(ggplot2::aes(label = ffu_count), size = 3, color = "black") +
        { if (input$color_palette == "viridis")
            ggplot2::scale_fill_viridis_c(name = "FFU Count", na.value = "grey80")
          else
            ggplot2::scale_fill_distiller(palette = input$color_palette, direction = 1,
                                          name = "FFU Count", na.value = "grey80") } +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_y_discrete(drop = FALSE) +
        ggplot2::labs(title = glue::glue("{input$sel_serotype} — Plate {input$sel_plate} — Spatial Layout"),
                      x = "Column (1-12)", y = "Row (A-H)") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
    })

    # ── TASK 9 — High-resolution rendering ──────────────────
    output$heatmap_plot <- plotly::renderPlotly({
      req(heatmap_gg())
      plotly::ggplotly(heatmap_gg(), tooltip = "text")
    })

    output$plate_stats <- DT::renderDT({
      req(plate_data())
      plate_data() %>%
        dplyr::distinct(serotype, plate, vc_avg, vc_sd, vc_cv_pct, mock_avg) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2))) %>%
        prettify_colnames()
    }, options = list(dom = "t"), rownames = FALSE)

    output$dl_heatmap <- shiny::downloadHandler(
      filename = function() glue::glue("heatmap_{input$sel_serotype}_P{input$sel_plate}_{Sys.Date()}.png"),
      content  = function(file) ggplot2::ggsave(file, plot = heatmap_gg(),
                                                width = 14, height = 8, dpi = 300, bg = "white")
    )
  })
}
