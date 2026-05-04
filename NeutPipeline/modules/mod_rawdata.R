# ============================================================
# NeutPipeline — modules/mod_rawdata.R
# ============================================================

mod_rawdata_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      title = "Plate Map Confirmation", width = 12, status = "primary", solidHeader = TRUE,
      shiny::fluidRow(shiny::column(3, shiny::selectInput(ns("preview_plate"), "Select Plate", choices = NULL))),
      plotly::plotlyOutput(ns("plate_map_preview"), height = "320px")
    ),
    shinydashboard::box(
      title = "Raw FFU Data", width = 12, status = "info", solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(4, shiny::selectInput(ns("filter_serotype"), "Serotype", choices = NULL, multiple = TRUE)),
        shiny::column(4, shiny::selectInput(ns("filter_plate"), "Plate", choices = NULL, multiple = TRUE)),
        shiny::column(4, tags$br(), shiny::downloadButton(ns("dl_raw"), "Download as XLSX"))
      ),
      DT::DTOutput(ns("raw_table"))
    ),
    shinydashboard::box(title = "VC and Mock Summary", width = 12,
                        status = "warning", solidHeader = TRUE,
                        DT::DTOutput(ns("vc_mock_summary")))
  )
}

mod_rawdata_server <- function(id, parsed_data, plate_map, neut_data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Populate filter selectors after Run Analysis (depends on parsed_data).
    shiny::observe({
      req(parsed_data())
      serotypes <- sort(unique(parsed_data()$serotype))
      plates    <- sort(unique(parsed_data()$plate))
      shiny::updateSelectInput(session, "filter_serotype", choices = serotypes, selected = serotypes)
      shiny::updateSelectInput(session, "filter_plate",    choices = plates,    selected = plates)
    })

    # Plate-map preview should be available *before* Run Analysis
    # so the analyst can confirm the layout before kicking off the
    # heavy pipeline. Driven directly by plate_map() (live).
    shiny::observe({
      req(plate_map())
      plates <- sort(unique(plate_map()$plate))
      shiny::updateSelectInput(session, "preview_plate", choices = plates,
                               selected = plates[1])
    })

    output$plate_map_preview <- plotly::renderPlotly({
      req(plate_map(), input$preview_plate)
      pm <- plate_map() %>% dplyr::filter(plate == as.integer(input$preview_plate))
      grid_df <- expand.grid(row = LETTERS[1:8], col = 1:12, stringsAsFactors = FALSE) %>%
        dplyr::mutate(label = "", group = "Empty")

      for (i in seq_len(nrow(pm))) {
        rows  <- get_row_range(pm$row_start[i], pm$row_end[i])
        cols  <- if (pm$half[i] == "left") 1:6 else if (pm$half[i] == "right") 7:12 else 1:12
        sample_label <- dplyr::case_when(pm$is_vc[i] ~ "VC",
                                         pm$is_mock[i] ~ "Mock",
                                         TRUE ~ pm$sample_id[i])
        grid_df <- grid_df %>% dplyr::mutate(
          label = ifelse(row %in% rows & col %in% cols, .env$sample_label, label),
          group = ifelse(row %in% rows & col %in% cols, .env$sample_label, group)
        )
      }

      p <- ggplot2::ggplot(grid_df,
                           ggplot2::aes(x = col,
                                        y = factor(row, levels = rev(LETTERS[1:8])),
                                        fill = group,
                                        text = glue::glue("Position: {row}{col}\nSample: {label}"))) +
        ggplot2::geom_tile(color = "white", linewidth = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = label), size = 2.5, color = "black") +
        ggplot2::scale_x_continuous(breaks = 1:12) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid = ggplot2::element_blank())
      plotly::ggplotly(p, tooltip = "text")
    })

    filtered_raw <- reactive({
      req(parsed_data(), input$filter_serotype, input$filter_plate)
      parsed_data() %>%
        dplyr::filter(serotype %in% input$filter_serotype,
                      plate    %in% as.integer(input$filter_plate)) %>%
        dplyr::select(serotype, plate, sample_id, concentration, replicate,
                      ffu_count, vc_avg, vc_cv_pct, mock_avg) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
    })

    output$raw_table <- DT::renderDT({
      req(filtered_raw())
      DT::datatable(prettify_colnames(filtered_raw()), filter = "top", rownames = FALSE)
    })

    output$vc_mock_summary <- DT::renderDT({
      req(parsed_data())
      parsed_data() %>%
        dplyr::distinct(serotype, plate, vc_avg, vc_sd, vc_cv_pct, mock_avg) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 1))) %>%
        prettify_colnames()
    }, options = list(dom = "t"), rownames = FALSE)

    # ── TASK 1 — XLSX download, ASCII-clean, bold Calibri ───
    output$dl_raw <- shiny::downloadHandler(
      filename = function() glue::glue("raw-ffu-{Sys.Date()}.xlsx"),
      content  = function(file) {
        write_neut_xlsx(filtered_raw(), file, default_sheet = "Raw FFU")
      }
    )
  })
}
