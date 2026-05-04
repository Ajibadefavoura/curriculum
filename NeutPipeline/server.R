# ============================================================
# NeutPipeline — server.R
# ============================================================

server <- function(input, output, session) {

  output$dilution_factor_display <- renderText({
    df <- compute_dilution_factor(input$vol_antibody, input$vol_virus)
    glue::glue("Dilution factor: {round(df, 3)}x")
  })

  # ── DYNAMIC Plate Template Generator ──────────────────────
  output$dl_plate_map_template <- downloadHandler(
    filename = function() { glue::glue("plate_map_template_{Sys.Date()}.csv") },
    content  = function(file) {

      if (!is.null(input$master_excel_file)) {
        sheets <- readxl::excel_sheets(input$master_excel_file$datapath)
        # Same dynamic regex as parse_master_excel (Task 1).
        valid_pattern <- "^(.+?)[ _]+(?:P(?:late)?[ _]?)?(\\d+)$"
        m <- regmatches(sheets, regexec(valid_pattern, sheets, ignore.case = TRUE, perl = TRUE))
        extracted <- sapply(m[lengths(m) > 0], function(x) x[3])
        if (length(extracted) > 0) {
          plates <- sort(unique(as.integer(extracted)))
        } else { plates <- 1:3 }
      } else { plates <- 1:3 }

      n_steps <- input$n_steps

      df_list <- lapply(plates, function(p) {
        if (n_steps > 6) {
          data.frame(
            plate     = p,
            sample_id = c(paste0("Sample_", p, "_1"), paste0("Sample_", p, "_2"), paste0("Sample_", p, "_3"), "VC", "Mock"),
            row_start = c("A", "C", "E", cfg$plate_layout$vc_row_default, cfg$plate_layout$mock_row_default),
            row_end   = c("B", "D", "F", cfg$plate_layout$vc_row_default, cfg$plate_layout$mock_row_default),
            half      = "full",
            is_vc     = c(FALSE, FALSE, FALSE, TRUE, FALSE),
            is_mock   = c(FALSE, FALSE, FALSE, FALSE, TRUE),
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            plate     = rep(p, 8),
            sample_id = c(paste0("Sample_", p, "_", 1:3), "VC", "Mock", paste0("Sample_", p, "_", 4:6)),
            row_start = c("A", "C", "E", cfg$plate_layout$vc_row_default, cfg$plate_layout$mock_row_default, "A", "C", "E"),
            row_end   = c("B", "D", "F", cfg$plate_layout$vc_row_default, cfg$plate_layout$mock_row_default, "B", "D", "F"),
            half      = c(rep("left", 5), rep("right", 3)),
            is_vc     = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
            is_mock   = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
            stringsAsFactors = FALSE
          )
        }
      })

      out <- sanitize_for_export(do.call(rbind, df_list))
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ── Live UI Configuration ─────────────────────────
  live_cfg <- reactive({
    req(input$min_hill)
    c <- cfg
    c$curve_fitting$model       <- input$model_type
    c$curve_fitting$bottom      <- input$bottom_constraint
    c$curve_fitting$top         <- input$top_constraint
    c$curve_fitting$free_bottom <- input$free_bottom
    c$curve_fitting$free_top    <- input$free_top
    c$curve_fitting$sample_type <- input$sample_type
    c$qc$min_hillslope          <- input$min_hill
    c$qc$min_r2_mab             <- input$min_r2_mab
    c$qc$min_r2_poly            <- input$min_r2_poly
    c$qc$ic50_cap               <- input$ic50_cap
    c$qc$max_vc_cv_pct          <- input$max_vc_cv
    c$qc$min_max_neut_pct       <- input$min_max_neut
    c
  })

  # ── TASK 4 — Reactive concentration series ─────────────────
  # Using a reactive() (not eventReactive) keeps the dilution
  # inputs *live*: every change to start_conc, dilution_fold,
  # n_steps, vol_antibody, vol_virus or conc_convention is
  # propagated immediately to the parsing / fitting pipeline.
  conc_series <- reactive({
    req(input$start_conc, input$dilution_fold, input$n_steps,
        input$vol_antibody, input$vol_virus, input$conc_convention)
    concs_prepared <- build_conc_series(
      input$start_conc, input$dilution_fold, input$n_steps
    )
    df <- compute_dilution_factor(input$vol_antibody, input$vol_virus)
    apply_conc_convention(concs_prepared, df, input$conc_convention)
  })

  # ── Plate map (live, used both for preview and analysis) ───
  plate_map_data <- reactive({
    if (!is.null(input$plate_map_file)) {
      pm <- read.csv(input$plate_map_file$datapath, stringsAsFactors = FALSE)
    } else if (!is.null(input$master_excel_file)) {
      sheets <- readxl::excel_sheets(input$master_excel_file$datapath)
      pm_sheet <- sheets[tolower(sheets) == "plate_map"]
      if (length(pm_sheet) > 0) {
        pm <- as.data.frame(readxl::read_excel(input$master_excel_file$datapath, sheet = pm_sheet[1]))
      } else { return(NULL) }
    } else { return(NULL) }

    # ── TASK 2 — CSV boolean coercion ────────────────────────
    # Excel/CSV exports `TRUE`/`FALSE` (and even "True", "false",
    # "1"/"0") as strings. Force them back to logical booleans so
    # all downstream filters (`is_vc == TRUE`, `is_mock == TRUE`)
    # work regardless of the source file.
    pm <- pm %>% dplyr::mutate(
      plate     = as.integer(plate),
      row_start = toupper(trimws(row_start)),
      row_end   = toupper(trimws(row_end)),
      half      = tolower(trimws(half)),
      is_vc     = as.logical(toupper(as.character(is_vc))),
      is_mock   = as.logical(toupper(as.character(is_mock)))
    )

    # Numeric "1"/"0" gets coerced to NA by toupper -> as.logical;
    # patch those edge cases so downstream code still works.
    pm$is_vc[is.na(pm$is_vc)]     <- FALSE
    pm$is_mock[is.na(pm$is_mock)] <- FALSE

    validation <- validate_plate_map(pm)
    if (!validation$valid) {
      showNotification(paste("Validation failed:", paste(validation$errors, collapse = " | ")), type = "error")
      return(NULL)
    }

    pm
  })

  output$plate_map_status <- renderUI({
    if (is.null(plate_map_data())) {
      tags$p("No plate map loaded yet",
             style = "color:#FF9F0A; padding-left:15px; font-size:11px;")
    } else {
      n_plates  <- length(unique(plate_map_data()$plate))
      n_samples <- sum(!plate_map_data()$is_vc & !plate_map_data()$is_mock)
      tags$p(glue::glue("Plate map loaded: {n_plates} plate(s), {n_samples} sample entries"),
             style = "color:#34C759; padding-left:15px; font-size:11px;")
    }
  })

  # ── TASK 7 — Run Analysis gating ──────────────────────────
  # Parsing, % neutralisation, curve fitting and QC now wait
  # for an explicit click on `input$run_analysis`. Using
  # eventReactive ensures the whole math pipeline only fires
  # when the user is ready, while the upstream reactives
  # (live_cfg, conc_series, plate_map_data) remain live so
  # the *next* click always sees the latest slider values.
  parsed_data <- eventReactive(input$run_analysis, {
    req(input$master_excel_file, plate_map_data())
    withProgress(message = "Reading VirIDot data...", value = 0.2, {
      parse_master_excel(
        input$master_excel_file$datapath,
        plate_map_data(),
        conc_series(),
        live_cfg()
      )
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  neut_data <- eventReactive(input$run_analysis, {
    req(parsed_data())
    withProgress(message = "Calculating % neutralization...", value = 0.4, {
      combined_plates <- NULL
      if (input$vc_method == "combined" && trimws(input$combined_vc_sero) != "") {
        plates <- as.integer(strsplit(input$combined_vc_plates, ",")[[1]])
        combined_plates <- list()
        combined_plates[[trimws(input$combined_vc_sero)]] <- plates
      }
      calculate_pct_neut(parsed_data(), live_cfg(), input$vc_method == "combined", combined_plates)
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  avg_data <- eventReactive(input$run_analysis, {
    req(neut_data()); average_replicates(neut_data())
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  fit_data <- eventReactive(input$run_analysis, {
    req(avg_data())
    withProgress(message = "Fitting curves...", value = 0.6, {
      fit_all_curves(avg_data(), live_cfg())
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  qc_data <- eventReactive(input$run_analysis, {
    req(fit_data())
    withProgress(message = "Applying QC...", value = 0.8, {
      apply_qc(fit_data(), live_cfg(), input$sample_type)
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  summary_data <- eventReactive(input$run_analysis, {
    req(qc_data())
    build_ic50_matrix(qc_data(), live_cfg()$export$decimal_places)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ── Run-button feedback ──────────────────────────────────
  output$run_status <- renderUI({
    if (is.null(input$run_analysis) || input$run_analysis == 0) {
      tags$p("Click \u2018Run Analysis\u2019 to compute results.",
             style = "color:#0A84FF; padding-left:15px; font-size:11px; font-weight:500;")
    } else {
      tags$p(glue::glue("Last run: {format(Sys.time(), '%H:%M:%S')}"),
             style = "color:#34C759; padding-left:15px; font-size:11px;")
    }
  })

  mod_rawdata_server("rawdata", parsed_data = parsed_data, plate_map = plate_map_data, neut_data = neut_data)
  mod_heatmap_server("heatmap", parsed_data = parsed_data)
  mod_curves_server("curves", avg_data = avg_data, concs = conc_series, conc_units = reactive(input$conc_units), cfg = live_cfg, fit_data = fit_data)
  mod_ic50table_server("ic50table", summary_data = summary_data, cfg = live_cfg)
  mod_charts_server("charts", summary_data = summary_data, conc_units = reactive(input$conc_units), ic50_cap = reactive(input$ic50_cap))
  mod_qcreport_server("qcreport", qc_data = summary_data, neut_data = neut_data, experiment_name = reactive(input$experiment_name), analyst_name = reactive(input$analyst_name))
}
