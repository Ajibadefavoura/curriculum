# ============================================================
# NeutPipeline — scripts/run_pipeline.R
# ============================================================
# Standalone, no-Shiny entry point. Runs the full pipeline
# (parse -> %neut -> 4PL fit -> QC -> charts) and writes
# everything to disk. Use this when you want headless / batch
# / CI / cron / RStudio-only execution without ever booting
# the Shiny UI.
#
# Usage from R:
#   source("scripts/run_pipeline.R")
#   results <- run_pipeline(
#     master_excel = "Experiment_2026.xlsx",
#     plate_map    = "plate_map.csv",            # optional
#     out_dir      = "out/run-2026-05-04"
#   )
#
# Usage from a shell:
#   Rscript scripts/run_pipeline.R \
#       --master-excel Experiment_2026.xlsx \
#       --plate-map    plate_map.csv \
#       --out          out/run-2026-05-04
#
# All charts and XLSX exports written to <out_dir>:
#   - raw-ffu.xlsx
#   - ic50-long.xlsx
#   - ic50-matrix.xlsx
#   - qc-full-detail.xlsx
#   - qc-matrix.xlsx
#   - dose-response-per-sample.png
#   - dose-response-per-plate.png
#   - heatmap-collective.png
#   - heatmap-<serotype>-plate-<n>.png  (one per plate)
#   - layout-1-ic50.png
#   - layout-2-inverse-ic50.png
#   - ic50-summary-potency.png

suppressPackageStartupMessages({
  here_dir <- if (file.exists("global.R")) "." else "NeutPipeline"
  setwd(here_dir)
  source("global.R")
})

# ── Standalone chart builders that don't depend on Shiny inputs ──
build_per_sample_curves <- function(avg_df, fit_df, conc_units = "ng/mL",
                                    n_cols = 5) {
  x_range <- 10^seq(log10(min(avg_df$concentration[avg_df$concentration > 0]) * 0.3),
                    log10(max(avg_df$concentration) * 3),
                    length.out = 300)

  smooth_df <- avg_df %>%
    dplyr::group_by(serotype, plate, sample_id) %>%
    dplyr::group_modify(~ {
      x <- .x$concentration; y <- .x$pct_neut_avg
      keep <- is.finite(x) & x > 0 & is.finite(y)
      x <- x[keep]; y <- y[keep]
      if (length(x) < 3) return(data.frame(x_smooth = x_range, y_smooth = NA_real_))
      fit <- tryCatch(
        suppressMessages(suppressWarnings(
          drc::drm(y ~ x, fct = drc::LL.2(upper = 100),
                   data = data.frame(x = x, y = y),
                   lowerl = c(-20, min(x) / 10),
                   upperl = c(20,  max(x) * 10),
                   control = drc::drmc(noMessage = TRUE)))),
        error = function(e) NULL
      )
      if (is.null(fit))
        return(data.frame(x_smooth = x_range, y_smooth = NA_real_))
      data.frame(x_smooth = x_range,
                 y_smooth = predict(fit, newdata = data.frame(x = x_range)))
    }) %>%
    dplyr::ungroup()

  ic50_marks <- fit_df %>%
    dplyr::filter(is.finite(ic50), ic50 > 0,
                  ic50 < max(avg_df$concentration) * 100) %>%
    dplyr::distinct(serotype, sample_id, ic50)

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(smooth_df, !is.na(y_smooth)),
      ggplot2::aes(x_smooth, y_smooth, color = serotype),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = avg_df,
      ggplot2::aes(concentration, pct_neut_avg, color = serotype),
      size = 2.4
    ) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
                        color = "black", linewidth = 0.4) +
    ggplot2::geom_segment(
      data = ic50_marks,
      ggplot2::aes(x = ic50, xend = ic50, y = 0, yend = 50, color = serotype),
      linetype = "dashed", linewidth = 0.4, show.legend = FALSE
    ) +
    ggplot2::scale_x_log10(
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    ggplot2::scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                                limits = c(0, 110)) +
    ggplot2::labs(x = "ng/mL, purified mAb", y = "% Neutralized",
                  color = "Serotype") +
    ggplot2::facet_wrap(~ sample_id, ncol = n_cols) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      axis.line  = ggplot2::element_line(color = "black", linewidth = 0.7),
      axis.text  = ggplot2::element_text(face = "bold", color = "black"),
      axis.title = ggplot2::element_text(face = "bold", color = "black"),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

build_per_plate_curves <- function(avg_df, conc_units = "ng/mL") {
  build <- build_per_sample_curves(avg_df, dplyr::tibble(
    serotype = character(), sample_id = character(), ic50 = numeric()
  ))  # reuse but we'll override the facet
  # Build a fresh per-plate version with sample colouring:
  x_range <- 10^seq(log10(min(avg_df$concentration[avg_df$concentration > 0]) * 0.3),
                    log10(max(avg_df$concentration) * 3),
                    length.out = 300)

  smooth_df <- avg_df %>%
    dplyr::group_by(serotype, plate, sample_id) %>%
    dplyr::group_modify(~ {
      x <- .x$concentration; y <- .x$pct_neut_avg
      keep <- is.finite(x) & x > 0 & is.finite(y)
      x <- x[keep]; y <- y[keep]
      if (length(x) < 3) return(data.frame(x_smooth = x_range, y_smooth = NA_real_))
      fit <- tryCatch(
        suppressMessages(suppressWarnings(
          drc::drm(y ~ x, fct = drc::LL.2(upper = 100),
                   data = data.frame(x = x, y = y),
                   control = drc::drmc(noMessage = TRUE)))),
        error = function(e) NULL
      )
      if (is.null(fit))
        return(data.frame(x_smooth = x_range, y_smooth = NA_real_))
      data.frame(x_smooth = x_range,
                 y_smooth = predict(fit, newdata = data.frame(x = x_range)))
    }) %>%
    dplyr::ungroup()

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(smooth_df, !is.na(y_smooth)),
      ggplot2::aes(x_smooth, y_smooth, color = sample_id,
                   group = interaction(sample_id, serotype)),
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      data = avg_df,
      ggplot2::aes(concentration, pct_neut_avg, color = sample_id),
      size = 1.6, alpha = 0.9
    ) +
    ggplot2::geom_hline(yintercept = 50, linetype = "dashed",
                        color = "black", linewidth = 0.4) +
    ggplot2::scale_x_log10(
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    ggplot2::scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100),
                                limits = c(0, 110)) +
    ggplot2::labs(x = "ng/mL, purified mAb", y = "% Neutralized",
                  color = "Sample") +
    ggplot2::facet_wrap(~ paste0("Plate ", plate)) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      axis.line  = ggplot2::element_line(color = "black", linewidth = 0.7),
      axis.text  = ggplot2::element_text(face = "bold", color = "black"),
      axis.title = ggplot2::element_text(face = "bold", color = "black"),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

build_collective_heatmap <- function(neut_df) {
  agg <- neut_df %>%
    dplyr::filter(!is.na(sample_id),
                  !grepl("^vc$",   sample_id, ignore.case = TRUE),
                  !grepl("^mock$", sample_id, ignore.case = TRUE)) %>%
    dplyr::group_by(serotype, sample_id) %>%
    dplyr::summarise(max_pct = max(pct_neut, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(fill = pmax(0, pmin(100, max_pct)) / 100)

  ggplot2::ggplot(agg, ggplot2::aes(sample_id, serotype, fill = fill)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f", max_pct)),
                       size = 3.2, fontface = "bold") +
    ggplot2::scale_fill_gradientn(
      colours = neutpipeline_heatmap_colors,
      values  = neutpipeline_heatmap_values,
      limits  = c(0, 1),
      labels  = c("0%", "25%", "50%", "75%", "90%", "100%"),
      breaks  = c(0, 0.25, 0.5, 0.75, 0.9, 1),
      name    = "% Neut"
    ) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = "Collective % Neutralization") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                          face = "bold", color = "black"),
      axis.text.y = ggplot2::element_text(face = "bold", color = "black"),
      panel.grid  = ggplot2::element_blank()
    )
}

build_potency_summary <- function(qc_df, ic50_cap = 20000) {
  d <- qc_df %>%
    dplyr::filter(!is.na(sample_id),
                  !grepl("^vc$",   sample_id, ignore.case = TRUE),
                  !grepl("^mock$", sample_id, ignore.case = TRUE)) %>%
    dplyr::mutate(
      numeric_ic50 = dplyr::if_else(
        !is.finite(ic50) |
          grepl("Inactive|FAIL", as.character(qc_status)),
        as.numeric(ic50_cap), ic50
      ),
      numeric_ic50 = dplyr::if_else(numeric_ic50 <= 0,
                                    as.numeric(ic50_cap), numeric_ic50),
      potency = 1 / numeric_ic50
    )

  pos <- d$potency[is.finite(d$potency) & d$potency > 0]
  y_min <- if (length(pos) > 0) min(c(pos, 1 / ic50_cap)) * 0.5 else 1e-6
  y_max <- if (length(pos) > 0) max(pos) * 1.5 else 1
  ref <- data.frame(ic50 = c(100, 1000, 10000),
                    label = c("IC50 = 100", "IC50 = 1,000", "IC50 = 10,000"))
  ref$y <- 1 / ref$ic50

  ggplot2::ggplot(d, ggplot2::aes(reorder(sample_id, -potency),
                                  potency, fill = serotype)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8,
                                                         preserve = "single"),
                      width = 0.7, color = "black", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = ref$y, linetype = "dashed",
                        color = "grey30", linewidth = 0.4) +
    ggplot2::annotate("text", x = 0.7, y = ref$y, label = ref$label,
                      hjust = 0, vjust = -0.4, size = 3.4,
                      color = "grey25", fontface = "bold") +
    ggplot2::scale_y_log10(
      labels = scales::label_number(),
      breaks = 10^seq(floor(log10(y_min)), ceiling(log10(y_max))),
      limits = c(y_min, y_max),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(x = NULL, y = "1 / IC50  (Potency)",
                  fill = "Serotype",
                  title = "IC50 Summary  (Y = 1 / IC50)") +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                          face = "bold", color = "black"),
      axis.text.y = ggplot2::element_text(face = "bold", color = "black"),
      axis.line   = ggplot2::element_line(color = "black", linewidth = 0.6),
      panel.grid  = ggplot2::element_blank()
    )
}

# ── Main entry point ──────────────────────────────────────
run_pipeline <- function(master_excel,
                         plate_map      = NULL,
                         out_dir        = "out",
                         start_conc     = NULL,
                         dilution_fold  = NULL,
                         n_steps        = NULL,
                         vol_antibody   = NULL,
                         vol_virus      = NULL,
                         conc_convention = "prepared",
                         sample_type    = "mab") {

  if (!file.exists(master_excel))
    stop("Master Excel not found: ", master_excel)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  message("==> Loading config")
  c <- cfg
  if (!is.null(start_conc))    c$concentration$starting_conc <- start_conc
  if (!is.null(dilution_fold)) c$concentration$dilution_fold <- dilution_fold
  if (!is.null(n_steps))       c$concentration$n_steps       <- n_steps
  if (!is.null(vol_antibody))  c$mixing$vol_antibody_ul      <- vol_antibody
  if (!is.null(vol_virus))     c$mixing$vol_virus_ul         <- vol_virus
  c$mixing$convention <- conc_convention

  concs_prepared <- build_conc_series(
    c$concentration$starting_conc,
    c$concentration$dilution_fold,
    c$concentration$n_steps
  )
  df_factor <- compute_dilution_factor(c$mixing$vol_antibody_ul,
                                       c$mixing$vol_virus_ul)
  concs <- apply_conc_convention(concs_prepared, df_factor,
                                 c$mixing$convention)

  message("==> Reading plate map")
  if (!is.null(plate_map) && file.exists(plate_map)) {
    pm <- read.csv(plate_map, stringsAsFactors = FALSE)
  } else {
    sheets <- readxl::excel_sheets(master_excel)
    pm_sheet <- sheets[tolower(sheets) == "plate_map"]
    if (length(pm_sheet) == 0)
      stop("No plate map provided and no 'plate_map' sheet in the Excel file.")
    pm <- as.data.frame(readxl::read_excel(master_excel, sheet = pm_sheet[1]))
  }
  pm$plate     <- as.integer(pm$plate)
  pm$row_start <- toupper(trimws(pm$row_start))
  pm$row_end   <- toupper(trimws(pm$row_end))
  pm$half      <- tolower(trimws(pm$half))
  pm$is_vc     <- as.logical(toupper(as.character(pm$is_vc)))
  pm$is_mock   <- as.logical(toupper(as.character(pm$is_mock)))
  pm$is_vc[is.na(pm$is_vc)]     <- FALSE
  pm$is_mock[is.na(pm$is_mock)] <- FALSE

  v <- validate_plate_map(pm)
  if (!v$valid) stop("Plate map validation failed: ",
                     paste(v$errors, collapse = "; "))

  message("==> Parsing FFU data")
  parsed <- parse_master_excel(master_excel, pm, concs, c)

  message("==> Computing % neutralization")
  neut <- calculate_pct_neut(parsed, c)
  avg  <- average_replicates(neut)

  message("==> Fitting 4PL curves")
  fit <- fit_all_curves(avg, c)

  message("==> Applying QC")
  qc  <- apply_qc(fit, c, sample_type)
  qc  <- avg %>%
    dplyr::distinct(serotype, plate, sample_id) %>%
    dplyr::left_join(qc, by = c("serotype", "sample_id"),
                     suffix = c("", ".fit")) %>%
    dplyr::mutate(plate = dplyr::coalesce(plate, plate.fit)) %>%
    dplyr::select(-dplyr::any_of("plate.fit"))
  summary_df <- build_ic50_matrix(qc, c$export$decimal_places)

  message("==> Writing XLSX exports")
  write_neut_xlsx(parsed,    file.path(out_dir, "raw-ffu.xlsx"),
                  default_sheet = "Raw FFU")
  write_neut_xlsx(summary_df, file.path(out_dir, "ic50-long.xlsx"),
                  default_sheet = "IC50 Long")
  write_neut_xlsx(pivot_ic50_wide(summary_df),
                  file.path(out_dir, "ic50-matrix.xlsx"),
                  default_sheet = "IC50 Matrix")
  write_neut_xlsx(summary_df, file.path(out_dir, "qc-full-detail.xlsx"),
                  default_sheet = "QC Detail")

  message("==> Writing PNG charts")
  ggplot2::ggsave(
    file.path(out_dir, "dose-response-per-sample.png"),
    plot = build_per_sample_curves(avg, fit),
    width = 16, height = 12, dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    file.path(out_dir, "dose-response-per-plate.png"),
    plot = build_per_plate_curves(avg),
    width = 14, height = 10, dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    file.path(out_dir, "heatmap-collective.png"),
    plot = build_collective_heatmap(neut),
    width = 14, height = 6, dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    file.path(out_dir, "ic50-summary-potency.png"),
    plot = build_potency_summary(summary_df, c$qc$ic50_cap),
    width = 14, height = 7, dpi = 300, bg = "white"
  )

  message("==> Done. Files in: ", normalizePath(out_dir))
  invisible(list(parsed = parsed, neut = neut, avg = avg,
                 fit = fit, qc = qc, summary = summary_df,
                 out_dir = out_dir))
}

# ── CLI parser ────────────────────────────────────────────
.np_parse_args <- function(argv) {
  args <- list()
  i <- 1
  while (i <= length(argv)) {
    a <- argv[i]
    if (a %in% c("--master-excel", "--plate-map", "--out",
                 "--start-conc", "--dilution-fold", "--n-steps",
                 "--vol-antibody", "--vol-virus",
                 "--conc-convention", "--sample-type")) {
      key <- gsub("^--", "", a); key <- gsub("-", "_", key)
      args[[key]] <- argv[i + 1]; i <- i + 2
    } else { i <- i + 1 }
  }
  args
}

if (!interactive() && length(commandArgs(trailingOnly = TRUE)) > 0) {
  args <- .np_parse_args(commandArgs(trailingOnly = TRUE))
  if (is.null(args$master_excel))
    stop("Required: --master-excel <path/to/file.xlsx>")
  to_num <- function(x) if (is.null(x)) NULL else as.numeric(x)
  run_pipeline(
    master_excel    = args$master_excel,
    plate_map       = args$plate_map,
    out_dir         = args$out %||% "out",
    start_conc      = to_num(args$start_conc),
    dilution_fold   = to_num(args$dilution_fold),
    n_steps         = to_num(args$n_steps),
    vol_antibody    = to_num(args$vol_antibody),
    vol_virus       = to_num(args$vol_virus),
    conc_convention = args$conc_convention %||% "prepared",
    sample_type     = args$sample_type %||% "mab"
  )
}
