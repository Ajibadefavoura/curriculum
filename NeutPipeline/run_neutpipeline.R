# ============================================================
# NeutPipeline — run_neutpipeline.R
# ============================================================
# Standalone R-only workflow (no Shiny required).
# Designed to be run from RStudio with a single source() call.
#
# Quick start (RStudio):
#   1. setwd() to the NeutPipeline folder
#   2. source("run_neutpipeline.R")
#   3. results <- np_run(
#        master_excel = "Experiment_2026.xlsx",
#        plate_map    = NULL,                 # NULL = read 'plate_map' sheet from Excel
#        out_dir      = "out/2026-05-04",
#        sample_type  = "mab"                 # or "polyclonal"
#      )
#   4. Open out/<date>/ in Windows Explorer to see every PNG
#      and XLSX written.
#
# What you get for any number of serotypes (DENV1, DENV2, DENV3,
# DENV4, etc.) and any number of plates per serotype:
#
#   raw-ffu.xlsx
#   ic50-long.xlsx
#   ic50-matrix.xlsx
#   qc-full-detail.xlsx
#   01-dose-response-per-sample.png        (publication grid)
#   02-dose-response-per-plate.png         (one panel per plate)
#   03-dose-response-per-serotype.png      (one panel per serotype)
#   04-heatmap-collective.png              (DENV1-4 x samples)
#   05-heatmap-<SEROTYPE>-plate-<n>.png    (one per plate)
#   06-layout-1-ic50.png                   (IC50 ng/mL bars, log-Y)
#   07-layout-2-potency.png                (1/IC50 bars, log-Y)
#
# Charts use theme_classic + bold black axes, no grid lines, log
# X axis labelled "ng/mL, purified mAb", Y "% Neutralized"
# (0,20,40,60,80,100 breaks), dashed 50% reference + dashed
# vertical IC50 drop lines.
# ============================================================

suppressPackageStartupMessages({
  if (file.exists("global.R")) {
    source("global.R", local = FALSE)
  } else if (file.exists("NeutPipeline/global.R")) {
    setwd("NeutPipeline")
    source("global.R", local = FALSE)
  } else {
    stop("Cannot find global.R - please setwd() into the NeutPipeline folder first.")
  }
})

# ── Shared publication theme ─────────────────────────────
np_theme <- function(base = 13) {
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
      plot.title  = ggplot2::element_text(face = "bold",
                                          size = base + 2,
                                          color = "black",
                                          hjust = 0),
      legend.position = "right",
      legend.title    = ggplot2::element_text(face = "bold")
    )
}

# ── Smooth-curve generator (shared LL.2 -> LL.4 retry chain) ──
np_smooth_curves <- function(avg_df, x_range) {
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

  avg_df %>%
    dplyr::group_by(serotype, plate, sample_id) %>%
    dplyr::group_modify(~ {
      x <- .x$concentration; y <- .x$pct_neut_avg
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
        fit <- try_fit(spec); if (!is.null(fit)) break
      }
      if (is.null(fit)) return(data.frame(x_smooth = x_range, y_smooth = NA_real_))
      data.frame(x_smooth = x_range,
                 y_smooth = predict(fit, newdata = data.frame(x = x_range)))
    }) %>%
    dplyr::ungroup()
}

# ── 1. Per-sample reference grid (one panel per sample) ──
np_plot_per_sample <- function(avg_df, fit_df, n_cols = 5) {
  x_range <- 10^seq(log10(min(avg_df$concentration[avg_df$concentration > 0]) * 0.3),
                    log10(max(avg_df$concentration) * 3),
                    length.out = 300)
  smooth_df <- np_smooth_curves(avg_df, x_range)
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
    np_theme(13)
}

# ── 2. One panel per plate (samples coloured) ────────────
np_plot_per_plate <- function(avg_df) {
  x_range <- 10^seq(log10(min(avg_df$concentration[avg_df$concentration > 0]) * 0.3),
                    log10(max(avg_df$concentration) * 3),
                    length.out = 300)
  smooth_df <- np_smooth_curves(avg_df, x_range)

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
                  color = "Sample",
                  title = "Dose Response by Plate") +
    ggplot2::facet_wrap(~ paste0("Plate ", plate)) +
    np_theme(12)
}

# ── 3. One panel per serotype (samples coloured) ────────
np_plot_per_serotype <- function(avg_df) {
  x_range <- 10^seq(log10(min(avg_df$concentration[avg_df$concentration > 0]) * 0.3),
                    log10(max(avg_df$concentration) * 3),
                    length.out = 300)
  smooth_df <- np_smooth_curves(avg_df, x_range)

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dplyr::filter(smooth_df, !is.na(y_smooth)),
      ggplot2::aes(x_smooth, y_smooth, color = sample_id,
                   group = interaction(sample_id, plate)),
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
                  color = "Sample",
                  title = "Dose Response by Serotype") +
    ggplot2::facet_wrap(~ serotype) +
    np_theme(12)
}

# ── 4. Per-plate spatial heatmap (% neutralization) ───────
np_plot_plate_heatmap <- function(neut_df, parsed_df, sero, plate) {
  pd <- parsed_df %>%
    dplyr::filter(serotype == sero, plate == !!plate)
  nd <- neut_df %>%
    dplyr::filter(serotype == sero, plate == !!plate) %>%
    dplyr::select(serotype, plate, well_row, well_col, pct_neut)
  d  <- pd %>% dplyr::left_join(nd, by = c("serotype","plate","well_row","well_col"))

  d <- d %>% dplyr::mutate(fill = pmax(0, pmin(100, pct_neut)) / 100,
                           label = sprintf("%.0f%%", pct_neut))

  ggplot2::ggplot(d, ggplot2::aes(
      x = factor(well_col, levels = 1:12),
      y = factor(well_row, levels = rev(LETTERS[1:8])),
      fill = fill
    )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       size = 3, color = "black", fontface = "bold") +
    ggplot2::scale_fill_gradientn(
      colours = neutpipeline_heatmap_colors,
      values  = neutpipeline_heatmap_values,
      limits  = c(0, 1),
      breaks  = c(0, 0.25, 0.5, 0.75, 0.9, 1),
      labels  = c("0%","25%","50%","75%","90%","100%"),
      name    = "% Neut",
      na.value = "#FFFFFF"
    ) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(
      x = "Column", y = "Row",
      title = sprintf("%s - Plate %s - Spatial Heatmap", sero, plate)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text  = ggplot2::element_text(face = "bold", color = "black"),
      axis.title = ggplot2::element_text(face = "bold", color = "black"),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

# ── 5. Collective heatmap (all serotypes x samples) ──────
np_plot_collective_heatmap <- function(neut_df) {
  agg <- neut_df %>%
    dplyr::filter(!is.na(sample_id),
                  !grepl("^vc$",   sample_id, ignore.case = TRUE),
                  !grepl("^mock$", sample_id, ignore.case = TRUE)) %>%
    dplyr::group_by(serotype, sample_id) %>%
    dplyr::summarise(max_pct = max(pct_neut, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(fill = pmax(0, pmin(100, max_pct)) / 100,
                  label = sprintf("%.0f", max_pct))

  ord <- unique(agg$sample_id)
  ctrl <- grepl("^ede|c8|c10", ord, ignore.case = TRUE)
  ord  <- c(ord[ctrl], sort(ord[!ctrl]))

  ggplot2::ggplot(agg, ggplot2::aes(
      x = factor(sample_id, levels = ord),
      y = serotype, fill = fill
    )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       size = 3.2, fontface = "bold") +
    ggplot2::scale_fill_gradientn(
      colours = neutpipeline_heatmap_colors,
      values  = neutpipeline_heatmap_values,
      limits  = c(0, 1),
      breaks  = c(0, 0.25, 0.5, 0.75, 0.9, 1),
      labels  = c("0%","25%","50%","75%","90%","100%"),
      name    = "% Neut",
      na.value = "#EBEBEB"
    ) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = "Collective % Neutralization (max per sample, per serotype)") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid  = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                          face = "bold", color = "black"),
      axis.text.y = ggplot2::element_text(face = "bold", color = "black"),
      plot.title  = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
}

# ── 6. Layout 1 — IC50 (ng/mL) bars on log scale ─────────
np_plot_layout1 <- function(qc_df, ic50_cap = 20000, conc_units = "ng/mL") {
  d <- qc_df %>%
    dplyr::filter(!is.na(sample_id),
                  !grepl("^vc$",   sample_id, ignore.case = TRUE),
                  !grepl("^mock$", sample_id, ignore.case = TRUE)) %>%
    dplyr::mutate(
      ic50_plot = pmax(.Machine$double.eps,
                       pmin(as.numeric(ic50), as.numeric(ic50_cap)))
    )
  y_min <- max(.Machine$double.eps,
               min(d$ic50_plot[d$ic50_plot > 0], na.rm = TRUE) / 3)
  y_max <- ic50_cap * 1.4

  ggplot2::ggplot(d, ggplot2::aes(
      x    = reorder(sample_id, ic50_plot),
      y    = ic50_plot,
      fill = serotype
    )) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.8, preserve = "single"),
      width = 0.72, alpha = 0.92, color = "black", linewidth = 0.35
    ) +
    ggplot2::geom_hline(yintercept = ic50_cap, linetype = "dashed",
                        color = "#DC2626", linewidth = 0.5) +
    ggplot2::annotate("text", x = 0.6, y = ic50_cap * 1.05,
                      label = sprintf("Cap: %s %s",
                                      formatC(ic50_cap, format = "d",
                                              big.mark = ","),
                                      conc_units),
                      color = "#DC2626", hjust = 0,
                      size = 3.4, fontface = "bold") +
    ggplot2::scale_y_log10(
      labels = scales::label_number(big.mark = ",",
                                    scale_cut = scales::cut_short_scale()),
      limits = c(y_min, y_max),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      x = NULL, y = sprintf("IC50 (%s) - log scale", conc_units),
      fill = "Serotype",
      title = "Layout 1 - IC50 (ng/mL)"
    ) +
    np_theme(13) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                          face = "bold", color = "black"),
      legend.position = "bottom"
    )
}

# ── 7. Layout 2 — 1/IC50 potency summary ─────────────────
np_plot_layout2 <- function(qc_df, ic50_cap = 20000) {
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

  ord <- d %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(is_ctrl = any(grepl("^ede|c8|c10", sample_id, ignore.case = TRUE)),
                     mp = max(potency, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(is_ctrl), sample_id) %>%
    dplyr::pull(sample_id)

  pos   <- d$potency[is.finite(d$potency) & d$potency > 0]
  y_min <- if (length(pos) > 0) min(c(pos, 1 / ic50_cap)) * 0.5 else 1e-6
  y_max <- if (length(pos) > 0) max(pos) * 1.5 else 1
  ref   <- data.frame(ic50 = c(100, 1000, 10000),
                      label = c("IC50 = 100","IC50 = 1,000","IC50 = 10,000"))
  ref$y <- 1 / ref$ic50

  ggplot2::ggplot(d, ggplot2::aes(
      x    = factor(sample_id, levels = ord),
      y    = potency,
      fill = serotype
    )) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.8, preserve = "single"),
      width = 0.72, color = "black", linewidth = 0.4
    ) +
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
    ggplot2::labs(
      x = NULL, y = "1 / IC50  (Potency)", fill = "Serotype",
      title = "Layout 2 - IC50 Summary (Y = 1 / IC50)"
    ) +
    np_theme(14) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                          face = "bold", color = "black"),
      plot.title  = ggplot2::element_text(face = "bold", hjust = 0.5)
    )
}

# ── Main entry point ─────────────────────────────────────
np_run <- function(master_excel,
                   plate_map       = NULL,
                   out_dir         = "out",
                   start_conc      = NULL,
                   dilution_fold   = NULL,
                   n_steps         = NULL,
                   vol_antibody    = NULL,
                   vol_virus       = NULL,
                   conc_convention = "prepared",
                   sample_type     = "mab",
                   verbose         = TRUE) {

  log <- function(...) if (isTRUE(verbose)) message("==> ", ...)

  if (!file.exists(master_excel))
    stop("Master Excel not found: ", master_excel)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # ── 1. Configure ──
  log("Loading configuration")
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

  # ── 2. Plate map ──
  # Accept either:
  #   - a separate CSV file (plate_map = "plate_map.csv")
  #   - a separate XLSX file (plate_map = "plate_map.xlsx")
  #   - or NULL, in which case we look for a 'plate_map'
  #     sheet inside the master FFU Excel.
  log("Reading plate map")
  if (!is.null(plate_map) && file.exists(plate_map)) {
    ext <- tolower(tools::file_ext(plate_map))
    if (ext %in% c("xlsx", "xls")) {
      pm_sheets <- readxl::excel_sheets(plate_map)
      pm_sheet  <- pm_sheets[tolower(pm_sheets) == "plate_map"]
      if (length(pm_sheet) == 0) pm_sheet <- pm_sheets[1]
      pm <- as.data.frame(readxl::read_excel(plate_map, sheet = pm_sheet[1]))
      log("  - read plate map sheet '", pm_sheet[1],
          "' from ", basename(plate_map))
    } else if (ext %in% c("csv", "tsv", "txt")) {
      sep <- if (ext == "tsv") "\t" else ","
      pm <- read.csv(plate_map, sep = sep, stringsAsFactors = FALSE)
      log("  - read plate map from ", basename(plate_map))
    } else {
      stop("Unsupported plate_map extension: ", ext,
           " (expected .csv, .tsv, .xlsx, or .xls)")
    }
  } else {
    sheets <- readxl::excel_sheets(master_excel)
    pm_sheet <- sheets[tolower(sheets) == "plate_map"]
    if (length(pm_sheet) == 0) {
      stop("No plate map provided and no 'plate_map' sheet inside ",
           basename(master_excel),
           ". Either pass plate_map = '<path>.csv' or '<path>.xlsx', ",
           "or add a 'plate_map' sheet to the master Excel.")
    }
    pm <- as.data.frame(readxl::read_excel(master_excel, sheet = pm_sheet[1]))
    log("  - read embedded 'plate_map' sheet from ", basename(master_excel))
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
  if (!v$valid) stop("Plate map invalid: ", paste(v$errors, collapse = "; "))

  # ── 3. Parse + neutralize + fit + QC ──
  log("Parsing FFU sheets")
  parsed <- parse_master_excel(master_excel, pm, concs, c)

  log("Computing % neutralization")
  neut <- calculate_pct_neut(parsed, c)
  avg  <- average_replicates(neut)

  log("Fitting 4PL curves (LL.2 -> LL.4 retry chain)")
  fit <- fit_all_curves(avg, c)

  log("Applying QC criteria")
  qc  <- apply_qc(fit, c, sample_type)
  qc_long <- avg %>%
    dplyr::distinct(serotype, plate, sample_id) %>%
    dplyr::left_join(qc, by = c("serotype", "sample_id"),
                     suffix = c("", ".fit")) %>%
    dplyr::mutate(plate = dplyr::coalesce(plate, plate.fit)) %>%
    dplyr::select(-dplyr::any_of("plate.fit"))
  summary_df <- build_ic50_matrix(qc_long, c$export$decimal_places)

  # ── 4. XLSX exports ──
  log("Writing XLSX exports")
  write_neut_xlsx(parsed,    file.path(out_dir, "raw-ffu.xlsx"),
                  default_sheet = "Raw FFU")
  write_neut_xlsx(summary_df, file.path(out_dir, "ic50-long.xlsx"),
                  default_sheet = "IC50 Long")
  write_neut_xlsx(pivot_ic50_wide(summary_df),
                  file.path(out_dir, "ic50-matrix.xlsx"),
                  default_sheet = "IC50 Matrix")
  write_neut_xlsx(summary_df, file.path(out_dir, "qc-full-detail.xlsx"),
                  default_sheet = "QC Detail")

  # ── 5. Charts ──
  log("Building dose-response per-sample grid")
  n_samples <- length(unique(avg$sample_id))
  ncol_ps <- 5
  nrow_ps <- max(1, ceiling(n_samples / ncol_ps))
  ggplot2::ggsave(
    file.path(out_dir, "01-dose-response-per-sample.png"),
    plot = np_plot_per_sample(avg, fit, n_cols = ncol_ps),
    width  = 2.6 * ncol_ps + 2,
    height = 2.4 * nrow_ps + 1,
    dpi = 300, bg = "white", limitsize = FALSE
  )

  log("Building dose-response per-plate panels")
  n_plates <- length(unique(avg$plate))
  ggplot2::ggsave(
    file.path(out_dir, "02-dose-response-per-plate.png"),
    plot = np_plot_per_plate(avg),
    width  = 4.5 * min(3, n_plates) + 2,
    height = 4 * ceiling(n_plates / 3) + 1,
    dpi = 300, bg = "white", limitsize = FALSE
  )

  log("Building dose-response per-serotype panels")
  n_sero <- length(unique(avg$serotype))
  ggplot2::ggsave(
    file.path(out_dir, "03-dose-response-per-serotype.png"),
    plot = np_plot_per_serotype(avg),
    width  = 4.5 * min(3, n_sero) + 2,
    height = 4 * ceiling(n_sero / 3) + 1,
    dpi = 300, bg = "white", limitsize = FALSE
  )

  log("Building collective heatmap")
  ggplot2::ggsave(
    file.path(out_dir, "04-heatmap-collective.png"),
    plot = np_plot_collective_heatmap(neut),
    width = 14, height = 6, dpi = 300, bg = "white"
  )

  log("Building per-plate spatial heatmaps")
  combos <- unique(parsed[, c("serotype", "plate")])
  for (i in seq_len(nrow(combos))) {
    sero <- combos$serotype[i]; pl <- combos$plate[i]
    fname <- sprintf("05-heatmap-%s-plate-%d.png",
                     gsub("[^A-Za-z0-9]+", "-", sero), pl)
    ggplot2::ggsave(
      file.path(out_dir, fname),
      plot = np_plot_plate_heatmap(neut, parsed, sero, pl),
      width = 10, height = 6, dpi = 300, bg = "white"
    )
  }

  log("Building Layout 1 (IC50 ng/mL)")
  ggplot2::ggsave(
    file.path(out_dir, "06-layout-1-ic50.png"),
    plot = np_plot_layout1(summary_df, c$qc$ic50_cap,
                            c$concentration$units),
    width = 14, height = 7, dpi = 300, bg = "white"
  )

  log("Building Layout 2 (1/IC50 potency)")
  ggplot2::ggsave(
    file.path(out_dir, "07-layout-2-potency.png"),
    plot = np_plot_layout2(summary_df, c$qc$ic50_cap),
    width = 14, height = 7, dpi = 300, bg = "white"
  )

  log("Done. ", normalizePath(out_dir))

  invisible(list(
    parsed     = parsed,
    neut       = neut,
    avg        = avg,
    fit        = fit,
    qc         = qc_long,
    summary    = summary_df,
    out_dir    = normalizePath(out_dir, mustWork = FALSE)
  ))
}

# ── CLI dispatch ─────────────────────────────────────────
if (!interactive() && length(commandArgs(trailingOnly = TRUE)) > 0) {
  argv <- commandArgs(trailingOnly = TRUE)
  parse_args <- function(argv) {
    args <- list(); i <- 1
    while (i <= length(argv)) {
      a <- argv[i]
      if (a %in% c("--master-excel","--plate-map","--out",
                   "--start-conc","--dilution-fold","--n-steps",
                   "--vol-antibody","--vol-virus",
                   "--conc-convention","--sample-type")) {
        key <- gsub("-", "_", gsub("^--", "", a))
        args[[key]] <- argv[i + 1]; i <- i + 2
      } else { i <- i + 1 }
    }
    args
  }
  args <- parse_args(argv)
  if (is.null(args$master_excel))
    stop("Required: --master-excel <path/to/file.xlsx>")
  to_num <- function(x) if (is.null(x)) NULL else suppressWarnings(as.numeric(x))
  np_run(
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
