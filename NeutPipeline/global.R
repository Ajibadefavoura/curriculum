# ============================================================
# NeutPipeline — global.R
# ============================================================

# ── Libraries ─────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)
library(drc)
library(ggplot2)
library(plotly)
library(DT)
library(yaml)
library(scales)
library(rlang)
library(stringr)
library(glue)
library(tibble)

# ── Load configuration ─────────────────────────────────────
cfg <- yaml::read_yaml("config.yml")

# yaml::read_yaml does not parse scientific notation (e.g. 1e-10) as numeric.
# Coerce all config values that must be numeric.
cfg$qc$ic50_floor      <- as.numeric(cfg$qc$ic50_floor)
cfg$qc$ic50_cap        <- as.numeric(cfg$qc$ic50_cap)
cfg$qc$min_hillslope   <- as.numeric(cfg$qc$min_hillslope)
cfg$qc$min_r2_mab      <- as.numeric(cfg$qc$min_r2_mab)
cfg$qc$min_r2_poly     <- as.numeric(cfg$qc$min_r2_poly)
cfg$qc$max_vc_cv_pct   <- as.numeric(cfg$qc$max_vc_cv_pct)
cfg$qc$max_mock_bg_pct <- as.numeric(cfg$qc$max_mock_bg_pct)
cfg$qc$min_max_neut_pct <- as.numeric(cfg$qc$min_max_neut_pct)
cfg$curve_fitting$max_iter <- as.numeric(cfg$curve_fitting$max_iter)
cfg$curve_fitting$ci_level <- as.numeric(cfg$curve_fitting$ci_level)

# ── Source pipeline helper functions ──────────────────────
source("R/parse_viridot.R")
source("R/calc_neut.R")
source("R/fit_curves.R")
source("R/qc_filter.R")
source("R/summarise.R")

# ── Source Shiny UI modules ────────────────────────────────
source("modules/mod_rawdata.R")
source("modules/mod_heatmap.R")
source("modules/mod_curves.R")
source("modules/mod_ic50table.R")
source("modules/mod_charts.R")
source("modules/mod_qcreport.R")

# ── Utility functions ──────────────────────────────────────
build_conc_series <- function(start, fold, steps) {
  purrr::map_dbl(0:(steps - 1), ~ start / (fold ^ .x))
}

compute_dilution_factor <- function(vol_ab, vol_virus) {
  (vol_ab + vol_virus) / vol_ab
}

apply_conc_convention <- function(concs, dilution_factor, convention = "prepared") {
  if (convention == "inwell") concs / dilution_factor
  else concs
}

# Local null-coalescing operator (rlang / shiny re-export it but
# define locally so older R installs work too).
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# ── Export sanitisation helpers ───────────────────────────
# Strip every non-ASCII / non-printable glyph from text columns
# and every non-numeric character from numeric columns so exports
# are clean of mojibake (e.g. "âŒ", "✅", "❌", "⚠", "≥") and
# downstream tools (Prism, Excel) can read numeric columns
# directly without manual cleanup.
sanitize_text_column <- function(x) {
  s <- as.character(x)
  s <- iconv(s, from = "UTF-8", to = "ASCII", sub = "")
  # Keep only printable ASCII (space through tilde); this also
  # removes any leftover control bytes from UTF-8 mojibake.
  s <- gsub("[^ -~]", "", s)
  s <- gsub("\\s+", " ", s)
  trimws(s)
}

sanitize_numeric_column <- function(x) {
  if (is.numeric(x)) return(x)
  s <- as.character(x)
  s <- iconv(s, from = "UTF-8", to = "ASCII", sub = "")
  # Strip Prism-incompatible prefixes such as ">", "<", "~", "≈"
  # before parsing so values like "> 20000" or "~4.08" become
  # pure numbers.
  s <- gsub("[><~\u2248]", "", s)
  s <- gsub("[^[:digit:].eE+-]", "", s)
  suppressWarnings(as.numeric(s))
}

# Numeric columns that must always export as pure numbers.
.np_numeric_cols <- c(
  "ic50", "ic50_display", "inv_ic50", "inv_ic50_display",
  "hill_slope", "r_squared", "ci_lower", "ci_upper",
  "lloq", "uloq",
  "concentration", "ffu_count", "vc_avg", "vc_sd",
  "vc_cv_pct", "mock_avg", "pct_neut", "pct_neut_avg",
  "pct_neut_rep1", "pct_neut_rep2", "plate", "replicate",
  "well_col", "n_reps", "potency", "numeric_ic50"
)

# Columns whose values are deliberately string-censored
# ("< LLOQ", "> 20000", "ND", "Inactive") and must therefore
# NEVER be coerced to numeric, even though the column name looks
# numeric to the auto-detector.
.np_string_only_cols <- c(
  "ic50_censored", "ic50_report", "ic50_classification",
  "ic50_type", "fit_status", "qc_status", "qc_detail",
  "flags", "plate_vc_flag", "plate_mock_flag",
  "ffu", "vc", "mock"
)

sanitize_for_export <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::any_of(.np_numeric_cols),
    ~ sanitize_numeric_column(.x)
  ))

  # Auto-detect "looks numeric" columns by name (e.g. the
  # serotype-prefixed columns in the wide IC50 matrix), but
  # exclude both the explicit numeric list and the string-only
  # censored columns.
  numeric_like_cols <- grep(
    "(?i)ic50|hill|r_squared|r2|ci_|concentration|ffu|pct|neut|potency|plate$|lloq|uloq",
    names(df), value = TRUE
  )
  numeric_like_cols <- setdiff(numeric_like_cols,
                               c(.np_numeric_cols, .np_string_only_cols))
  # Also exclude any column whose values are clearly censored
  # strings (contain "<", ">", "ND", "LLOQ", "ULOQ", "Inactive").
  censored_pattern <- "<|>|ND|LLOQ|ULOQ|Inactive|FAIL|PASS|Ambiguous"
  for (col in numeric_like_cols) {
    vals <- as.character(df[[col]])
    if (any(grepl(censored_pattern, vals, ignore.case = FALSE))) {
      next
    }
    df[[col]] <- sanitize_numeric_column(df[[col]])
  }
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::where(is.character),
    ~ sanitize_text_column(.x)
  ))
  df
}

# ── UI label prettifier (domain-aware) ────────────────────
# Converts internal snake_case identifiers into publication-ready
# headings. Domain-specific tokens (IC50, FFU, VC, CV, R squared,
# 95% CI, Hill slope, %neut, etc.) preserve their accepted
# capitalisation; everything else falls back to Title Case.
.np_label_dictionary <- c(
  "ic50"               = "IC50",
  "ic50_display"       = "IC50",
  "ic50_report"        = "IC50",
  "ic50_censored"      = "IC50 (Reported)",
  "ic50_classification"= "IC50 Classification",
  "ic50_type"          = "IC50 Type",
  "ic50_cap"           = "IC50 Cap",
  "ic50_floor"         = "IC50 Floor",
  "inv_ic50"           = "1 / IC50",
  "inv_ic50_display"   = "1 / IC50",
  "lloq"               = "LLOQ",
  "uloq"               = "ULOQ",
  "hill_slope"         = "Hill Slope",
  "min_hillslope"      = "Min Hill Slope",
  "r_squared"          = "R Squared",
  "min_r2_mab"         = "Min R Squared (mAb)",
  "min_r2_poly"        = "Min R Squared (Polyclonal)",
  "ci_lower"           = "CI Lower",
  "ci_upper"           = "CI Upper",
  "ci_level"           = "CI Level",
  "ffu"                = "FFU",
  "ffu_count"          = "FFU Count",
  "vc"                 = "VC",
  "mock"               = "Mock",
  "vc_avg"             = "VC Mean",
  "vc_sd"              = "VC SD",
  "vc_cv_pct"          = "VC CV (%)",
  "max_vc_cv_pct"      = "Max VC CV (%)",
  "mock_avg"           = "Mock Mean",
  "max_mock_bg_pct"    = "Max Mock Background (%)",
  "min_max_neut_pct"   = "Min Max %Neut",
  "pct_neut"           = "% Neutralization",
  "pct_neut_avg"       = "% Neutralization (Mean)",
  "pct_neut_rep1"      = "% Neutralization (Rep 1)",
  "pct_neut_rep2"      = "% Neutralization (Rep 2)",
  "pct_neut_raw"       = "% Neutralization (Raw)",
  "n_reps"             = "Replicates",
  "well_row"           = "Well Row",
  "well_col"           = "Well Column",
  "sample_id"          = "Sample",
  "fit_status"         = "Fit Status",
  "qc_status"          = "QC Status",
  "qc_detail"          = "QC Detail",
  "plate_vc_flag"      = "Plate VC Flag",
  "plate_mock_flag"    = "Plate Mock Flag",
  "experiment_name"    = "Experiment",
  "analyst_name"       = "Analyst",
  "row_start"          = "Row Start",
  "row_end"            = "Row End",
  "is_vc"              = "Is VC",
  "is_mock"            = "Is Mock",
  "concentration"      = "Concentration",
  "potency"            = "Potency"
)

prettify_label <- function(x) {
  s <- as.character(x)
  out <- character(length(s))
  for (i in seq_along(s)) {
    key <- s[i]
    key_lc <- tolower(key)
    if (!is.na(key) && key_lc %in% names(.np_label_dictionary)) {
      out[i] <- .np_label_dictionary[[key_lc]]
    } else {
      tmp <- gsub("_", " ", key)
      tmp <- tools::toTitleCase(tolower(tmp))
      # Re-uppercase common scientific abbreviations.
      tmp <- gsub("\\bIc50\\b",   "IC50", tmp, ignore.case = FALSE)
      tmp <- gsub("\\bFfu\\b",    "FFU",  tmp, ignore.case = FALSE)
      tmp <- gsub("\\bVc\\b",     "VC",   tmp, ignore.case = FALSE)
      tmp <- gsub("\\bCv\\b",     "CV",   tmp, ignore.case = FALSE)
      tmp <- gsub("\\bSd\\b",     "SD",   tmp, ignore.case = FALSE)
      tmp <- gsub("\\bCi\\b",     "CI",   tmp, ignore.case = FALSE)
      tmp <- gsub("\\bQc\\b",     "QC",   tmp, ignore.case = FALSE)
      tmp <- gsub("\\bMab\\b",    "mAb",  tmp, ignore.case = FALSE)
      # \b doesn't fire between a letter and a digit, so match
      # both 'Denv' alone and 'Denv1', 'Denv2' etc.
      tmp <- gsub("\\bDenv([0-9]*)", "DENV\\1", tmp, ignore.case = FALSE)
      tmp <- gsub("\\bLloq\\b",   "LLOQ", tmp, ignore.case = FALSE)
      tmp <- gsub("\\bUloq\\b",   "ULOQ", tmp, ignore.case = FALSE)
      tmp <- gsub("\\bNd\\b",     "ND",   tmp, ignore.case = FALSE)
      tmp <- gsub("\\bPct\\b",    "%",    tmp, ignore.case = FALSE)
      out[i] <- tmp
    }
  }
  out
}

prettify_colnames <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  colnames(df) <- prettify_label(colnames(df))
  df
}

# ── XLSX export (Calibri, bold headers, ASCII-clean) ──────
# Single helper used by every download handler: writes a tidy
# .xlsx workbook with bold Calibri headers and Prism-friendly
# numeric columns. Accepts either a data.frame or a named list
# of data.frames (one per worksheet).
write_neut_xlsx <- function(data, file, default_sheet = "Sheet1") {
  if (is.data.frame(data)) {
    sheets <- list()
    sheets[[default_sheet]] <- data
  } else {
    sheets <- data
  }

  wb <- openxlsx::createWorkbook(creator = "NeutPipeline")
  openxlsx::modifyBaseFont(wb, fontName = "Calibri", fontSize = 11)

  header_style <- openxlsx::createStyle(
    fontName = "Calibri", fontSize = 11, textDecoration = "bold",
    halign = "left", valign = "center",
    fgFill = "#1D1D1F", fontColour = "#FFFFFF",
    border = "TopBottomLeftRight", borderStyle = "medium",
    borderColour = "#1D1D1F"
  )
  body_style <- openxlsx::createStyle(
    fontName = "Calibri", fontSize = 11, valign = "center",
    border = "TopBottomLeftRight", borderStyle = "thin",
    borderColour = "#BFBFBF"
  )
  alt_row_style <- openxlsx::createStyle(
    fgFill = "#F7F7FA"
  )

  for (sheet_name in names(sheets)) {
    df <- sheets[[sheet_name]]
    df <- sanitize_for_export(df)
    df <- prettify_colnames(df)

    safe_name <- substr(
      stringr::str_replace_all(sheet_name, "[\\\\/?*:\\[\\]]", "_"),
      1, 31
    )
    openxlsx::addWorksheet(wb, safe_name, gridLines = TRUE)
    openxlsx::writeData(wb, safe_name, df, headerStyle = header_style,
                        borders = "all", borderStyle = "thin",
                        borderColour = "#BFBFBF")
    if (nrow(df) > 0) {
      openxlsx::addStyle(
        wb, safe_name, body_style,
        rows = 2:(nrow(df) + 1), cols = seq_len(ncol(df)),
        gridExpand = TRUE, stack = TRUE
      )
      alt_rows <- seq(3, nrow(df) + 1, by = 2)
      if (length(alt_rows) > 0) {
        openxlsx::addStyle(
          wb, safe_name, alt_row_style,
          rows = alt_rows, cols = seq_len(ncol(df)),
          gridExpand = TRUE, stack = TRUE
        )
      }
    }
    openxlsx::setColWidths(wb, safe_name, cols = seq_len(ncol(df)), widths = "auto")
    openxlsx::setRowHeights(wb, safe_name, rows = 1, heights = 22)
    openxlsx::freezePane(wb, safe_name, firstRow = TRUE)
  }

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}

# ── Heatmap palette (laboratory red-to-grey) ─────────────
# Strict gradient for % Neutralization fills used by the
# heatmap module. Higher %neut = deeper red; 0% = light grey.
neutpipeline_heatmap_palette <- c(
  "0"   = "#EBEBEB",
  "25"  = "#FFF0EE",
  "50"  = "#FFBBBB",
  "75"  = "#FF5555",
  "90"  = "#E02020",
  "100" = "#B30000"
)

neutpipeline_heatmap_values  <- as.numeric(names(neutpipeline_heatmap_palette)) / 100
neutpipeline_heatmap_colors  <- unname(neutpipeline_heatmap_palette)
