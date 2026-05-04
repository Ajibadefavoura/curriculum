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
  "concentration", "ffu_count", "vc_avg", "vc_sd",
  "vc_cv_pct", "mock_avg", "pct_neut", "pct_neut_avg",
  "pct_neut_rep1", "pct_neut_rep2", "plate", "replicate",
  "well_col", "n_reps", "potency", "numeric_ic50"
)

sanitize_for_export <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::any_of(.np_numeric_cols),
    ~ sanitize_numeric_column(.x)
  ))
  # Also clean any column whose name *looks* numeric (e.g. the
  # serotype-prefixed columns in the wide IC50 matrix).
  numeric_like_cols <- grep(
    "(?i)ic50|hill|r_squared|r2|ci_|concentration|ffu|pct|neut|potency|plate$",
    names(df), value = TRUE
  )
  numeric_like_cols <- setdiff(numeric_like_cols, .np_numeric_cols)
  if (length(numeric_like_cols) > 0) {
    df <- dplyr::mutate(df, dplyr::across(
      dplyr::all_of(numeric_like_cols),
      ~ sanitize_numeric_column(.x)
    ))
  }
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::where(is.character),
    ~ sanitize_text_column(.x)
  ))
  df
}

# ── UI label prettifier ───────────────────────────────────
prettify_label <- function(x) {
  s <- as.character(x)
  s <- gsub("_", " ", s)
  tools::toTitleCase(tolower(s))
}

prettify_colnames <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  colnames(df) <- vapply(colnames(df), prettify_label, character(1))
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
    fgFill = "#F2F2F2", border = "bottom", borderStyle = "thin"
  )
  body_style <- openxlsx::createStyle(
    fontName = "Calibri", fontSize = 11, valign = "center"
  )

  for (sheet_name in names(sheets)) {
    df <- sheets[[sheet_name]]
    df <- sanitize_for_export(df)
    df <- prettify_colnames(df)

    safe_name <- substr(
      stringr::str_replace_all(sheet_name, "[\\\\/?*:\\[\\]]", "_"),
      1, 31
    )
    openxlsx::addWorksheet(wb, safe_name, gridLines = FALSE)
    openxlsx::writeData(wb, safe_name, df, headerStyle = header_style)
    if (nrow(df) > 0) {
      openxlsx::addStyle(
        wb, safe_name, body_style,
        rows = 2:(nrow(df) + 1), cols = seq_len(ncol(df)),
        gridExpand = TRUE, stack = TRUE
      )
    }
    openxlsx::setColWidths(wb, safe_name, cols = seq_len(ncol(df)), widths = "auto")
    openxlsx::freezePane(wb, safe_name, firstRow = TRUE)
  }

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}

# ── Heatmap palette (Ben laboratory red-to-grey) ──────────
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
