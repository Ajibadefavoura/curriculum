# ============================================================
# NeutPipeline — global.R
# ============================================================

# ── Libraries ─────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(readxl)
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
# Strip non-printable / non-ASCII glyphs from text columns and
# strip every non-numeric character from numeric columns so the
# downloaded CSVs are clean of encoding artefacts and stray
# math symbols (e.g. "≥", "✅", "❌", "⚠").
sanitize_text_column <- function(x) {
  s <- as.character(x)
  s <- iconv(s, from = "UTF-8", to = "ASCII", sub = "")
  s <- gsub("[^[:print:]]", "", s)
  trimws(s)
}

sanitize_numeric_column <- function(x) {
  if (is.numeric(x)) return(x)
  s <- as.character(x)
  s <- iconv(s, from = "UTF-8", to = "ASCII", sub = "")
  # Keep only digits, decimal points, sign and scientific notation.
  # Hyphen at the end of the character class is literal in POSIX.
  s <- gsub("[^[:digit:].eE+-]", "", s)
  suppressWarnings(as.numeric(s))
}

sanitize_for_export <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  numeric_cols <- c(
    "ic50", "ic50_display", "inv_ic50", "inv_ic50_display",
    "hill_slope", "r_squared", "ci_lower", "ci_upper",
    "concentration", "ffu_count", "vc_avg", "vc_sd",
    "vc_cv_pct", "mock_avg", "pct_neut", "pct_neut_avg",
    "pct_neut_rep1", "pct_neut_rep2", "plate", "replicate",
    "well_col", "n_reps"
  )
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::any_of(numeric_cols),
    ~ sanitize_numeric_column(.x)
  ))
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
