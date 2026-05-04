# ============================================================
# NeutPipeline — R/qc_filter.R
# ============================================================

apply_qc <- function(fit_df, cfg, sample_type = "mab") {
  r2_threshold <- if (sample_type == "mab") cfg$qc$min_r2_mab else cfg$qc$min_r2_poly

  # ── TASK 5 — Hill-slope FAIL rule ────────────────────────
  # UNC neutralization QC (per the protocol PDF): the absolute
  # value of the Hill slope MUST be greater than the minimum
  # threshold (default 0.5). Anything below is a structural fit
  # failure and must be marked FAIL in both qc_status and
  # qc_detail. Fits that did not converge (hill_slope = NA) are
  # captured by the FitFailed branch below.
  fit_df %>%
    dplyr::mutate(
      fail_hill   = !is.na(hill_slope) & abs(hill_slope) < cfg$qc$min_hillslope,
      fail_r2     = !is.na(r_squared) & r_squared < r2_threshold,
      fail_fit    = fit_status %in% c("FitFailed", "Inactive"),
      warn_extrap = ic50_type %in% c("Extrapolated (below range)", "Extrapolated (above range)"),

      qc_status = dplyr::case_when(
        fit_status == "Inactive" ~ "\u26ab Inactive",
        fail_fit                 ~ "\u274c FAIL",
        fail_hill | fail_r2      ~ "\u274c FAIL",
        warn_extrap              ~ "\u26a0 Ambiguous",
        TRUE                     ~ "\u2705 PASS"
      ),

      qc_detail = dplyr::case_when(
        fit_status == "Inactive"  ~ "No neutralization detected",
        fit_status == "FitFailed" ~ paste("Curve fitting failed:", flags),
        fail_hill & fail_r2 ~ glue::glue(
          "FAIL: Hill slope |{round(hill_slope, 3)}| < {cfg$qc$min_hillslope}; ",
          "R\u00b2 = {round(r_squared, 3)} < {r2_threshold}"
        ),
        fail_hill ~ glue::glue(
          "FAIL: Hill slope |{round(hill_slope, 3)}| < ",
          "{cfg$qc$min_hillslope} (UNC criterion)"
        ),
        fail_r2 ~ glue::glue(
          "FAIL: R\u00b2 = {round(r_squared, 3)} < {r2_threshold}"
        ),
        warn_extrap ~ "IC50 is extrapolated",
        TRUE        ~ "Passed all QC criteria"
      )
    ) %>%
    dplyr::select(-fail_hill, -fail_r2, -fail_fit, -warn_extrap)
}
