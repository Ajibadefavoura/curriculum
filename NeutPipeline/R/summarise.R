# ============================================================
# NeutPipeline — R/summarise.R
# ============================================================

build_ic50_matrix <- function(qc_df, decimals = 2) {
  censored_classes <- c("< LLOQ", "> ULOQ", "> Cap",
                        "Solver pathology", "Undetermined", "Inactive")

  has_censored <- "ic50_censored" %in% names(qc_df)
  has_class    <- "ic50_classification" %in% names(qc_df)

  out <- qc_df %>%
    dplyr::mutate(ic50_display = round(ic50, decimals))

  # Manuscript-ready report string: prefer the censored / ND label
  # when the fit is below LLOQ, above ULOQ, above the cap, or a
  # solver pathology. Otherwise show the rounded numeric.
  if (has_censored && has_class) {
    out <- out %>% dplyr::mutate(
      ic50_report = dplyr::if_else(
        ic50_classification %in% censored_classes,
        as.character(ic50_censored),
        as.character(ic50_display)
      )
    )
  } else {
    out <- out %>% dplyr::mutate(
      ic50_report = as.character(ic50_display)
    )
  }

  # 1/IC50 is zero for FAIL / Inactive / non-quantifiable rows so
  # potency plots don't pretend a solver failure is a real number.
  invalid_for_potency <- out$qc_status %in% c("\u274c FAIL", "\u26ab Inactive")
  if (has_class) {
    invalid_for_potency <- invalid_for_potency |
      out$ic50_classification %in%
        c("Solver pathology", "Undetermined", "Inactive")
  }
  out <- out %>% dplyr::mutate(
    inv_ic50         = dplyr::if_else(invalid_for_potency, 0, 1 / ic50),
    inv_ic50_display = round(inv_ic50, decimals + 4)
  )

  out
}

pivot_ic50_wide <- function(summary_df) {
  value_col <- if ("ic50_report" %in% names(summary_df)) "ic50_report" else "ic50_display"
  summary_df %>%
    dplyr::select(sample_id, serotype, dplyr::all_of(value_col), qc_status) %>%
    tidyr::pivot_wider(
      names_from  = serotype,
      values_from = c(dplyr::all_of(value_col), qc_status),
      names_glue  = "{serotype}_{.value}"
    )
}
