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
  wide <- summary_df %>%
    dplyr::select(sample_id, serotype, dplyr::all_of(value_col), qc_status) %>%
    tidyr::pivot_wider(
      names_from  = serotype,
      values_from = c(dplyr::all_of(value_col), qc_status),
      names_glue  = "{serotype} {.value}"
    )
  # Replace internal value-suffix tokens with the publication
  # vocabulary so the pivoted columns read as e.g.
  # "DENV1 IC50" / "DENV1 QC Status" rather than
  # "DENV1 ic50_report" / "DENV1 qc_status".
  rename_map <- c(
    "ic50_report"  = "IC50",
    "ic50_display" = "IC50",
    "qc_status"    = "QC Status"
  )
  for (key in names(rename_map)) {
    colnames(wide) <- gsub(paste0("\\b", key, "\\b"),
                           rename_map[[key]], colnames(wide))
  }
  wide
}
