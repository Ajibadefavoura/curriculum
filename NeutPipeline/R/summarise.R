# ============================================================
# NeutPipeline — R/summarise.R
# ============================================================

build_ic50_matrix <- function(qc_df, decimals = 2) {
  qc_df %>%
    dplyr::mutate(
      ic50_display = round(ic50, decimals),
      inv_ic50 = dplyr::if_else(qc_status %in% c("\u274c FAIL", "\u26ab Inactive"), 0, 1 / ic50),
      inv_ic50_display = round(inv_ic50, decimals + 4)
    )
}

pivot_ic50_wide <- function(summary_df) {
  summary_df %>%
    dplyr::select(sample_id, serotype, ic50_display, qc_status) %>%
    tidyr::pivot_wider(names_from = serotype, values_from = c(ic50_display, qc_status), names_glue = "{serotype}_{.value}")
}
