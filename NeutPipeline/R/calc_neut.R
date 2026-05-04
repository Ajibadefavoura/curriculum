# ============================================================
# NeutPipeline — R/calc_neut.R
# ============================================================

calculate_pct_neut <- function(long_df, cfg, use_combined_vc = FALSE, combined_vc_plates = NULL) {
  if (use_combined_vc && !is.null(combined_vc_plates)) {
    long_df <- apply_combined_vc(long_df, combined_vc_plates)
  }

  long_df %>%
    dplyr::mutate(
      pct_neut_raw = (1 - ffu_count / vc_avg) * 100,
      pct_neut = dplyr::if_else(pct_neut_raw < 0, 0, pct_neut_raw),
      floored = pct_neut_raw < 0,
      plate_vc_flag = dplyr::case_when(
        is.na(vc_avg) | is.nan(vc_avg) ~ "ERROR: VC average is NA",
        vc_avg == 0 ~ "ERROR: VC average is zero",
        vc_cv_pct > cfg$qc$max_vc_cv_pct ~ glue::glue("WARNING: VC CV = {round(vc_cv_pct,1)}% exceeds threshold"),
        TRUE ~ "OK"
      ),
      plate_mock_flag = dplyr::case_when(
        is.na(mock_avg) ~ "No Mock wells defined",
        mock_avg > cfg$qc$max_mock_bg_pct ~ glue::glue("WARNING: Mock avg = {round(mock_avg,1)} FFU exceeds threshold"),
        TRUE ~ "OK"
      )
    )
}

average_replicates <- function(neut_df) {
  neut_df %>%
    dplyr::group_by(serotype, plate, sample_id, concentration, vc_avg, vc_cv_pct, plate_vc_flag, plate_mock_flag) %>%
    dplyr::summarise(
      pct_neut_rep1 = dplyr::first(pct_neut[replicate == 1]),
      pct_neut_rep2 = dplyr::first(pct_neut[replicate == 2]),
      pct_neut_avg  = mean(pct_neut, na.rm = TRUE),
      n_reps        = dplyr::n(),
      .groups       = "drop"
    )
}

apply_combined_vc <- function(long_df, combined_vc_plates) {
  purrr::map_dfr(unique(long_df$serotype), function(sero) {
    sero_df <- long_df %>% dplyr::filter(serotype == sero)
    if (sero %in% names(combined_vc_plates)) {
      src_plates <- combined_vc_plates[[sero]]
      combined_avg <- sero_df %>%
        dplyr::filter(plate %in% src_plates, sample_id == "VC") %>%
        dplyr::pull(ffu_count) %>%
        mean(na.rm = TRUE)
      sero_df <- sero_df %>% dplyr::mutate(vc_avg = combined_avg)
    }
    sero_df
  })
}
