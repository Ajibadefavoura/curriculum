# ============================================================
# NeutPipeline — R/fit_curves.R
# ============================================================

fit_all_curves <- function(avg_df, cfg, conc_col = "concentration") {
  avg_df %>%
    dplyr::group_by(serotype, sample_id) %>%
    dplyr::group_modify(~ fit_single_curve(.x, cfg, conc_col)) %>%
    dplyr::ungroup()
}

fit_single_curve <- function(df, cfg, conc_col) {
  x     <- df[[conc_col]]
  y     <- df$pct_neut_avg
  plate <- unique(df$plate)

  if (all(y == 0 | is.na(y))) {
    return(make_fit_result(cfg$qc$ic50_cap, NA_real_, NA_real_,
                           NA_real_, NA_real_, "Inactive",
                           "All %neut values are zero", "Capped", plate))
  }

  keep <- is.finite(x) & x > 0 & is.finite(y)
  x <- x[keep]
  y <- y[keep]

  if (length(x) < 3) {
    return(make_fit_result(cfg$qc$ic50_cap, NA_real_, NA_real_,
                           NA_real_, NA_real_, "FitFailed",
                           "Insufficient finite data points", "Capped", plate))
  }

  max_neut <- max(y, na.rm = TRUE)
  inactive_flag <- if (max_neut < cfg$qc$min_max_neut_pct) {
    glue::glue("Max %neut below threshold")
  } else {
    NULL
  }

  ic50_floor <- as.numeric(cfg$qc$ic50_floor)
  ic50_cap   <- as.numeric(cfg$qc$ic50_cap)

  fit_result <- tryCatch({
    invisible(capture.output(
      capture.output(
        model <- suppressMessages(suppressWarnings(drc::drm(
          y ~ x,
          fct  = drc::LL.2(upper = 100),
          data = data.frame(x = x, y = y),
          lowerl  = c(-20, ic50_floor),
          upperl  = c(20,  ic50_cap * 10),
          control = drc::drmc(maxIt = cfg$curve_fitting$max_iter,
                              noMessage = TRUE)
        ))),
        type = "message"
      ),
      type = "output"
    ))

    ic50 <- drc::ED(model, 50, type = "absolute",
                    display = FALSE)[1, "Estimate"]
    hill <- abs(coef(model)[["b:(Intercept)"]])

    ci <- tryCatch({
      drc::ED(model, 50, type = "absolute", interval = "delta",
              level = cfg$curve_fitting$ci_level,
              display = FALSE)[1, c("Lower", "Upper")]
    }, error = function(e) c(Lower = NA_real_, Upper = NA_real_))

    y_pred <- predict(model)
    ss_res <- sum((y - y_pred)^2, na.rm = TRUE)
    ss_tot <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
    r2     <- if (ss_tot > 1e-9) 1 - ss_res / ss_tot else NA_real_

    ic50_type <- dplyr::case_when(
      is.na(ic50)                      ~ "Undetermined",
      ic50 >= min(x) & ic50 <= max(x)  ~ "Interpolated",
      ic50 < min(x)                    ~ "Extrapolated (below range)",
      TRUE                             ~ "Extrapolated (above range)"
    )

    # ── TASK 5 — Impossible-fit capping ──────────────────────
    # An IC50 below the floor (default 1e-10 ng/mL) is biologically
    # meaningless and is always a solver pathology (boundary hit,
    # near-flat curve, divide-by-zero in LL.2). Cap it to ic50_cap
    # AND mark the fit as failed so QC propagates "FAIL".
    impossibly_small <- !is.na(ic50) && ic50 < ic50_floor
    impossibly_large <- !is.na(ic50) && ic50 > ic50_cap

    flags <- c(
      inactive_flag,
      if (impossibly_small) "IC50 below floor; capped (fit error)",
      if (impossibly_large) "IC50 above cap; capped"
    )

    ic50_final <- dplyr::case_when(
      is.na(ic50)       ~ ic50_cap,
      impossibly_small  ~ ic50_cap,
      impossibly_large  ~ ic50_cap,
      TRUE              ~ ic50
    )

    final_status <- if (impossibly_small) "FitFailed" else "Fitted"
    final_type   <- if (impossibly_small) "Capped"    else ic50_type

    make_fit_result(ic50_final, hill, r2, ci["Lower"], ci["Upper"],
                    final_status,
                    paste(flags[!sapply(flags, is.null)], collapse = "; "),
                    final_type, plate)

  }, error = function(e) {
    make_fit_result(cfg$qc$ic50_cap, NA_real_, NA_real_,
                    NA_real_, NA_real_, "FitFailed",
                    paste("Fitting error:", conditionMessage(e)),
                    "Capped", plate)
  })

  fit_result
}

make_fit_result <- function(ic50, hill, r2, ci_lower, ci_upper,
                            status, flag, ic50_type, plate) {
  data.frame(
    plate      = plate,
    ic50       = ic50,
    hill_slope = hill,
    r_squared  = r2,
    ci_lower   = ci_lower,
    ci_upper   = ci_upper,
    fit_status = status,
    ic50_type  = ic50_type,
    flags      = flag,
    stringsAsFactors = FALSE
  )
}
