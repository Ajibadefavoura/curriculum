# ============================================================
# NeutPipeline — R/fit_curves.R
# ============================================================
#
# Curve fitting for dose-response neutralization data.
#
# Statistical basis:
#   - Four-parameter logistic (4PL) model fitted via drc::drm using
#     the LL.x family. Reference: Ritz C, Baty F, Streibig JC,
#     Gerhard D (2015) "Dose-Response Analysis Using R."
#     PLOS ONE 10(12): e0146021. doi:10.1371/journal.pone.0146021
#   - IC50 = ED50 = the absolute concentration producing 50%
#     of the response between the fitted bottom and top.
#
# Censoring conventions (FDA Bioanalytical Method Validation
# 2018, ICH M10 2022, Khoury et al. Nature Medicine 2021):
#   - LLOQ: lower limit of quantitation = lowest tested concentration.
#   - ULOQ: upper limit of quantitation = highest tested concentration.
#   - IC50 < LLOQ      ->  reported as "< {LLOQ}"  (left-censored)
#   - IC50 > ULOQ      ->  reported as "> {ULOQ}"  (right-censored)
#   - Solver pathology ->  reported as "ND"  (not determined),
#                          NEVER as a numeric value.
#   - Inactive curve   ->  reported as "Inactive"
# This avoids the common pitfall of conflating real
# "below detection" results with solver failures, which yields
# biased downstream summary statistics.

# ── ic50_cap rationale ─────────────────────────────────────
# The numeric `ic50` column is *always* finite for downstream
# arithmetic (e.g. potency = 1/IC50 plots). When a value is not
# truly measurable, we (a) clamp the numeric to ic50_cap so plots
# don't blow up, AND (b) populate ic50_classification /
# ic50_censored so manuscript-ready exports never present a
# capped solver-pathology value as if it were a real measurement.

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

  ic50_floor <- as.numeric(cfg$qc$ic50_floor)
  ic50_cap   <- as.numeric(cfg$qc$ic50_cap)

  if (all(y == 0 | is.na(y))) {
    return(make_fit_result(
      ic50 = ic50_cap, hill = NA_real_, r2 = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_,
      status = "Inactive",
      flag = "All %neut values are zero",
      ic50_type = "Capped",
      classification = "Inactive",
      censored_str   = "Inactive",
      lloq = NA_real_, uloq = NA_real_, plate = plate
    ))
  }

  keep <- is.finite(x) & x > 0 & is.finite(y)
  x <- x[keep]; y <- y[keep]

  if (length(x) < 3) {
    return(make_fit_result(
      ic50 = ic50_cap, hill = NA_real_, r2 = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_,
      status = "FitFailed",
      flag = "Insufficient finite data points",
      ic50_type = "Capped",
      classification = "Undetermined",
      censored_str   = "ND",
      lloq = NA_real_, uloq = NA_real_, plate = plate
    ))
  }

  lloq <- min(x, na.rm = TRUE)
  uloq <- max(x, na.rm = TRUE)

  max_neut <- max(y, na.rm = TRUE)
  inactive_flag <- if (max_neut < cfg$qc$min_max_neut_pct) {
    glue::glue("Max %neut below threshold")
  } else {
    NULL
  }

  # ── Solver helper: try several drc fits before giving up ──
  # Some samples that look fittable to Prism cause the default
  # Nelder-Mead in drc to hit "Convergence failed". We retry with
  # progressively more permissive specifications:
  #   1. LL.2 with upper=100 (current default — fast, constrained)
  #   2. LL.4 with upper=100 floating-bottom (LM optimiser)
  #   3. LL.4 fully free (last resort)
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

  base_data <- data.frame(x = x, y = y)
  fit_specs <- list(
    list(
      formula = y ~ x, fct = drc::LL.2(upper = 100), data = base_data,
      lowerl  = c(-20, ic50_floor),
      upperl  = c(20,  ic50_cap * 10),
      control = drc::drmc(maxIt = cfg$curve_fitting$max_iter, noMessage = TRUE)
    ),
    list(
      formula = y ~ x,
      fct     = drc::LL.4(fixed = c(NA, NA, 100, NA)),
      data    = base_data,
      control = drc::drmc(method = "L-BFGS-B",
                          maxIt = cfg$curve_fitting$max_iter,
                          noMessage = TRUE)
    ),
    list(
      formula = y ~ x, fct = drc::LL.4(), data = base_data,
      control = drc::drmc(method = "Nelder-Mead",
                          maxIt = cfg$curve_fitting$max_iter,
                          noMessage = TRUE)
    )
  )

  model <- NULL
  for (spec in fit_specs) {
    model <- try_fit(spec)
    if (!is.null(model)) break
  }
  if (is.null(model)) {
    return(make_fit_result(
      ic50 = ic50_cap, hill = NA_real_, r2 = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_,
      status = "FitFailed",
      flag  = "Solver did not converge after LL.2/LL.4 retries",
      ic50_type = "Capped",
      classification = "Undetermined",
      censored_str   = "ND",
      lloq = lloq, uloq = uloq, plate = plate
    ))
  }

  fit_result <- tryCatch({
    ic50 <- drc::ED(model, 50, type = "absolute",
                    display = FALSE)[1, "Estimate"]
    coefs <- coef(model)
    hill_name <- grep("^b:", names(coefs), value = TRUE)[1]
    hill <- if (!is.na(hill_name)) abs(coefs[[hill_name]]) else NA_real_

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
      is.na(ic50)               ~ "Undetermined",
      ic50 >= lloq & ic50 <= uloq ~ "Interpolated",
      ic50 < lloq               ~ "Extrapolated (below range)",
      TRUE                      ~ "Extrapolated (above range)"
    )

    # ── Academic classification (Ritz et al. 2015; FDA / ICH M10) ─
    classify <- function(ic50, lloq, uloq, ic50_floor, ic50_cap) {
      if (is.na(ic50))                 return("Undetermined")
      if (ic50 < ic50_floor)           return("Solver pathology")
      if (ic50 > ic50_cap)             return("> Cap")
      if (ic50 < lloq)                 return("< LLOQ")
      if (ic50 > uloq)                 return("> ULOQ")
      "Quantified"
    }

    classification <- classify(ic50, lloq, uloq, ic50_floor, ic50_cap)

    # ── Manuscript-ready censored string ────────────────────
    censored_str <- switch(
      classification,
      "Quantified"        = formatC(ic50, format = "fg", digits = 4),
      "< LLOQ"            = sprintf("< %s", formatC(lloq, format = "fg", digits = 4)),
      "> ULOQ"            = sprintf("> %s", formatC(uloq, format = "fg", digits = 4)),
      "> Cap"             = sprintf("> %s", formatC(ic50_cap, format = "d")),
      "Solver pathology"  = "ND",
      "Undetermined"      = "ND",
      "Inactive"          = "Inactive",
      "ND"
    )

    # Numeric column always finite for downstream arithmetic
    # (potency plots, mean/median across samples).
    ic50_final <- dplyr::case_when(
      is.na(ic50)            ~ ic50_cap,
      ic50 < ic50_floor      ~ ic50_cap,
      ic50 > ic50_cap        ~ ic50_cap,
      TRUE                   ~ ic50
    )

    # Fit status: only solver pathologies and Inactive samples
    # fail outright. Above-/below-range fits are kept as Fitted
    # but the censored string makes the limitation explicit.
    final_status <- dplyr::case_when(
      classification == "Solver pathology" ~ "FitFailed",
      classification == "Undetermined"     ~ "FitFailed",
      TRUE                                 ~ "Fitted"
    )
    final_type <- if (classification %in% c("Solver pathology", "Undetermined")) {
      "Capped"
    } else {
      ic50_type
    }

    flags <- c(
      inactive_flag,
      if (classification == "Solver pathology")
        "IC50 below floor; reported as ND (solver pathology)",
      if (classification == "> Cap")
        glue::glue("IC50 > {ic50_cap} ng/mL; reported as '> {ic50_cap}'"),
      if (classification == "< LLOQ")
        sprintf("IC50 below LLOQ (%.4g); left-censored", lloq),
      if (classification == "> ULOQ")
        sprintf("IC50 above ULOQ (%.4g); right-censored", uloq)
    )

    make_fit_result(
      ic50 = ic50_final, hill = hill, r2 = r2,
      ci_lower = ci["Lower"], ci_upper = ci["Upper"],
      status = final_status,
      flag   = paste(flags[!sapply(flags, is.null)], collapse = "; "),
      ic50_type = final_type,
      classification = classification,
      censored_str   = censored_str,
      lloq = lloq, uloq = uloq, plate = plate
    )

  }, error = function(e) {
    make_fit_result(
      ic50 = ic50_cap, hill = NA_real_, r2 = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_,
      status = "FitFailed",
      flag   = paste("Fitting error:", conditionMessage(e)),
      ic50_type = "Capped",
      classification = "Undetermined",
      censored_str   = "ND",
      lloq = lloq, uloq = uloq, plate = plate
    )
  })

  fit_result
}

make_fit_result <- function(ic50, hill, r2, ci_lower, ci_upper,
                            status, flag, ic50_type,
                            classification, censored_str,
                            lloq, uloq, plate) {
  data.frame(
    plate                = plate,
    ic50                 = ic50,
    hill_slope           = hill,
    r_squared            = r2,
    ci_lower             = ci_lower,
    ci_upper             = ci_upper,
    fit_status           = status,
    ic50_type            = ic50_type,
    ic50_classification  = classification,
    ic50_censored        = censored_str,
    lloq                 = lloq,
    uloq                 = uloq,
    flags                = flag,
    stringsAsFactors     = FALSE
  )
}
