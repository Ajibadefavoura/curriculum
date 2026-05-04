# ============================================================
# NeutPipeline — R/parse_viridot.R
# ============================================================

parse_master_excel <- function(excel_path, plate_map_df, concs, cfg) {
  all_sheets <- readxl::excel_sheets(excel_path)
  data_sheets <- all_sheets[!grepl("^plate_map$", all_sheets, ignore.case = TRUE)]

  # ── TASK 1 — Sheet naming bug fix ──────────────────────────
  # Accept both compact (DENV2_P1) and spaced (DENV4 Furin Plate 1)
  # naming styles.  The serotype is everything before the final
  # token, the plate number is the trailing integer.  Optional
  # underscore / "P" / "Plate" prefix in front of the digit so
  # legacy sheets still parse.
  valid_pattern <- "^(.+?)[ _]+(?:P(?:late)?[ _]?)?(\\d+)$"
  invalid <- data_sheets[!grepl(valid_pattern, data_sheets, ignore.case = TRUE, perl = TRUE)]
  if (length(invalid) > 0) {
    stop(glue::glue(
      "The following sheet names do not follow the required ",
      "<SEROTYPE>_<PLATE_NUMBER> format:\n",
      paste(invalid, collapse = ", ")
    ))
  }

  purrr::map_dfr(data_sheets, function(sheet_name) {
    m <- regmatches(
      sheet_name,
      regexec(valid_pattern, sheet_name, ignore.case = TRUE, perl = TRUE)
    )[[1]]
    serotype  <- trimws(m[2])
    plate_num <- as.integer(m[3])

    ffu_grid <- read_ffu_sheet(excel_path, sheet_name, cfg)
    pm_plate <- plate_map_df %>% dplyr::filter(plate == plate_num)

    if (nrow(pm_plate) == 0) {
      stop(glue::glue("No plate map entries found for plate {plate_num}."))
    }

    grid_to_long(ffu_grid, pm_plate, serotype, plate_num, concs, cfg)
  })
}

read_ffu_sheet <- function(excel_path, sheet_name, cfg) {
  raw <- readxl::read_excel(
    excel_path, sheet = sheet_name,
    col_names = FALSE, .name_repair = "minimal"
  )
  n_rows <- cfg$plate_layout$n_rows
  n_cols <- cfg$plate_layout$n_cols

  col1 <- toupper(trimws(as.character(raw[[1]])))
  start_idx <- match("A", col1)

  if (is.na(start_idx)) {
    stop(glue::glue(
      "Sheet '{sheet_name}': Could not find row label 'A' in the first column."
    ))
  }

  if (nrow(raw) < (start_idx + n_rows - 1)) {
    stop(glue::glue(
      "Sheet '{sheet_name}': Not enough data rows after 'A'."
    ))
  }

  ffu_matrix <- as.matrix(raw[start_idx:(start_idx + n_rows - 1), 2:(n_cols + 1)])
  mode(ffu_matrix) <- "numeric"

  rownames(ffu_matrix) <- LETTERS[1:n_rows]
  colnames(ffu_matrix) <- as.character(1:n_cols)

  return(ffu_matrix)
}

# ── Dynamic Column Assignment ─────────────────────────────
get_cols_used <- function(half, n_steps, cfg) {
  if (half == "left") {
    if (n_steps > 6) stop("Cannot use 'left' half with >6 dilution steps. Change plate map to 'full'.")
    cfg$plate_layout$left_half_cols[1:n_steps]
  } else if (half == "right") {
    if (n_steps > 6) stop("Cannot use 'right' half with >6 dilution steps. Change plate map to 'full'.")
    cfg$plate_layout$right_half_cols[1:n_steps]
  } else if (half == "full") {
    if (n_steps > 12) stop("Dilution steps cannot exceed 12 columns.")
    1:n_steps
  } else {
    stop(glue::glue("Invalid half value: {half}"))
  }
}

get_conc_map <- function(cols_used, half, concs, cfg) {
  direction <- if (half == "left") cfg$plate_layout$left_direction
  else if (half == "right") cfg$plate_layout$right_direction
  else "descending"

  if (direction == "ascending") setNames(rev(concs), cols_used)
  else setNames(concs, cols_used)
}

grid_to_long <- function(ffu_grid, plate_map, serotype, plate_num, concs, cfg) {
  n_steps <- length(concs)

  vc_entry <- plate_map %>% dplyr::filter(is_vc == TRUE)
  if (nrow(vc_entry) == 0) stop(glue::glue("Plate {plate_num}: no VC row found."))

  vc_rows <- get_row_range(vc_entry$row_start, vc_entry$row_end)
  vc_cols <- get_cols_used(vc_entry$half, n_steps, cfg)
  vc_vals <- as.numeric(ffu_grid[vc_rows, as.character(vc_cols)])
  vc_avg  <- mean(vc_vals, na.rm = TRUE)
  vc_cv   <- (sd(vc_vals, na.rm = TRUE) / vc_avg) * 100

  mock_entry <- plate_map %>% dplyr::filter(is_mock == TRUE)
  mock_avg <- if (nrow(mock_entry) > 0) {
    mock_rows <- get_row_range(mock_entry$row_start, mock_entry$row_end)
    mock_cols <- get_cols_used(mock_entry$half, n_steps, cfg)
    mean(as.numeric(ffu_grid[mock_rows, as.character(mock_cols)]), na.rm = TRUE)
  } else { NA_real_ }

  samples <- plate_map %>% dplyr::filter(is_vc == FALSE, is_mock == FALSE)

  purrr::map_dfr(seq_len(nrow(samples)), function(i) {
    smp       <- samples$sample_id[i]
    rows_used <- get_row_range(samples$row_start[i], samples$row_end[i])
    half      <- samples$half[i]

    cols_used <- get_cols_used(half, n_steps, cfg)
    conc_map  <- get_conc_map(cols_used, half, concs, cfg)

    purrr::map_dfr(seq_along(cols_used), function(j) {
      col_idx  <- cols_used[j]
      conc_val <- conc_map[[as.character(col_idx)]]

      purrr::map_dfr(seq_along(rows_used), function(r) {
        ffu_val <- tryCatch(
          as.numeric(ffu_grid[rows_used[r], as.character(col_idx)]),
          error = function(e) NA_real_
        )
        data.frame(
          serotype = serotype, plate = plate_num, sample_id = smp,
          concentration = conc_val,
          replicate = r, well_row = rows_used[r], well_col = col_idx,
          ffu_count = ffu_val, vc_avg = vc_avg,
          vc_sd = sd(vc_vals, na.rm = TRUE),
          vc_cv_pct = vc_cv, mock_avg = mock_avg,
          stringsAsFactors = FALSE
        )
      })
    })
  })
}

get_row_range <- function(row_start, row_end) {
  start_idx <- utf8ToInt(toupper(row_start)) - 64
  end_idx   <- utf8ToInt(toupper(row_end))   - 64
  LETTERS[start_idx:end_idx]
}

validate_plate_map <- function(pm) {
  errors <- c()
  required <- c("plate","sample_id","row_start","row_end","half","is_vc","is_mock")
  missing  <- setdiff(required, names(pm))
  if (length(missing) > 0) return(list(valid = FALSE, errors = c(errors, "Missing required columns")))

  bad_starts <- pm$row_start[!toupper(trimws(pm$row_start)) %in% LETTERS[1:8]]
  if (length(bad_starts) > 0) errors <- c(errors, "Invalid row_start values")

  bad_halves <- pm$half[!tolower(trimws(pm$half)) %in% c("left","right","full")]
  if (length(bad_halves) > 0) errors <- c(errors, "Invalid half values")

  vc_per_plate <- pm %>%
    dplyr::group_by(plate) %>%
    dplyr::summarise(
      n_vc = sum(is_vc == TRUE | is_vc == "TRUE"),
      .groups = "drop"
    )
  bad_vc <- vc_per_plate$plate[vc_per_plate$n_vc != 1]
  if (length(bad_vc) > 0) errors <- c(errors, "Each plate must have exactly one VC row.")

  list(valid = length(errors) == 0, errors = errors)
}
