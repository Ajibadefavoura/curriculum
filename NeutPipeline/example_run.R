# ============================================================
# NeutPipeline — example_run.R
# ============================================================
# Copy this file, edit the three paths/parameters at the top,
# then in RStudio: Source the file (Ctrl+Shift+S) or step through.
# Every PNG and XLSX in the run lands in `out_dir`.
# ============================================================

# ---- 0. Make sure RStudio's working directory is the
#         NeutPipeline folder. If you opened this file by
#         double-clicking it, RStudio usually does this for
#         you; otherwise run the next line manually.
# setwd("C:/Users/Ajiba/Desktop/NeutPipeline")

# ---- 1. EDIT THESE THREE LINES ----------------------------

master_excel <- "C:/Users/Ajiba/Desktop/NeutPipeline/data/Experiment_2026.xlsx"
plate_map    <- NULL  # NULL = read from the 'plate_map' sheet inside the Excel
                      # OR provide a separate CSV path:
                      # "C:/Users/Ajiba/Desktop/NeutPipeline/data/plate_map.csv"
out_dir      <- file.path("out", format(Sys.Date(), "%Y-%m-%d"))

# ---- 2. (Optional) override defaults from config.yml ------
#
# Defaults (from config.yml) are:
#   start_conc     = 10000   ng/mL
#   dilution_fold  = 4       (1:4)
#   n_steps        = 6
#   vol_antibody   = 50 uL
#   vol_virus      = 50 uL
#
# Pass overrides only if your assay differs. Otherwise leave
# this whole section as-is.

start_conc     <- NULL  # e.g. 50000
dilution_fold  <- NULL  # e.g. 3 for 1:3 series
n_steps        <- NULL  # e.g. 8
vol_antibody   <- NULL
vol_virus      <- NULL
sample_type    <- "mab"   # or "polyclonal"
conc_convention <- "prepared"  # or "inwell"

# ---- 3. Run the entire pipeline ---------------------------

source("run_neutpipeline.R")

results <- np_run(
  master_excel    = master_excel,
  plate_map       = plate_map,
  out_dir         = out_dir,
  start_conc      = start_conc,
  dilution_fold   = dilution_fold,
  n_steps         = n_steps,
  vol_antibody    = vol_antibody,
  vol_virus       = vol_virus,
  conc_convention = conc_convention,
  sample_type     = sample_type
)

# ---- 4. Inspect results in RStudio ------------------------
# Every dataframe produced by the pipeline is in `results`:
#
#   results$parsed   - long-format raw FFU counts
#   results$neut     - %neutralization per well
#   results$avg      - %neutralization averaged across replicates
#   results$fit      - per-curve IC50 / Hill / R^2 fits
#   results$qc       - QC-classified results
#   results$summary  - publication-ready table with ic50_report
#                      ('< LLOQ', '> 20000', 'ND', etc.)
#   results$out_dir  - absolute path of the output folder

# Open the output folder in Windows Explorer (Windows only):
if (.Platform$OS.type == "windows") {
  shell.exec(results$out_dir)
}

# Quick view of the IC50 results in the RStudio Viewer:
View(results$summary)

# Quick PASS/FAIL/Ambiguous/Inactive summary:
table(results$qc$qc_status)
