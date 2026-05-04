# NeutPipeline Analysis

R Shiny application for DENV neutralization assay analysis. Calculates
IC50 values via Four Parameter Logistic (4PL) curves from VirIDot 96
well plate output, applies QC, and produces publication-ready charts.

## Layout

96 well split-plate with 6-point dilutions on the left and right halves.
Default protocol: 10000 ng/mL start, 6 dilution points, 1:4 dilution
factor. The app dynamically scales to any number of serotypes / plates
based on the sheet names in the uploaded master Excel file.

## Files

```
NeutPipeline/
├── app.R                 # Shiny entrypoint
├── global.R              # libraries, config loader, helpers
├── ui.R                  # dashboard UI + Apple-style CSS
├── server.R              # reactive pipeline (Run-button gated)
├── config.yml            # default thresholds
├── R/
│   ├── parse_viridot.R   # Excel ingestion, plate-map validation
│   ├── calc_neut.R       # % neutralization
│   ├── fit_curves.R      # 4PL via drc::LL.2
│   ├── qc_filter.R       # PASS/FAIL/Ambiguous/Inactive logic
│   └── summarise.R       # IC50 matrix builders
└── modules/
    ├── mod_rawdata.R
    ├── mod_heatmap.R
    ├── mod_curves.R      # interactive + Prism-style 4PL
    ├── mod_ic50table.R
    ├── mod_charts.R      # Layout 1 / Layout 2 / Prism potency
    └── mod_qcreport.R
```

## Running

```r
install.packages(c(
  "shiny", "shinydashboard", "readxl", "dplyr", "tidyr", "purrr",
  "drc", "ggplot2", "plotly", "DT", "yaml", "scales", "rlang",
  "stringr", "glue", "tibble"
))

setwd("NeutPipeline")
shiny::runApp(".")
```

## Sheet naming convention

Master Excel sheets must match `<SEROTYPE>_<PLATE>` where the trailing
integer is the plate number. Both compact and spaced styles are
accepted, e.g. `DENV2_P1`, `DENV4_Plate2`, `DENV4 Furin Plate 1`. A
`plate_map` sheet (or a separate `plate_map.csv`) defines sample wells.

## Workflow

1. Upload the master FFU Excel file and (optionally) a separate plate
   map CSV.
2. Adjust dilution / mixing / curve fitting / QC parameters in the
   sidebar — the live values feed the pipeline.
3. Click **Run Analysis** to parse, calculate %neut, fit 4PL curves,
   apply QC and refresh every tab.
4. Review tables / heatmaps / dose-response curves / charts.
5. Export sanitized CSVs and 300-DPI PNGs from each tab.
