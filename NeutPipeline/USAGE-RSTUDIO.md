# NeutPipeline — RStudio User Guide (no Shiny required)

This guide is for analysts who want to run the full DENV
neutralization analysis directly from RStudio. No Shiny app, no
clicking, no UI — just one R script that reads your Excel,
fits all the curves, and writes every chart and spreadsheet
to disk.

If you ever want the Shiny UI again, it is still there —
`shiny::runApp(".")` works as before. This file documents the
**pure-R workflow only**.

---

## 1. One-time setup

### 1a. Install R and RStudio

If you don't already have them:

- R: <https://cran.r-project.org/>
- RStudio Desktop: <https://posit.co/download/rstudio-desktop/>

### 1b. Install the R packages

Open RStudio and run **once**:

```r
install.packages(c(
  "readxl", "openxlsx", "dplyr", "tidyr", "purrr",
  "drc", "ggplot2", "DT", "yaml", "scales", "rlang",
  "stringr", "glue", "tibble"
))
```

Takes a few minutes the first time. `drc` is the heaviest dependency.

### 1c. Get the code

Either clone the GitHub repo or download the ZIP and unzip it.
You should end up with a folder like:

```
C:/Users/Ajiba/Desktop/NeutPipeline/
├── run_neutpipeline.R   <-- main script
├── example_run.R        <-- copy and edit this
├── config.yml
├── global.R
├── R/
├── modules/
└── ...
```

---

## 2. Prepare your input files

NeutPipeline takes **one Excel file** and (optionally) one
plate-map CSV. The Excel file holds your raw FFU counts; the
plate map tells the pipeline which well belongs to which sample.

### 2a. Master FFU Excel

One sheet **per (serotype, plate)** combination. Sheet names
follow `<SEROTYPE>_<PLATE>` or `<SEROTYPE> <PLATE>`. Both
compact and spaced styles are accepted:

```
DENV1_P1   DENV1_P2   DENV1_P3
DENV2_P1   DENV2_P2   DENV2_P3
DENV3_P1   DENV3_P2   DENV3_P3
DENV4_P1   DENV4_P2   DENV4_P3
plate_map           <-- optional, see 2b
```

Equivalent forms: `DENV2 P1`, `DENV4_Plate2`, `DENV4 Furin Plate 1`.

Inside each sheet, paste the 8x12 grid of FFU counts. The
parser scans the first column for the row label `A`, then reads
the 8 rows by 12 columns immediately to its right.

### 2b. Plate map — three equivalent options

The plate map can be supplied in any of these formats. Pick whichever fits how you receive the data; the pipeline accepts all three.

**Option A — separate CSV file** (the most common workflow). For example `plate_map_denv4.csv`. You reference it from the script as:

```r
plate_map <- "C:/Users/Ajiba/Desktop/plate_map_denv4.csv"
```

**Option B — separate XLSX file.** For example `plate_map_denv4.xlsx` (the parser uses the first sheet, or a sheet named `plate_map` if one exists). You reference it the same way:

```r
plate_map <- "C:/Users/Ajiba/Desktop/plate_map_denv4.xlsx"
```

**Option C — embedded `plate_map` sheet inside the master FFU Excel.** Add a sheet named exactly `plate_map` next to your `DENV1_P1`, `DENV2_P1`, … sheets. Then:

```r
plate_map <- NULL    # the parser auto-detects the embedded sheet
```

Either way the columns are:

| column     | meaning                                      | example       |
|------------|----------------------------------------------|---------------|
| `plate`    | plate number (1, 2, 3, …)                    | `1`           |
| `sample_id`| sample label (your free-text ID)             | `id001`       |
| `row_start`| first plate row this sample occupies         | `A`           |
| `row_end`  | last plate row this sample occupies          | `B`           |
| `half`     | `left` (cols 1-6), `right` (7-12), or `full` | `left`        |
| `is_vc`    | `TRUE` if this row is the virus control      | `FALSE`       |
| `is_mock`  | `TRUE` if this row is the mock control       | `FALSE`       |

`is_vc` and `is_mock` accept any case (`TRUE`/`FALSE`/`true`/`False`).

You can also generate a starter template by running:

```r
source("server.R")  # one-off, just to expose the helper
# ...or simpler: use the example_run.R, see "out/" directory.
```

---

## 3. Run the analysis (90% of users start here)

1. Open RStudio.
2. **File → Open File…** → pick `example_run.R`.
3. Edit the three paths near the top:

   ```r
   master_excel <- "C:/Users/.../Experiment.xlsx"
   plate_map    <- NULL                       # or a CSV path
   out_dir      <- file.path("out",
                             format(Sys.Date(), "%Y-%m-%d"))
   ```

4. Click the **▶ Source** button at the top right of the editor
   (or press <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>S</kbd>).
5. Wait. You'll see progress messages in the console:

   ```
   ==> Loading configuration
   ==> Reading plate map
   ==> Parsing FFU sheets
   ==> Computing % neutralization
   ==> Fitting 4PL curves (LL.2 -> LL.4 retry chain)
   ==> Applying QC criteria
   ==> Writing XLSX exports
   ==> Building dose-response per-sample grid
   ==> Building dose-response per-plate panels
   ==> Building dose-response per-serotype panels
   ==> Building collective heatmap
   ==> Building per-plate spatial heatmaps
   ==> Building Layout 1 (IC50 ng/mL)
   ==> Building Layout 2 (1/IC50 potency)
   ==> Done. C:/Users/.../out/2026-05-04
   ```

6. On Windows, the output folder opens automatically in Explorer.
7. The console also gives you `results`, the in-memory bundle:

   ```r
   View(results$summary)              # IC50 per (sample, serotype)
   table(results$qc$qc_status)        # PASS / FAIL / Ambiguous / Inactive counts
   ```

That's it.

---

## 4. What you get in the output folder

For any number of serotypes (DENV1, DENV2, DENV3, DENV4 — or just one)
and any number of plates per serotype, NeutPipeline writes:

| File | Contents |
|------|----------|
| `raw-ffu.xlsx` | Long-format raw FFU counts joined to the plate map |
| `ic50-long.xlsx` | One row per (serotype, sample): IC50, Hill, R², CI, classification, censored report string |
| `ic50-matrix.xlsx` | Wide cross-serotype IC50 matrix (one row per sample, columns are `DENV1 IC50`, `DENV1 QC Status`, …) |
| `qc-full-detail.xlsx` | Same as `ic50-long` but every QC column + flags |
| `01-dose-response-per-sample.png` | Reference 5-column grid: one panel per sample with all serotypes overlaid, dashed 50% line, dashed IC50 drop lines |
| `02-dose-response-per-plate.png` | One panel per plate with samples coloured |
| `03-dose-response-per-serotype.png` | One panel per serotype (all DENV1 samples on one axis, all DENV2 on another, …) |
| `04-heatmap-collective.png` | Collective % neutralization heatmap (DENV1-4 × samples) using the laboratory red-to-grey palette |
| `05-heatmap-<serotype>-plate-<n>.png` | Per-plate spatial heatmap (8×12 wells), one per (serotype, plate) |
| `06-layout-1-ic50.png` | IC50 (ng/mL) bar chart, **log Y axis** so capped + quantified bars are both visible |
| `07-layout-2-potency.png` | 1/IC50 potency summary, log Y, dashed reference lines at IC50 = 100, 1,000, 10,000 |

All charts are 300 DPI, rendered with `theme_classic` + bold black axes, no grid lines, log X axis labelled `"ng/mL, purified mAb"`, and a Y axis at `0, 20, 40, 60, 80, 100`.

All Excel exports are bold-Calibri with frozen header row, alternating-row banding, and ASCII-only text (no mojibake).

---

## 4b. Single-serotype, multi-plate, two-file workflow (your DENV4 case)

If you currently have **one serotype across several plates** with the FFU data and the plate map in **two separate files**, that's the most common real-world case and it's fully supported.

Example: one DENV4 experiment across 3 plates, files like:

```
C:/Users/Ajiba/Desktop/Master_FFU_DENV4.xlsx
   sheets: DENV4_P1, DENV4_P2, DENV4_P3       (FFU grids only)

C:/Users/Ajiba/Desktop/plate_map_denv4.csv     (or .xlsx)
   columns: plate, sample_id, row_start, row_end,
            half, is_vc, is_mock
```

In `example_run.R` set:

```r
master_excel <- "C:/Users/Ajiba/Desktop/Master_FFU_DENV4.xlsx"
plate_map    <- "C:/Users/Ajiba/Desktop/plate_map_denv4.csv"
out_dir      <- file.path("out", format(Sys.Date(), "%Y-%m-%d"))
```

Click **Source** in RStudio. Output:

| File | What's in it |
|---|---|
| `01-dose-response-per-sample.png` | One panel per sample, all in DENV4 colour |
| `02-dose-response-per-plate.png` | **3 panels** — Plate 1, Plate 2, Plate 3, each with its samples coloured |
| `03-dose-response-per-serotype.png` | **1 panel** with all DENV4 samples overlaid |
| `04-heatmap-collective.png` | One row labelled `DENV4` × all your samples |
| `05-heatmap-DENV4-plate-1.png`, `05-heatmap-DENV4-plate-2.png`, `05-heatmap-DENV4-plate-3.png` | **3 spatial heatmaps** |
| `06-layout-1-ic50.png` | IC50 ng/mL bars, all in DENV4 colour |
| `07-layout-2-potency.png` | 1/IC50 potency summary |
| `raw-ffu.xlsx`, `ic50-long.xlsx`, `ic50-matrix.xlsx`, `qc-full-detail.xlsx` | All 3 plates merged |

Same script, same command, exactly your data shape. Whatever's in the Excel + plate map is what gets analysed.

---

## 5. Multi-serotype / multi-plate workflow (the DENV1-4 case)

You said you'll receive **DENV1 / DENV2 / DENV3 / DENV4 each with three plates**. Here's exactly how to handle that.

### 5a. One Excel, twelve sheets + plate_map

Create a single Excel file (e.g. `Experiment_2026_05.xlsx`) with thirteen sheets:

```
DENV1_P1   DENV1_P2   DENV1_P3
DENV2_P1   DENV2_P2   DENV2_P3
DENV3_P1   DENV3_P2   DENV3_P3
DENV4_P1   DENV4_P2   DENV4_P3
plate_map
```

Paste the 8×12 FFU grid into each of the twelve `DENV*_P*` sheets. Fill out `plate_map` once — the same plate map applies to every serotype because `plate` numbers (1, 2, 3) are reused per serotype.

### 5b. plate_map example for the 12-plate experiment

If samples on plate 1 occupy rows A–B (left half), C–D (left half), E–F (left half), with VC in row G and Mock in row H, and plates 2 and 3 follow the same layout:

```csv
plate,sample_id,row_start,row_end,half,is_vc,is_mock
1,id001,A,B,left,FALSE,FALSE
1,id002,C,D,left,FALSE,FALSE
1,id003,E,F,left,FALSE,FALSE
1,VC,G,G,left,TRUE,FALSE
1,Mock,H,H,left,FALSE,TRUE
2,id004,A,B,left,FALSE,FALSE
2,id005,C,D,left,FALSE,FALSE
2,id006,E,F,left,FALSE,FALSE
2,VC,G,G,left,TRUE,FALSE
2,Mock,H,H,left,FALSE,TRUE
3,id007,A,B,left,FALSE,FALSE
3,id008,C,D,left,FALSE,FALSE
3,id009,E,F,left,FALSE,FALSE
3,VC,G,G,left,TRUE,FALSE
3,Mock,H,H,left,FALSE,TRUE
```

If you also fill the right half of each plate (6-point split-plate design — the default), add `right` rows with `half = "right"`. If a single sample spans the whole row, use `half = "full"`.

### 5c. Run the analysis

```r
master_excel <- "C:/.../Experiment_2026_05.xlsx"
plate_map    <- NULL          # uses the embedded sheet
out_dir      <- "out/2026-05-04"

source("run_neutpipeline.R")
results <- np_run(
  master_excel = master_excel,
  plate_map    = plate_map,
  out_dir      = out_dir,
  sample_type  = "mab"
)
```

The output folder will then contain:

- `01-dose-response-per-sample.png` — every sample on its own panel, all four serotypes overlaid (the canonical reference grid).
- `02-dose-response-per-plate.png` — twelve panels (DENV1 P1, P2, P3, DENV2 P1, …, DENV4 P3) with samples coloured.
- `03-dose-response-per-serotype.png` — four panels (one per DENV1-4) with all samples for that serotype.
- `04-heatmap-collective.png` — DENV1, DENV2, DENV3, DENV4 stacked vertically × all samples horizontally.
- `05-heatmap-DENV1-plate-1.png`, `05-heatmap-DENV1-plate-2.png`, …, `05-heatmap-DENV4-plate-3.png` — twelve spatial heatmaps.
- `06-layout-1-ic50.png` — IC50 ng/mL bars, samples on X axis, four colours for the serotypes.
- `07-layout-2-potency.png` — 1/IC50 bars, four colours, dashed reference lines at IC50 = 100 and 1,000.

### 5d. Run all at once vs split

You asked: "Is it possible to run all at once?" Yes — that's exactly what `np_run()` does in the example above. One `source()` and one function call processes all 12 plates × 4 serotypes in a single pass and writes every figure and spreadsheet.

If you instead want to run a subset (say only DENV2), there is no flag for that — but you can either (a) put only DENV2 sheets in a separate Excel, or (b) filter `results$avg` and rebuild any chart manually:

```r
denv2_only <- dplyr::filter(results$avg, serotype == "DENV2")
np_plot_per_sample(denv2_only, dplyr::filter(results$fit, serotype == "DENV2"))
```

---

## 6. QC criteria (academically defensible)

Every IC50 lands in one of these categories — see the column `ic50_classification` in `qc-full-detail.xlsx`:

| Classification | Meaning | Reported as |
|---|---|---|
| `Quantified` | LLOQ ≤ IC50 ≤ ULOQ (in tested range) | numeric, e.g. `4.08` |
| `< LLOQ` | IC50 estimated below lowest tested concentration | `"< 4.88"` (left-censored) |
| `> ULOQ` | IC50 estimated above highest tested concentration | `"> 10000"` (right-censored) |
| `> Cap` | IC50 above the safety cap (default 20,000 ng/mL = 2× highest tested) | `"> 20000"` |
| `Solver pathology` | IC50 < 1e-10 ng/mL — sub-attomolar, biologically impossible, always a fit failure | `"ND"` |
| `Undetermined` | Solver did not converge after LL.2 → LL.4 retries | `"ND"` |
| `Inactive` | No neutralization detected | `"Inactive"` |

QC status flow (per UNC neutralization protocol + FDA / ICH M10 conventions):

- `|Hill slope| < 0.5` → **FAIL** (`qc_detail`: `"FAIL: Hill slope |…| < 0.5 (UNC criterion)"`).
- R² < 0.85 (mAb) or < 0.75 (polyclonal) → **FAIL**.
- Solver pathology or fit failure → **FAIL**.
- Censored (< LLOQ / > ULOQ / > Cap) → **PASS** with explicit `qc_detail` note (these are real measurements at the assay limit, not failures).
- Curve extrapolated outside tested range → **Ambiguous**.
- Otherwise → **PASS**.

---

## 7. Standard graph styling

Every dose-response chart uses the same Prism-style template:

- `theme_classic(base_size = 13)` — no grid, no panel border.
- Black axis lines (`linewidth = 0.7`), black tick marks (4 pt long).
- Bold black axis text and titles.
- X axis: log10, labelled `"ng/mL, purified mAb"`, ticks at `10^x`.
- Y axis: `"% Neutralized"`, fixed breaks `0, 20, 40, 60, 80, 100`, limits `0-110`.
- Dashed horizontal line at 50%.
- Dashed vertical drop line from y=50 down to each fitted IC50 (per-sample panels).
- Bold, left-aligned strip labels (no grey background).

Layout 1 / Layout 2 charts use the same theme with a 45°-rotated bold X axis text and a discrete legend at the bottom or right.

---

## 8. Running from the command line (advanced / batch)

Open PowerShell or Command Prompt:

```bat
cd C:\Users\Ajiba\Desktop\NeutPipeline
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" run_neutpipeline.R ^
   --master-excel "C:\Users\Ajiba\Desktop\data\Experiment.xlsx" ^
   --out          "out\2026-05-04" ^
   --sample-type  mab
```

(Replace the R version with whichever you have installed.) Equivalent to step 3 above. Returns when every PNG/XLSX is written.

---

## 9. Troubleshooting

| Symptom | Cause | Fix |
|---|---|---|
| `there is no package called 'drc'` | Skipped step 1b | Run the `install.packages(...)` block |
| `Master Excel not found` | Wrong path | Check `master_excel`; on Windows use forward slashes |
| `Plate map invalid: Each plate must have exactly one VC row.` | Missing `is_vc = TRUE` row | Add a VC row per plate |
| `The following sheet names do not follow the required <SEROTYPE>_<PLATE> format` | Sheet renamed unexpectedly | Rename to `DENV1_P1` etc. |
| All IC50s show as `ND` | Curve fitting failed for everything | Check VC counts aren't zero; check FFU values are present in the sheets |
| `Solver pathology` for many samples | Genuinely flat / chaotic curves | These are real biological negatives. Confirm by viewing `01-dose-response-per-sample.png` |

---

## 10. When to come back to the Shiny UI

The Shiny app still works (`shiny::runApp(".")` from inside the
NeutPipeline folder) and is useful when:

- You want to interactively tune QC thresholds (Hill, R², IC50 cap)
  without rerunning the full pipeline each time.
- You want to slice / filter / sort tables in the browser.
- You want plotly tooltips on the dose-response curves.

For routine analysis, the R-only workflow above is faster and
gives you publication-ready PNGs in a single source().
