# ============================================================
# NeutPipeline — ui.R
# ============================================================
# Modern, publication-grade UI built on shinydashboard skeleton
# but heavily restyled. Patterns adopted from Viridot:
#   - Numbered "Step N." sections for explicit workflow ordering.
#   - Hover tooltips (`tags$div(title=...)`) on every input so
#     analysts get inline help without UI clutter.
#   - A dedicated "Help & Methods" tab documenting the statistical
#     basis (Ritz et al. 2015, FDA bioanalytical guidance, ICH M10,
#     Khoury et al. 2021) and citations.
# Visual layer: Inter / SF Pro Text typography, soft shadows,
# rounded corners, sticky Run-Analysis button, workflow stepper.

# ── Helper: tooltip-wrapped input ─────────────────────────
.np_tip <- function(content, tip) {
  shiny::tags$div(title = tip, class = "np-tip-wrapper", content)
}

# ── Helper: numbered sidebar section header ───────────────
.np_step <- function(n, title) {
  shiny::tags$div(
    class = "np-step-header",
    shiny::tags$span(class = "np-step-num", as.character(n)),
    shiny::tags$span(class = "np-step-title", title)
  )
}

neutpipeline_css <- HTML("
@import url('https://rsms.me/inter/inter.css');

:root {
  --np-bg:        #F5F5F7;
  --np-bg-2:      #EEEEF1;
  --np-surface:   #FFFFFF;
  --np-text:      #1D1D1F;
  --np-muted:     #6E6E73;
  --np-line:      #E5E5EA;
  --np-accent:    #0A84FF;
  --np-accent-2:  #0071E3;
  --np-success:   #34C759;
  --np-warning:   #FF9F0A;
  --np-danger:    #FF3B30;
  --np-shadow-sm: 0 1px 2px rgba(0,0,0,0.04);
  --np-shadow:    0 1px 2px rgba(0,0,0,0.04), 0 12px 32px rgba(0,0,0,0.08);
  --np-shadow-lg: 0 2px 4px rgba(0,0,0,0.06), 0 24px 48px rgba(0,0,0,0.10);
  --np-radius:    16px;
  --np-radius-sm: 10px;
  --np-radius-pill: 999px;
}

html, body, .content-wrapper, .right-side, .main-footer,
.main-header, .main-sidebar, .skin-blue .main-header .logo,
.skin-blue .main-header .navbar, .box, .form-control,
.btn, .selectize-input, .nav-tabs > li > a, .dataTables_wrapper,
.shiny-input-container, .shiny-output-error,
.value-box, .info-box, .table {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont,
               'SF Pro Text', 'San Francisco', 'Segoe UI',
               'Helvetica Neue', 'Calibri', 'Roboto', Arial,
               sans-serif !important;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  letter-spacing: -0.005em;
  color: var(--np-text);
  font-feature-settings: 'cv02', 'cv03', 'cv04', 'cv11';
}

body, .content-wrapper, .right-side {
  background: linear-gradient(180deg, var(--np-bg) 0%, var(--np-bg-2) 100%) !important;
  background-attachment: fixed;
}

/* ── Header ─────────────────────────────────────────────── */
.skin-blue .main-header .logo,
.skin-blue .main-header .navbar {
  background: rgba(255,255,255,0.85) !important;
  backdrop-filter: saturate(180%) blur(20px);
  -webkit-backdrop-filter: saturate(180%) blur(20px);
  color: var(--np-text) !important;
  border-bottom: 1px solid var(--np-line);
  box-shadow: var(--np-shadow-sm);
}
.skin-blue .main-header .logo {
  font-weight: 700;
  font-size: 17px;
  letter-spacing: -0.02em;
}
.skin-blue .main-header .logo:hover { background: rgba(255,255,255,0.85) !important; }
.skin-blue .main-header .navbar .sidebar-toggle { color: var(--np-text) !important; }
.skin-blue .main-header .navbar .sidebar-toggle:hover { background: rgba(0,0,0,0.04) !important; }

/* ── Sidebar ────────────────────────────────────────────── */
.skin-blue .main-sidebar {
  background: #161618 !important;
  border-right: 1px solid rgba(255,255,255,0.06);
}
.skin-blue .sidebar a, .skin-blue .sidebar h4,
.skin-blue .sidebar label, .skin-blue .sidebar .control-label,
.skin-blue .sidebar p, .skin-blue .sidebar span {
  color: #F5F5F7 !important;
  font-weight: 500;
}
.skin-blue .sidebar-menu > li > a {
  border-radius: 0 var(--np-radius-sm) var(--np-radius-sm) 0;
  margin: 2px 8px 2px 0;
  padding: 11px 14px;
  font-size: 13.5px;
  transition: all 0.15s ease;
}
.skin-blue .sidebar-menu > li:hover > a {
  background: rgba(10,132,255,0.10) !important;
  border-left-color: var(--np-accent) !important;
}
.skin-blue .sidebar-menu > li.active > a {
  background: rgba(10,132,255,0.20) !important;
  border-left-color: var(--np-accent) !important;
  color: #FFFFFF !important;
}
.shiny-input-container { padding: 0 16px 8px 16px; width: 100%; }
.shiny-input-container .form-control,
.shiny-input-container .selectize-input { width: 100% !important; }
.skin-blue .sidebar .form-control,
.skin-blue .sidebar .selectize-input {
  background: rgba(255,255,255,0.06) !important;
  color: #FFFFFF !important;
  border: 1px solid rgba(255,255,255,0.12) !important;
  border-radius: var(--np-radius-sm) !important;
  font-size: 13px;
}
.skin-blue .sidebar .selectize-input > input,
.skin-blue .sidebar .selectize-dropdown { color: #1D1D1F !important; }
.sidebar hr {
  border-top: 1px solid rgba(255,255,255,0.08);
  margin: 16px 14px;
}

/* ── Numbered step headers ──────────────────────────────── */
.np-step-header {
  display: flex; align-items: center;
  padding: 16px 16px 6px 16px;
  font-size: 11.5px; font-weight: 700;
  text-transform: uppercase; letter-spacing: 0.08em;
  color: rgba(255,255,255,0.55) !important;
}
.np-step-num {
  display: inline-flex; align-items: center; justify-content: center;
  width: 22px; height: 22px; margin-right: 10px;
  border-radius: 50%;
  background: rgba(10,132,255,0.18);
  color: #5AC8FA !important;
  font-weight: 700; font-size: 11px;
  border: 1px solid rgba(10,132,255,0.35);
}
.np-step-title { flex: 1; }

/* ── Sticky Run button ──────────────────────────────────── */
.np-run-zone {
  position: sticky; top: 0; z-index: 30;
  padding: 14px 16px 10px 16px;
  background: linear-gradient(180deg,
              rgba(22,22,24,0.96) 0%,
              rgba(22,22,24,0.86) 100%);
  backdrop-filter: blur(8px);
  border-bottom: 1px solid rgba(255,255,255,0.06);
}
#run_analysis {
  width: 100%;
  padding: 13px 18px !important;
  background: linear-gradient(135deg, #0A84FF 0%, #0071E3 100%) !important;
  border: none !important;
  color: #FFFFFF !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  letter-spacing: 0.01em;
  border-radius: var(--np-radius-sm) !important;
  box-shadow: 0 4px 16px rgba(10,132,255,0.40);
  transition: all 0.15s ease;
}
#run_analysis:hover {
  background: linear-gradient(135deg, #0071E3 0%, #0061CC 100%) !important;
  box-shadow: 0 6px 22px rgba(10,132,255,0.50);
  transform: translateY(-1px);
}
#run_analysis:active { transform: translateY(0); }

/* ── Boxes / Cards ──────────────────────────────────────── */
.box {
  background: var(--np-surface) !important;
  border: 1px solid var(--np-line) !important;
  border-radius: var(--np-radius) !important;
  box-shadow: var(--np-shadow) !important;
  padding: 6px 6px 14px 6px;
  margin-bottom: 22px;
  overflow: hidden;
  transition: box-shadow 0.2s ease;
}
.box:hover { box-shadow: var(--np-shadow-lg) !important; }
.box-header {
  border-bottom: 1px solid var(--np-line);
  border-radius: var(--np-radius) var(--np-radius) 0 0 !important;
  padding: 16px 20px !important;
}
.box-header > .box-title {
  font-weight: 600; font-size: 15px;
  letter-spacing: -0.01em; color: var(--np-text);
}
.box.box-solid.box-primary  > .box-header { background: linear-gradient(135deg, #0A84FF, #0071E3); }
.box.box-solid.box-info     > .box-header { background: linear-gradient(135deg, #5AC8FA, #00B8D4); }
.box.box-solid.box-success  > .box-header { background: linear-gradient(135deg, #34C759, #2EB04E); }
.box.box-solid.box-warning  > .box-header { background: linear-gradient(135deg, #FF9F0A, #E08800); }
.box.box-solid.box-danger   > .box-header { background: linear-gradient(135deg, #FF3B30, #D63027); }
.box.box-solid > .box-header,
.box.box-solid > .box-header > .box-title { color: #FFFFFF; }
.box-body { padding: 18px 20px !important; }

/* ── Buttons ────────────────────────────────────────────── */
.btn, .btn-default, .btn-primary, .shiny-download-link {
  border-radius: var(--np-radius-pill) !important;
  border: 1px solid var(--np-line) !important;
  padding: 7px 18px !important;
  font-weight: 500 !important;
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  transition: all 0.15s ease;
  box-shadow: var(--np-shadow-sm);
}
.btn:hover, .btn-default:hover, .btn-primary:hover,
.shiny-download-link:hover {
  background: #F2F2F7 !important;
  transform: translateY(-1px);
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
}
.btn:active { transform: translateY(0); }

/* ── Inputs ─────────────────────────────────────────────── */
.form-control, .selectize-input {
  border-radius: var(--np-radius-sm) !important;
  border: 1px solid var(--np-line) !important;
  box-shadow: none !important;
  padding: 8px 12px !important;
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  transition: border-color 0.15s ease, box-shadow 0.15s ease;
}
.form-control:focus, .selectize-input.focus {
  border-color: var(--np-accent) !important;
  box-shadow: 0 0 0 3px rgba(10,132,255,0.15) !important;
}
.shiny-input-container > label,
.control-label {
  font-size: 12px; font-weight: 500;
  color: var(--np-muted) !important;
  text-transform: none; letter-spacing: 0;
  margin-bottom: 4px;
}

/* ── DataTables ─────────────────────────────────────────── */
table.dataTable {
  border-collapse: separate !important;
  border-spacing: 0;
}
table.dataTable thead th {
  font-weight: 600 !important;
  background: #FAFAFC !important;
  border-bottom: 1px solid var(--np-line) !important;
  padding: 12px 14px !important;
}
table.dataTable tbody td {
  border-top: 1px solid #F2F2F7 !important;
  padding: 10px 14px !important;
}
table.dataTable tbody tr:hover { background: #F7F7FA !important; }
.dataTables_wrapper { padding: 6px 4px 0 4px; }
.dataTables_wrapper .dataTables_length select,
.dataTables_wrapper .dataTables_filter input {
  border-radius: var(--np-radius-sm) !important;
}

/* ── Plots / Plotly ─────────────────────────────────────── */
.shiny-plot-output, .plotly {
  border-radius: var(--np-radius-sm);
  overflow: hidden;
  background: var(--np-surface);
}

/* ── Tooltips (CSS-only) ────────────────────────────────── */
.np-tip-wrapper { position: relative; }

/* ── Value boxes ────────────────────────────────────────── */
.small-box { border-radius: var(--np-radius) !important; box-shadow: var(--np-shadow) !important; }
.small-box .icon > i { font-size: 70px !important; }

/* ── Progress notifications ─────────────────────────────── */
.shiny-notification {
  border-radius: var(--np-radius-sm) !important;
  box-shadow: var(--np-shadow-lg) !important;
  font-weight: 500;
}

/* ── Scrollbar ──────────────────────────────────────────── */
::-webkit-scrollbar { width: 10px; height: 10px; }
::-webkit-scrollbar-thumb {
  background: rgba(0,0,0,0.18);
  border-radius: 5px;
}
::-webkit-scrollbar-thumb:hover { background: rgba(0,0,0,0.28); }
::-webkit-scrollbar-track { background: transparent; }

/* ── Workflow stepper at top of body ───────────────────── */
.np-stepper {
  display: flex; align-items: center; justify-content: center;
  gap: 0;
  padding: 14px 24px;
  margin: 0 0 18px 0;
  background: var(--np-surface);
  border-radius: var(--np-radius);
  border: 1px solid var(--np-line);
  box-shadow: var(--np-shadow-sm);
}
.np-stepper .np-stage {
  display: flex; align-items: center;
  gap: 8px;
  padding: 0 12px;
  font-size: 12.5px; font-weight: 500;
  color: var(--np-muted);
  white-space: nowrap;
}
.np-stepper .np-stage.active {
  color: var(--np-accent);
  font-weight: 600;
}
.np-stepper .np-stage .dot {
  width: 22px; height: 22px;
  border-radius: 50%;
  display: inline-flex; align-items: center; justify-content: center;
  background: #F2F2F7;
  color: var(--np-muted);
  font-weight: 700; font-size: 11px;
  border: 1px solid var(--np-line);
}
.np-stepper .np-stage.active .dot {
  background: var(--np-accent);
  color: #FFFFFF;
  border-color: var(--np-accent);
}
.np-stepper .np-arrow {
  color: var(--np-line);
  font-size: 14px;
}

/* ── Help / Methods page styling ───────────────────────── */
.np-help h3 {
  margin-top: 28px; margin-bottom: 12px;
  font-size: 16px; font-weight: 700;
  letter-spacing: -0.01em;
}
.np-help p, .np-help li {
  font-size: 13.5px; line-height: 1.6; color: #2C2C2E;
}
.np-help code {
  background: #F2F2F7; padding: 2px 6px;
  border-radius: 4px; font-size: 12.5px;
  border: 1px solid var(--np-line);
}
.np-help blockquote {
  border-left: 3px solid var(--np-accent);
  padding: 8px 14px; margin: 12px 0;
  background: #F7F8FB; border-radius: 0 8px 8px 0;
  color: var(--np-muted); font-style: italic;
}
")

# ── Workflow stepper component ────────────────────────────
.np_stepper <- function() {
  tags$div(
    class = "np-stepper",
    tags$span(class = "np-stage active",
              tags$span(class = "dot", "1"), "Upload"),
    tags$span(class = "np-arrow", "\u203A"),
    tags$span(class = "np-stage active",
              tags$span(class = "dot", "2"), "Configure"),
    tags$span(class = "np-arrow", "\u203A"),
    tags$span(class = "np-stage active",
              tags$span(class = "dot", "3"), "Run Analysis"),
    tags$span(class = "np-arrow", "\u203A"),
    tags$span(class = "np-stage",
              tags$span(class = "dot", "4"), "Review"),
    tags$span(class = "np-arrow", "\u203A"),
    tags$span(class = "np-stage",
              tags$span(class = "dot", "5"), "Export")
  )
}

# ── Help / Methods tab content ────────────────────────────
.np_help_content <- function() {
  shinydashboard::box(
    title = "Help, Methods, and Statistical Basis",
    width = 12, status = "primary", solidHeader = TRUE,
    tags$div(class = "np-help",
      tags$h3("Workflow"),
      tags$ol(
        tags$li(tags$b("Upload"), " a master FFU Excel file. Sheet names follow ",
                tags$code("<SEROTYPE>_<PLATE>"), " e.g. ", tags$code("DENV2_P1"),
                ", ", tags$code("DENV4 Furin Plate 1"), ". An optional ",
                tags$code("plate_map"), " sheet (or separate CSV) defines wells."),
        tags$li(tags$b("Configure"), " starting concentration, fold dilution, ",
                "step count, mixing volumes, and QC thresholds. ",
                "Sliders are live."),
        tags$li(tags$b("Run Analysis"), " (sticky button). Triggers parsing, ",
                "%neutralization, 4PL fitting, QC, and chart generation."),
        tags$li(tags$b("Review"), " each tab. The IC50 Charts tab includes ",
                "Layout 1 (IC50 ng/mL), Layout 2 (1/IC50 potency), and the ",
                "publication-style ", tags$em("IC50 Summary"), " bar chart."),
        tags$li(tags$b("Export"), " bold-Calibri ", tags$code(".xlsx"),
                " spreadsheets and 300 DPI ", tags$code(".png"), " plots.")
      ),

      tags$h3("Plate layout"),
      tags$p("96-well plates support three orientations per sample, set in the ",
             tags$code("half"), " column of the plate map:"),
      tags$ul(
        tags$li(tags$code("left"), "  -- columns 1-6 (descending or ascending series)."),
        tags$li(tags$code("right"), " -- columns 7-12."),
        tags$li(tags$code("full"), "  -- columns 1 through ",
                tags$code("n_steps"), " stretched horizontally; supports up to 12 dilutions per row.")
      ),
      tags$p("VC and Mock controls are flagged via the boolean ",
             tags$code("is_vc"), " and ", tags$code("is_mock"),
             " columns. Excel-encoded ", tags$code("TRUE"), "/",
             tags$code("FALSE"), " strings are coerced to logical at ingest."),

      tags$h3("Curve fitting"),
      tags$p("4-parameter logistic via ", tags$code("drc::drm"),
             " (Ritz et al. 2015). The solver retries with three ",
             "specifications before failing:"),
      tags$ol(
        tags$li(tags$code("LL.2(upper = 100)"),
                " with constrained Hill / IC50 bounds (default, fastest)."),
        tags$li(tags$code("LL.4(upper = 100)"),
                " floating bottom, L-BFGS-B optimiser."),
        tags$li(tags$code("LL.4()"),
                " fully free, Nelder-Mead (last resort).")
      ),

      tags$h3("IC50 reporting and censoring"),
      tags$p("Following the FDA Bioanalytical Method Validation guidance (2018) ",
             "and ICH M10 (2022), IC50 values are reported using ",
             "left/right-censoring conventions rather than substituting ",
             "arbitrary numeric values:"),
      tags$ul(
        tags$li(tags$b("Quantified"), ": IC50 lies inside the tested concentration range ",
                "(LLOQ <= IC50 <= ULOQ). Reported as a numeric value."),
        tags$li(tags$b("< LLOQ"), ": IC50 estimated below the lowest tested concentration. ",
                "Reported as ", tags$code("\"< LLOQ\""), " (left-censored)."),
        tags$li(tags$b("> ULOQ"), ": IC50 estimated above the highest tested concentration. ",
                "Reported as ", tags$code("\"> ULOQ\""), " (right-censored)."),
        tags$li(tags$b("> Cap"), ": IC50 above the user-set cap (default 20,000 ng/mL = ",
                "2x highest tested). Reported as ", tags$code("\"> 20000\""), "."),
        tags$li(tags$b("Solver pathology"), ": IC50 below 1e-10 ng/mL (sub-attomolar). ",
                "These are ALWAYS algorithmic failures (boundary hits, near-flat curves) ",
                "and are reported as ", tags$code("\"ND\""),
                " (not determined). They are NOT treated as real measurements ",
                "in any summary statistic."),
        tags$li(tags$b("Inactive"), ": no neutralization detected. Reported as ",
                tags$code("\"Inactive\""), ".")
      ),
      tags$blockquote(
        "Conflating solver failures with real below-detection results yields ",
        "biased downstream summary statistics and is rejected by ",
        "high-impact journals. NeutPipeline keeps the underlying numeric ",
        "value (capped) for plotting purposes only; every analyst-facing ",
        "table and export uses the censored string."
      ),

      tags$h3("Quality control"),
      tags$p("UNC neutralization QC criterion (per protocol document):"),
      tags$ul(
        tags$li("|Hill slope| > ", tags$code("min_hillslope"),
                " (default 0.5). Below threshold -> ", tags$b("FAIL"), "."),
        tags$li("R\u00b2 >= ", tags$code("min_r2_mab"),
                " (0.85 mAb) or ", tags$code("min_r2_poly"),
                " (0.75 polyclonal). Below -> ", tags$b("FAIL"), "."),
        tags$li("Solver pathology -> ", tags$b("FAIL"),
                " regardless of numeric value."),
        tags$li("IC50 censored (< LLOQ / > ULOQ / > Cap) -> ",
                tags$b("PASS"), " with explicit qc_detail note."),
        tags$li("Curve extrapolated outside tested range -> ",
                tags$b("Ambiguous"), ".")
      ),

      tags$h3("References"),
      tags$ul(
        tags$li("Ritz C, Baty F, Streibig JC, Gerhard D. ",
                tags$em("Dose-Response Analysis Using R"), ". ",
                "PLOS ONE 10(12): e0146021 (2015)."),
        tags$li("FDA. ", tags$em("Bioanalytical Method Validation: ",
                "Guidance for Industry"), ". CDER (2018)."),
        tags$li("ICH M10. ", tags$em("Bioanalytical Method Validation ",
                "and Study Sample Analysis"), ". ICH (2022)."),
        tags$li("Khoury DS et al. ",
                tags$em("Neutralizing antibody levels are highly predictive ",
                "of immune protection from symptomatic SARS-CoV-2 infection"),
                ". Nat Med 27: 1205-1211 (2021).")
      )
    )
  )
}

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(title = "NeutPipeline"),
  shinydashboard::dashboardSidebar(
    width = 320,
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      shinydashboard::menuItem("Raw Data and Plate Map",  tabName = "tab_raw",     icon = icon("table")),
      shinydashboard::menuItem("Plate Heatmap",           tabName = "tab_heat",    icon = icon("th")),
      shinydashboard::menuItem("Dose Response Curves",    tabName = "tab_curves",  icon = icon("chart-line")),
      shinydashboard::menuItem("IC50 Results Table",      tabName = "tab_ic50",    icon = icon("list-alt")),
      shinydashboard::menuItem("IC50 Charts",             tabName = "tab_charts",  icon = icon("bar-chart")),
      shinydashboard::menuItem("QC Report",               tabName = "tab_qc",      icon = icon("check-circle")),
      shinydashboard::menuItem("Help & Methods",          tabName = "tab_help",    icon = icon("book"))
    ),
    tags$div(
      class = "np-run-zone",
      shiny::actionButton(
        inputId = "run_analysis",
        label   = "Run Analysis",
        icon    = icon("play")
      ),
      uiOutput("run_status")
    ),

    .np_step(1, "Upload"),
    .np_tip(
      fileInput("master_excel_file",
                "Master FFU file (.xlsx)",
                accept = ".xlsx",
                placeholder = "Drop Excel file here"),
      "Master VirIDot Excel: one sheet per (serotype, plate). A 'plate_map' sheet (or separate CSV) defines wells."
    ),
    .np_tip(
      fileInput("plate_map_file",
                "Plate map (.csv, optional)",
                accept = ".csv",
                placeholder = "Drop CSV here"),
      "Optional separate plate map. If empty, NeutPipeline reads the plate_map sheet from the Excel above."
    ),
    downloadButton("dl_plate_map_template",
                   label = "Download plate map template",
                   style = "width:90%; margin:0 5% 8px 5%; font-size:12px;"),
    uiOutput("plate_map_status"),

    .np_step(2, "Identify"),
    .np_tip(textInput("experiment_name", "Experiment ID", value = ""),
            "Free-text experiment identifier. Appears in exports."),
    .np_tip(textInput("analyst_name",    "Analyst Name",  value = ""),
            "Analyst who performed the assay. Appears in exports."),

    .np_step(3, "Concentrations"),
    .np_tip(numericInput("start_conc", "Top concentration (ng/mL)",
                         value = cfg$concentration$starting_conc, min = 0),
            "Starting (highest) concentration in the dilution series."),
    .np_tip(numericInput("dilution_fold", "Fold dilution between steps",
                         value = cfg$concentration$dilution_fold, min = 1),
            "Each step divides the previous concentration by this factor."),
    .np_tip(numericInput("n_steps", "Number of dilution steps",
                         value = cfg$concentration$n_steps, min = 2, max = 24),
            "Total number of concentrations in the series. >6 forces 'full' plate layout."),
    .np_tip(textInput("conc_units", "Concentration units label",
                      value = cfg$concentration$units),
            "Label shown on plot axes and tables."),

    .np_step(4, "Mixing"),
    .np_tip(numericInput("vol_antibody", "Antibody volume (\u00b5L)",
                         value = cfg$mixing$vol_antibody_ul, min = 1),
            "Volume of antibody/serum added per well."),
    .np_tip(numericInput("vol_virus", "Virus volume (\u00b5L)",
                         value = cfg$mixing$vol_virus_ul, min = 1),
            "Volume of virus inoculum added per well."),
    verbatimTextOutput("dilution_factor_display"),
    .np_tip(radioButtons("conc_convention", "Report concentrations as:",
                         choices  = c("Prepared (before mixing)" = "prepared",
                                      "In well (after mixing)"   = "inwell"),
                         selected = cfg$mixing$convention),
            "Whether reported concentrations reflect pre-mix or in-well values."),

    .np_step(5, "Curve fitting"),
    .np_tip(radioButtons("model_type", "Model",
                         choices  = c("4PL variable slope"   = "4PL",
                                      "3PL fixed slope of 1" = "3PL"),
                         selected = cfg$curve_fitting$model),
            "4PL = Hill slope free; 3PL = slope fixed at 1."),
    numericInput("bottom_constraint", "Bottom asymptote (constrained)",
                 value = cfg$curve_fitting$bottom),
    numericInput("top_constraint",    "Top asymptote (constrained)",
                 value = cfg$curve_fitting$top),
    checkboxInput("free_bottom", "Allow bottom to float freely",
                  value = cfg$curve_fitting$free_bottom),
    checkboxInput("free_top",    "Allow top to float freely",
                  value = cfg$curve_fitting$free_top),
    radioButtons("sample_type", "Sample type",
                 choices  = c("Monoclonal antibody (R\u00b2 \u2265 0.85)" = "mab",
                              "Polyclonal serum (R\u00b2 \u2265 0.75)"    = "polyclonal"),
                 selected = cfg$curve_fitting$sample_type),

    .np_step(6, "QC thresholds"),
    .np_tip(numericInput("min_hill", "Minimum |Hill slope|",
                         value = cfg$qc$min_hillslope, min = 0, step = 0.05),
            "UNC criterion: |Hill| < this value -> FAIL."),
    .np_tip(numericInput("min_r2_mab", "Minimum R\u00b2 (mAb)",
                         value = cfg$qc$min_r2_mab, min = 0, max = 1, step = 0.01),
            "UNC monoclonal antibody R^2 threshold."),
    .np_tip(numericInput("min_r2_poly", "Minimum R\u00b2 (polyclonal)",
                         value = cfg$qc$min_r2_poly, min = 0, max = 1, step = 0.01),
            "UNC polyclonal serum R^2 threshold."),
    .np_tip(numericInput("ic50_cap", "IC50 cap (ng/mL)",
                         value = cfg$qc$ic50_cap, min = 0),
            "Values above this are reported as '> cap' rather than as numeric measurements (FDA / ICH M10)."),
    numericInput("max_vc_cv",    "Max VC coefficient of variation (%)",
                 value = cfg$qc$max_vc_cv_pct, min = 0),
    numericInput("min_max_neut", "Min max %neut required to fit curve",
                 value = cfg$qc$min_max_neut_pct, min = 0),

    .np_step(7, "Virus control"),
    radioButtons("vc_method", "VC averaging method",
                 choices  = c("Per plate (recommended)" = "perplate",
                              "Combined across plates"  = "combined"),
                 selected = "perplate"),
    conditionalPanel(condition = "input.vc_method == 'combined'",
                     textInput("combined_vc_sero",
                               "Target serotype (e.g. DENV2)", value = ""),
                     textInput("combined_vc_plates",
                               "Plate numbers to pool (comma separated)",
                               value = "1,2"))
  ),
  shinydashboard::dashboardBody(
    tags$head(
      tags$title("NeutPipeline Analysis"),
      tags$style(neutpipeline_css)
    ),
    .np_stepper(),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "tab_raw",    mod_rawdata_ui("rawdata")),
      shinydashboard::tabItem(tabName = "tab_heat",   mod_heatmap_ui("heatmap")),
      shinydashboard::tabItem(tabName = "tab_curves", mod_curves_ui("curves")),
      shinydashboard::tabItem(tabName = "tab_ic50",   mod_ic50table_ui("ic50table")),
      shinydashboard::tabItem(tabName = "tab_charts", mod_charts_ui("charts")),
      shinydashboard::tabItem(tabName = "tab_qc",     mod_qcreport_ui("qcreport")),
      shinydashboard::tabItem(tabName = "tab_help",   .np_help_content())
    )
  )
)
