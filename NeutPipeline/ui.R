# ============================================================
# NeutPipeline — ui.R
# ============================================================

# ── TASK 8 — Apple / Viridot inspired CSS ─────────────────
# Modernises the entire dashboard with a San Francisco-style
# system font stack, rounded corners, soft shadows, generous
# padding, and a minimalist neutral palette. Layout is left
# untouched; only typography, spacing and surface treatment
# change.
neutpipeline_css <- HTML("
:root {
  --np-bg:        #F5F5F7;
  --np-surface:   #FFFFFF;
  --np-text:      #1D1D1F;
  --np-muted:     #6E6E73;
  --np-accent:    #0A84FF;
  --np-success:   #34C759;
  --np-warning:   #FF9F0A;
  --np-danger:    #FF3B30;
  --np-border:    #E5E5EA;
  --np-shadow:    0 1px 2px rgba(0,0,0,0.04), 0 8px 24px rgba(0,0,0,0.06);
  --np-radius:    14px;
  --np-radius-sm: 10px;
}

html, body, .content-wrapper, .right-side, .main-footer,
.main-header, .main-sidebar, .skin-blue .main-header .logo,
.skin-blue .main-header .navbar, .box, .form-control,
.btn, .selectize-input, .nav-tabs > li > a, .dataTables_wrapper,
.shiny-input-container, .shiny-output-error,
.value-box, .info-box, .table {
  font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Text',
               'San Francisco', 'Segoe UI', 'Helvetica Neue',
               'Calibri', 'Roboto', Arial, sans-serif !important;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  letter-spacing: -0.005em;
  color: var(--np-text);
}

body, .content-wrapper, .right-side {
  background-color: var(--np-bg) !important;
}

/* ── Header ─────────────────────────────────────────── */
.skin-blue .main-header .logo,
.skin-blue .main-header .navbar {
  background-color: var(--np-surface) !important;
  color: var(--np-text) !important;
  border-bottom: 1px solid var(--np-border);
  box-shadow: var(--np-shadow);
}
.skin-blue .main-header .logo {
  font-weight: 600;
  font-size: 18px;
  letter-spacing: -0.02em;
}
.skin-blue .main-header .logo:hover { background-color: var(--np-surface) !important; }
.skin-blue .main-header .navbar .sidebar-toggle { color: var(--np-text) !important; }

/* ── Sidebar ────────────────────────────────────────── */
.skin-blue .main-sidebar {
  background-color: #1D1D1F !important;
}
.skin-blue .sidebar a, .skin-blue .sidebar h4,
.skin-blue .sidebar label, .skin-blue .sidebar .control-label {
  color: #F5F5F7 !important;
  font-weight: 500;
}
.skin-blue .sidebar-menu > li.active > a,
.skin-blue .sidebar-menu > li:hover > a {
  background: rgba(10,132,255,0.18) !important;
  border-left-color: var(--np-accent) !important;
  color: #FFFFFF !important;
}
.skin-blue .sidebar-menu > li > a {
  border-radius: 0 var(--np-radius-sm) var(--np-radius-sm) 0;
  margin: 2px 8px 2px 0;
  padding: 10px 14px;
}
.shiny-input-container { padding: 0 16px 8px 16px; width: 100%; }
.shiny-input-container .form-control,
.shiny-input-container .selectize-input { width: 100% !important; }
.shiny-input-container input[type='file'] { font-size: 12px; }
.skin-blue .sidebar .form-control,
.skin-blue .sidebar .selectize-input {
  background: rgba(255,255,255,0.06) !important;
  color: #FFFFFF !important;
  border: 1px solid rgba(255,255,255,0.15) !important;
}
.skin-blue .sidebar .selectize-input > input,
.skin-blue .sidebar .selectize-dropdown { color: #1D1D1F !important; }
.sidebar hr { border-top: 1px solid rgba(255,255,255,0.08); margin: 14px 12px; }

/* ── Boxes / Panels ────────────────────────────────── */
.box {
  background-color: var(--np-surface) !important;
  border: 1px solid var(--np-border) !important;
  border-radius: var(--np-radius) !important;
  box-shadow: var(--np-shadow) !important;
  padding: 6px 6px 14px 6px;
  margin-bottom: 22px;
}
.box-header {
  border-bottom: 1px solid var(--np-border);
  border-radius: var(--np-radius) var(--np-radius) 0 0 !important;
  padding: 14px 18px !important;
}
.box-header > .box-title {
  font-weight: 600;
  font-size: 15px;
  letter-spacing: -0.01em;
  color: var(--np-text);
}
.box.box-solid.box-primary  > .box-header { background: #0A84FF; }
.box.box-solid.box-info     > .box-header { background: #5AC8FA; }
.box.box-solid.box-success  > .box-header { background: #34C759; }
.box.box-solid.box-warning  > .box-header { background: #FF9F0A; }
.box.box-solid.box-danger   > .box-header { background: #FF3B30; }
.box.box-solid > .box-header,
.box.box-solid > .box-header > .box-title { color: #FFFFFF; }
.box-body { padding: 16px 18px !important; }

/* ── Buttons & inputs ──────────────────────────────── */
.btn, .btn-default, .btn-primary, .shiny-download-link {
  border-radius: 999px !important;
  border: 1px solid var(--np-border) !important;
  padding: 7px 18px !important;
  font-weight: 500 !important;
  letter-spacing: -0.005em;
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  transition: all 0.15s ease-in-out;
}
.btn:hover, .btn-default:hover, .btn-primary:hover, .shiny-download-link:hover {
  background: #F2F2F7 !important;
  transform: translateY(-1px);
  box-shadow: 0 2px 6px rgba(0,0,0,0.06);
}
#run_analysis {
  width: 90%;
  margin: 4px 5% 14px 5%;
  padding: 12px 16px !important;
  background: var(--np-accent) !important;
  border: none !important;
  color: #FFFFFF !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  border-radius: 12px !important;
  box-shadow: 0 4px 16px rgba(10,132,255,0.35);
}
#run_analysis:hover {
  background: #0071E3 !important;
  box-shadow: 0 6px 20px rgba(10,132,255,0.45);
}
.form-control, .selectize-input {
  border-radius: var(--np-radius-sm) !important;
  border: 1px solid var(--np-border) !important;
  box-shadow: none !important;
  padding: 8px 12px !important;
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
}
.selectize-input { min-height: 36px; }
.shiny-input-container > label,
.control-label {
  font-size: 12px;
  font-weight: 500;
  color: var(--np-muted) !important;
  text-transform: none;
  letter-spacing: 0;
}

/* ── DataTables ────────────────────────────────────── */
table.dataTable thead th {
  font-weight: 600 !important;
  background: #FAFAFC !important;
  border-bottom: 1px solid var(--np-border) !important;
}
table.dataTable tbody td { border-top: 1px solid #F2F2F7 !important; }
.dataTables_wrapper { padding: 6px 4px 0 4px; }

/* ── Plots ─────────────────────────────────────────── */
.shiny-plot-output, .plotly { border-radius: var(--np-radius-sm); overflow: hidden; }
")

ui <- shinydashboard::dashboardPage(
  skin = "blue",
  shinydashboard::dashboardHeader(title = "NeutPipeline Analysis"),
  shinydashboard::dashboardSidebar(
    width = 320,
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      shinydashboard::menuItem("Raw Data and Plate Map", tabName = "tab_raw", icon = icon("table")),
      shinydashboard::menuItem("Plate Heatmap", tabName = "tab_heat", icon = icon("th")),
      shinydashboard::menuItem("Dose Response Curves", tabName = "tab_curves", icon = icon("chart-line")),
      shinydashboard::menuItem("IC50 Results Table", tabName = "tab_ic50", icon = icon("list-alt")),
      shinydashboard::menuItem("IC50 Charts", tabName = "tab_charts", icon = icon("bar-chart")),
      shinydashboard::menuItem("QC Report", tabName = "tab_qc", icon = icon("check-circle"))
    ),
    tags$hr(),
    # ── TASK 7 — Prominent Run Analysis button ────────────
    shiny::actionButton(
      inputId = "run_analysis",
      label   = "Run Analysis",
      icon    = icon("play")
    ),
    uiOutput("run_status"),
    tags$hr(),
    tags$h4("Upload Data", style = "color:white; padding-left:15px; font-size:13px; font-weight:bold;"),
    fileInput("master_excel_file", label = "Master FFU File (xlsx)", accept = ".xlsx", placeholder = "e.g. Experiment 2026 04 15.xlsx"),
    fileInput("plate_map_file", label = "Plate Map (csv)", accept = ".csv", placeholder = "plate map.csv"),
    downloadButton("dl_plate_map_template", label = "Download Plate Map Template", style = "width:90%; margin:0 5% 8px 5%; font-size:11px;"),
    uiOutput("plate_map_status"),
    tags$hr(),
    textInput("experiment_name", "Experiment ID", value = ""),
    textInput("analyst_name", "Analyst Name", value = ""),
    tags$hr(),
    tags$h4("Concentrations", style = "color:white; padding-left:15px; font-size:13px; font-weight:bold;"),
    numericInput("start_conc", "Top Concentration (ng/mL)", value = cfg$concentration$starting_conc, min = 0),
    numericInput("dilution_fold", "Fold Dilution Between Steps", value = cfg$concentration$dilution_fold, min = 1),
    numericInput("n_steps", "Number of Dilution Steps", value = cfg$concentration$n_steps, min = 2, max = 24),
    textInput("conc_units", "Concentration Units Label", value = cfg$concentration$units),
    tags$hr(),
    tags$h4("Mixing Volumes", style = "color:white; padding-left:15px; font-size:13px; font-weight:bold;"),
    numericInput("vol_antibody", "Antibody Volume (\u00b5L)", value = cfg$mixing$vol_antibody_ul, min = 1),
    numericInput("vol_virus", "Virus Volume (\u00b5L)", value = cfg$mixing$vol_virus_ul, min = 1),
    verbatimTextOutput("dilution_factor_display"),
    radioButtons("conc_convention", "Report Concentrations As:",
                 choices  = c("Prepared (Before Mixing)" = "prepared",
                              "In Well (After Mixing)"   = "inwell"),
                 selected = cfg$mixing$convention),
    tags$hr(),
    tags$h4("Curve Fitting", style = "color:white; padding-left:15px; font-size:13px; font-weight:bold;"),
    radioButtons("model_type", "Model",
                 choices  = c("4PL Variable Slope"   = "4PL",
                              "3PL Fixed Slope of 1" = "3PL"),
                 selected = cfg$curve_fitting$model),
    numericInput("bottom_constraint", "Bottom Asymptote (Constrained)", value = cfg$curve_fitting$bottom),
    numericInput("top_constraint", "Top Asymptote (Constrained)", value = cfg$curve_fitting$top),
    checkboxInput("free_bottom", "Allow Bottom to Float Freely", value = cfg$curve_fitting$free_bottom),
    checkboxInput("free_top", "Allow Top to Float Freely", value = cfg$curve_fitting$free_top),
    radioButtons("sample_type", "Sample Type",
                 choices  = c("Monoclonal Antibody (R\u00b2 \u2265 0.85)" = "mab",
                              "Polyclonal Serum (R\u00b2 \u2265 0.75)"    = "polyclonal"),
                 selected = cfg$curve_fitting$sample_type),
    tags$hr(),
    tags$h4("QC Thresholds", style = "color:white; padding-left:15px; font-size:13px; font-weight:bold;"),
    numericInput("min_hill", "Minimum Hill Slope", value = cfg$qc$min_hillslope, min = 0, step = 0.05),
    numericInput("min_r2_mab", "Minimum R\u00b2 (Monoclonal Ab)", value = cfg$qc$min_r2_mab, min = 0, max = 1, step = 0.01),
    numericInput("min_r2_poly", "Minimum R\u00b2 (Polyclonal)", value = cfg$qc$min_r2_poly, min = 0, max = 1, step = 0.01),
    numericInput("ic50_cap", "IC50 Cap (ng/mL)", value = cfg$qc$ic50_cap, min = 0),
    numericInput("max_vc_cv", "Max VC Coefficient of Variation (%)", value = cfg$qc$max_vc_cv_pct, min = 0),
    numericInput("min_max_neut", "Min Max Percent Neut Required to Fit Curve", value = cfg$qc$min_max_neut_pct, min = 0),
    tags$hr(),
    tags$h4("Virus Control", style = "color:white; padding-left:15px; font-size:13px; font-weight:bold;"),
    radioButtons("vc_method", "VC Averaging Method",
                 choices  = c("Per Plate (Recommended)"   = "perplate",
                              "Combined Across Plates"    = "combined"),
                 selected = "perplate"),
    conditionalPanel(condition = "input.vc_method == 'combined'",
                     textInput("combined_vc_sero", "Target Serotype (e.g. DENV2)", value = ""),
                     textInput("combined_vc_plates", "Plate Numbers to Pool (Comma Separated)", value = "1,2"))
  ),
  shinydashboard::dashboardBody(
    tags$head(
      tags$title("NeutPipeline Analysis"),
      tags$style(neutpipeline_css)
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "tab_raw", mod_rawdata_ui("rawdata")),
      shinydashboard::tabItem(tabName = "tab_heat", mod_heatmap_ui("heatmap")),
      shinydashboard::tabItem(tabName = "tab_curves", mod_curves_ui("curves")),
      shinydashboard::tabItem(tabName = "tab_ic50", mod_ic50table_ui("ic50table")),
      shinydashboard::tabItem(tabName = "tab_charts", mod_charts_ui("charts")),
      shinydashboard::tabItem(tabName = "tab_qc", mod_qcreport_ui("qcreport"))
    )
  )
)
