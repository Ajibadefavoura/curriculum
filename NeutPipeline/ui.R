# ============================================================
# NeutPipeline — ui.R
# ============================================================
# Modern, light, card-based UI built on top of shinydashboard's
# skeleton but with the entire visual layer overridden.
# Inspired by Linear / Notion / Stripe Dashboard / Posit Connect.

# ── Helper: tooltip-wrapped input ─────────────────────────
.np_tip <- function(content, tip) {
  shiny::tags$div(title = tip, class = "np-tip", content)
}

# ── Helper: numbered sidebar section header ───────────────
.np_step <- function(n, title) {
  shiny::tags$div(
    class = "np-step",
    shiny::tags$span(class = "np-step__num", as.character(n)),
    shiny::tags$span(class = "np-step__title", title)
  )
}

# ── Helper: light card-style box (replaces shinydashboard::box) ──
.np_card <- function(..., title = NULL, subtitle = NULL,
                     icon = NULL, accent = NULL, width = 12,
                     class = NULL) {
  header <- if (!is.null(title)) {
    shiny::tags$div(
      class = "np-card__header",
      if (!is.null(accent)) shiny::tags$span(class = paste0("np-card__accent np-card__accent--", accent)),
      if (!is.null(icon))   shiny::tags$span(class = "np-card__icon", shiny::icon(icon)),
      shiny::tags$div(
        class = "np-card__titles",
        shiny::tags$h3(class = "np-card__title", title),
        if (!is.null(subtitle))
          shiny::tags$p(class = "np-card__subtitle", subtitle)
      )
    )
  } else NULL

  shiny::div(
    class = paste("col-sm-", width, sep = ""),
    shiny::tags$section(
      class = paste(c("np-card", class), collapse = " "),
      header,
      shiny::tags$div(class = "np-card__body", ...)
    )
  )
}

# ── Theme ────────────────────────────────────────────────
neutpipeline_css <- HTML("
@import url('https://rsms.me/inter/inter.css');

:root {
  --np-bg:        #FAFAFB;
  --np-bg-deep:   #F2F2F5;
  --np-surface:   #FFFFFF;
  --np-surface-2: #F7F7F9;
  --np-text:      #0F1115;
  --np-text-2:    #2C2E33;
  --np-muted:     #6E6E73;
  --np-line:      #E5E5EA;
  --np-line-2:    #EFEFF1;
  --np-accent:    #2563EB;
  --np-accent-2:  #1D4ED8;
  --np-accent-soft: rgba(37,99,235,0.10);
  --np-success:   #16A34A;
  --np-warning:   #D97706;
  --np-danger:    #DC2626;
  --np-shadow-xs: 0 1px 2px rgba(15,17,21,0.04);
  --np-shadow-sm: 0 2px 6px rgba(15,17,21,0.05),
                  0 1px 2px rgba(15,17,21,0.04);
  --np-shadow:    0 4px 12px rgba(15,17,21,0.06),
                  0 1px 2px rgba(15,17,21,0.04);
  --np-shadow-lg: 0 12px 32px rgba(15,17,21,0.08),
                  0 2px 6px rgba(15,17,21,0.05);
  --np-radius:    14px;
  --np-radius-sm: 10px;
  --np-radius-xs: 6px;
  --np-pill:      999px;
}

* { box-sizing: border-box; }

html, body, .content-wrapper, .right-side, .main-footer,
.main-header, .main-sidebar,
.box, .form-control, .btn, .selectize-input,
.nav-tabs > li > a, .dataTables_wrapper,
.shiny-input-container, .shiny-output-error,
.value-box, .info-box, .table {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont,
               'SF Pro Text', 'Segoe UI', 'Helvetica Neue',
               'Calibri', 'Roboto', Arial, sans-serif !important;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  letter-spacing: -0.005em;
  font-feature-settings: 'cv02','cv03','cv04','cv11';
}

body, .content-wrapper, .right-side {
  background: var(--np-bg) !important;
  color: var(--np-text) !important;
}

/* ── Header ─────────────────────────────────────────────── */
.skin-blue .main-header .logo,
.skin-blue .main-header .navbar {
  background: rgba(255,255,255,0.92) !important;
  backdrop-filter: saturate(180%) blur(18px);
  -webkit-backdrop-filter: saturate(180%) blur(18px);
  color: var(--np-text) !important;
  border-bottom: 1px solid var(--np-line);
  box-shadow: var(--np-shadow-xs);
}
.skin-blue .main-header .logo {
  font-weight: 700; font-size: 16px; letter-spacing: -0.01em;
}
.skin-blue .main-header .logo:hover  { background: rgba(255,255,255,0.92) !important; }
.skin-blue .main-header .navbar .sidebar-toggle { color: var(--np-text) !important; }
.skin-blue .main-header .navbar .sidebar-toggle:hover { background: rgba(0,0,0,0.04) !important; }

/* ── Sidebar (light, modern) ────────────────────────────── */
.skin-blue .main-sidebar {
  background: var(--np-surface) !important;
  border-right: 1px solid var(--np-line);
  box-shadow: 1px 0 0 var(--np-line);
}
.skin-blue .sidebar a, .skin-blue .sidebar h4,
.skin-blue .sidebar label, .skin-blue .sidebar .control-label,
.skin-blue .sidebar p, .skin-blue .sidebar span,
.skin-blue .sidebar li, .skin-blue .sidebar div {
  color: var(--np-text) !important;
}
.skin-blue .sidebar-menu > li > a {
  color: var(--np-text-2) !important;
  border-radius: var(--np-radius-sm);
  margin: 2px 10px;
  padding: 10px 12px;
  font-size: 13.5px; font-weight: 500;
  border-left: none !important;
  transition: background 0.12s ease;
}
.skin-blue .sidebar-menu > li:hover > a {
  background: var(--np-surface-2) !important;
  color: var(--np-text) !important;
}
.skin-blue .sidebar-menu > li.active > a {
  background: var(--np-accent-soft) !important;
  color: var(--np-accent) !important;
  border-left: 3px solid var(--np-accent) !important;
  padding-left: 9px;
  font-weight: 600;
}
.skin-blue .sidebar-menu > li.active > a > .fa { color: var(--np-accent) !important; }

/* Sidebar inputs - light theme */
.shiny-input-container { padding: 0 18px 10px 18px; width: 100%; }
.shiny-input-container > label,
.skin-blue .sidebar .control-label {
  font-size: 11.5px !important;
  font-weight: 600 !important;
  color: var(--np-muted) !important;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  margin-bottom: 6px;
}
.skin-blue .sidebar .form-control,
.skin-blue .sidebar .selectize-input {
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  border: 1px solid var(--np-line) !important;
  border-radius: var(--np-radius-sm) !important;
  font-size: 13.5px !important;
  padding: 9px 12px !important;
  box-shadow: var(--np-shadow-xs) !important;
  height: auto !important;
}
.skin-blue .sidebar .form-control:focus,
.skin-blue .sidebar .selectize-input.focus {
  border-color: var(--np-accent) !important;
  box-shadow: 0 0 0 3px rgba(37,99,235,0.15) !important;
}
.skin-blue .sidebar .selectize-dropdown,
.skin-blue .sidebar .selectize-dropdown-content {
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  border: 1px solid var(--np-line) !important;
  border-radius: var(--np-radius-sm) !important;
}
.skin-blue .sidebar .selectize-dropdown-content .option { color: var(--np-text) !important; }
.skin-blue .sidebar .selectize-dropdown-content .option.active { background: var(--np-accent-soft) !important; }

/* File input - drop-zone style */
.shiny-input-container input[type='file'] {
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  border: 1.5px dashed var(--np-line) !important;
  border-radius: var(--np-radius-sm) !important;
  padding: 10px 12px !important;
  width: 100% !important;
  font-size: 12.5px !important;
  cursor: pointer;
}
.shiny-input-container input[type='file']:hover {
  border-color: var(--np-accent) !important;
  background: var(--np-accent-soft) !important;
}
.btn-file {
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  border: 1px solid var(--np-line) !important;
  border-radius: var(--np-radius-sm) !important;
}
.input-group, .input-group-btn { width: 100%; }
.shiny-file-input-progress { display: none; }

/* Radio + checkbox groups */
.skin-blue .sidebar .radio,
.skin-blue .sidebar .checkbox {
  margin: 6px 0;
}
.skin-blue .sidebar .radio label,
.skin-blue .sidebar .checkbox label {
  color: var(--np-text) !important;
  font-size: 13px !important;
  font-weight: 400 !important;
  text-transform: none !important;
  letter-spacing: 0 !important;
}
.skin-blue .sidebar input[type='radio'],
.skin-blue .sidebar input[type='checkbox'] {
  accent-color: var(--np-accent);
  margin-right: 6px;
}

/* Conditional panel inputs inherit too */
.skin-blue .sidebar .form-group { margin-bottom: 12px; }

/* Sidebar HR */
.sidebar hr { border-top: 1px solid var(--np-line); margin: 16px 18px; }

/* ── Numbered step headers ──────────────────────────────── */
.np-step {
  display: flex; align-items: center;
  padding: 16px 18px 8px 18px;
  font-size: 11px; font-weight: 700;
  text-transform: uppercase; letter-spacing: 0.10em;
  color: var(--np-muted) !important;
}
.np-step__num {
  display: inline-flex; align-items: center; justify-content: center;
  width: 20px; height: 20px; margin-right: 10px;
  border-radius: 50%;
  background: var(--np-accent-soft);
  color: var(--np-accent) !important;
  font-weight: 700; font-size: 10.5px;
}
.np-step__title { flex: 1; color: var(--np-muted) !important; }

/* ── Sticky Run-Analysis zone ───────────────────────────── */
.np-run-zone {
  position: sticky; top: 0; z-index: 30;
  padding: 16px 18px 12px 18px;
  background: var(--np-surface);
  border-bottom: 1px solid var(--np-line);
  box-shadow: 0 4px 8px -8px rgba(0,0,0,0.10);
}
#run_analysis {
  width: 100%;
  padding: 12px 18px !important;
  background: var(--np-accent) !important;
  border: none !important;
  color: #FFFFFF !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  letter-spacing: 0.005em;
  border-radius: var(--np-radius-sm) !important;
  box-shadow: 0 4px 12px rgba(37,99,235,0.30);
  transition: all 0.12s ease;
}
#run_analysis:hover {
  background: var(--np-accent-2) !important;
  box-shadow: 0 6px 18px rgba(37,99,235,0.40);
  transform: translateY(-1px);
}
#run_analysis:active { transform: translateY(0); }

.np-run-zone .shiny-html-output { font-size: 12px; color: var(--np-muted); margin-top: 8px; }
.np-run-zone .shiny-html-output p { color: var(--np-muted) !important; padding-left: 0 !important; }

/* ── Cards (replaces colored shinydashboard boxes) ──────── */
.np-card {
  background: var(--np-surface);
  border: 1px solid var(--np-line);
  border-radius: var(--np-radius);
  box-shadow: var(--np-shadow-sm);
  padding: 0;
  margin-bottom: 18px;
  overflow: hidden;
  transition: box-shadow 0.16s ease;
}
.np-card:hover { box-shadow: var(--np-shadow); }
.np-card__header {
  display: flex; align-items: center; gap: 12px;
  padding: 16px 22px;
  border-bottom: 1px solid var(--np-line-2);
  background: var(--np-surface);
  position: relative;
}
.np-card__accent {
  position: absolute; left: 0; top: 14px; bottom: 14px;
  width: 3px; border-radius: 0 2px 2px 0;
  background: var(--np-accent);
}
.np-card__accent--info     { background: #2563EB; }
.np-card__accent--success  { background: #16A34A; }
.np-card__accent--warning  { background: #D97706; }
.np-card__accent--danger   { background: #DC2626; }
.np-card__accent--neutral  { background: #6E6E73; }
.np-card__icon {
  display: inline-flex; align-items: center; justify-content: center;
  width: 32px; height: 32px;
  border-radius: 8px;
  background: var(--np-accent-soft);
  color: var(--np-accent);
  font-size: 14px;
  flex-shrink: 0;
}
.np-card__titles { flex: 1; min-width: 0; }
.np-card__title {
  margin: 0; font-size: 15px; font-weight: 600;
  letter-spacing: -0.01em; color: var(--np-text);
  line-height: 1.3;
}
.np-card__subtitle {
  margin: 2px 0 0 0; font-size: 12.5px;
  color: var(--np-muted); font-weight: 400;
}
.np-card__body { padding: 18px 22px; color: var(--np-text); }

/* Force every shinydashboard ::box (including solidHeader) into
   the same flat white card style. Kills the 2010 'admin
   template' blue/red/teal/orange/green saturated bars. */
.box,
.box.box-solid,
.box.box-primary,
.box.box-info,
.box.box-success,
.box.box-warning,
.box.box-danger {
  background: var(--np-surface) !important;
  border: 1px solid var(--np-line) !important;
  border-top: 1px solid var(--np-line) !important;
  border-radius: var(--np-radius) !important;
  box-shadow: var(--np-shadow-sm) !important;
  margin-bottom: 18px;
  overflow: hidden;
}
.box-header,
.box.box-solid > .box-header,
.box.box-primary > .box-header,
.box.box-info > .box-header,
.box.box-success > .box-header,
.box.box-warning > .box-header,
.box.box-danger > .box-header {
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  border-bottom: 1px solid var(--np-line-2) !important;
  border-top: none !important;
  padding: 14px 20px !important;
}
.box-header > .box-title,
.box.box-solid > .box-header > .box-title {
  font-weight: 600 !important;
  font-size: 14.5px !important;
  letter-spacing: -0.01em !important;
  color: var(--np-text) !important;
}
/* Discreet 3px left accent in place of the saturated bar */
.box.box-primary { border-left: 3px solid #2563EB !important; }
.box.box-info    { border-left: 3px solid #0EA5E9 !important; }
.box.box-success { border-left: 3px solid #16A34A !important; }
.box.box-warning { border-left: 3px solid #D97706 !important; }
.box.box-danger  { border-left: 3px solid #DC2626 !important; }
.box-body { padding: 18px 22px !important; color: var(--np-text); background: var(--np-surface) !important; }
/* Box icons inside box-tools (collapse, remove) */
.box-tools .btn-box-tool { color: var(--np-muted) !important; }
.box-tools .btn-box-tool:hover { color: var(--np-text) !important; background: var(--np-surface-2) !important; }

/* ── Buttons ────────────────────────────────────────────── */
.btn, .btn-default, .shiny-download-link,
.skin-blue .sidebar .btn {
  border-radius: var(--np-radius-sm) !important;
  border: 1px solid var(--np-line) !important;
  padding: 8px 16px !important;
  font-weight: 500 !important;
  font-size: 13px !important;
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  box-shadow: var(--np-shadow-xs) !important;
  transition: all 0.12s ease;
}
.btn:hover, .btn-default:hover, .shiny-download-link:hover {
  background: var(--np-surface-2) !important;
  border-color: #D5D5D9 !important;
  transform: translateY(-1px);
  box-shadow: var(--np-shadow-sm) !important;
}
.btn-primary {
  background: var(--np-accent) !important;
  color: #FFFFFF !important;
  border: none !important;
}
.btn-primary:hover {
  background: var(--np-accent-2) !important;
  color: #FFFFFF !important;
}

/* Sidebar download buttons */
.skin-blue .sidebar .shiny-download-link {
  display: inline-block;
  background: var(--np-surface-2) !important;
  color: var(--np-text) !important;
  border: 1px solid var(--np-line) !important;
}
.skin-blue .sidebar .shiny-download-link:hover {
  background: var(--np-accent-soft) !important;
  color: var(--np-accent) !important;
  border-color: var(--np-accent) !important;
}

/* ── Body inputs ────────────────────────────────────────── */
.content-wrapper .form-control,
.content-wrapper .selectize-input {
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  border: 1px solid var(--np-line) !important;
  border-radius: var(--np-radius-sm) !important;
  box-shadow: none !important;
  padding: 9px 12px !important;
}
.content-wrapper .form-control:focus,
.content-wrapper .selectize-input.focus {
  border-color: var(--np-accent) !important;
  box-shadow: 0 0 0 3px rgba(37,99,235,0.15) !important;
}
.content-wrapper .control-label,
.content-wrapper .shiny-input-container > label {
  font-size: 12px !important;
  font-weight: 600 !important;
  color: var(--np-muted) !important;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}

/* ── DataTables ─────────────────────────────────────────── */
.dataTables_wrapper { padding: 6px 4px 0 4px; color: var(--np-text); }
table.dataTable {
  border-collapse: separate !important;
  border-spacing: 0 !important;
  background: var(--np-surface);
  border-radius: var(--np-radius-sm);
}
table.dataTable thead th {
  font-weight: 600 !important;
  font-size: 12.5px !important;
  background: var(--np-surface-2) !important;
  border-bottom: 1px solid var(--np-line) !important;
  border-top: none !important;
  padding: 12px 14px !important;
  color: var(--np-text) !important;
  text-transform: none;
  letter-spacing: 0;
}
table.dataTable tbody td {
  border-top: 1px solid var(--np-line-2) !important;
  padding: 11px 14px !important;
  font-size: 13px !important;
  color: var(--np-text) !important;
}
table.dataTable tbody tr:hover { background: var(--np-surface-2) !important; }

.dataTables_wrapper .dataTables_length select,
.dataTables_wrapper .dataTables_filter input {
  border-radius: var(--np-radius-sm) !important;
  border: 1px solid var(--np-line) !important;
  padding: 6px 10px !important;
  font-size: 13px !important;
}

/* ── Plots ──────────────────────────────────────────────── */
.shiny-plot-output, .plotly {
  border-radius: var(--np-radius-sm);
  background: var(--np-surface);
  overflow: hidden;
}

/* ── Value boxes (QC PASS/FAIL/Ambiguous/Inactive tiles) ── */
.small-box {
  border-radius: var(--np-radius) !important;
  box-shadow: var(--np-shadow-sm) !important;
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
  border: 1px solid var(--np-line);
  overflow: hidden;
  position: relative;
  padding: 6px;
}
.small-box::before {
  content: '';
  position: absolute;
  left: 0; top: 14px; bottom: 14px; width: 4px;
  border-radius: 0 3px 3px 0;
  background: var(--np-line);
}
.small-box.bg-green::before  { background: #16A34A; }
.small-box.bg-red::before    { background: #DC2626; }
.small-box.bg-yellow::before { background: #D97706; }
.small-box.bg-black::before  { background: #6E6E73; }
.small-box.bg-aqua::before,
.small-box.bg-blue::before   { background: #2563EB; }
.small-box.bg-green,  .small-box.bg-red,
.small-box.bg-yellow, .small-box.bg-black,
.small-box.bg-aqua,   .small-box.bg-blue {
  background: var(--np-surface) !important;
  color: var(--np-text) !important;
}
.small-box > .inner {
  padding: 16px 18px 16px 22px;
}
.small-box > .inner > h3 {
  font-size: 32px !important;
  font-weight: 700 !important;
  margin: 0 0 4px 0 !important;
  color: var(--np-text) !important;
  letter-spacing: -0.02em;
}
.small-box > .inner > p {
  font-size: 12.5px !important;
  font-weight: 600 !important;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--np-muted) !important;
  margin: 0 !important;
}
.small-box > .icon {
  color: var(--np-line) !important;
  font-size: 56px !important;
  top: 14px !important;
  right: 18px !important;
  opacity: 0.6;
}
.small-box.bg-green  > .icon { color: rgba(22,163,74,0.18) !important; }
.small-box.bg-red    > .icon { color: rgba(220,38,38,0.18) !important; }
.small-box.bg-yellow > .icon { color: rgba(217,119,6,0.18) !important; }
.small-box.bg-black  > .icon { color: rgba(110,110,115,0.20) !important; }
.small-box .small-box-footer { display: none !important; }

/* ── Notifications & errors ─────────────────────────────── */
.shiny-notification {
  border-radius: var(--np-radius-sm) !important;
  box-shadow: var(--np-shadow-lg) !important;
  font-weight: 500;
  font-size: 13px;
}
.shiny-output-error { color: var(--np-danger) !important; padding: 12px; font-size: 13px; }

/* ── Workflow stepper ───────────────────────────────────── */
.np-stepper {
  display: flex; align-items: center; justify-content: center;
  gap: 4px;
  padding: 12px 22px;
  margin: 0 0 18px 0;
  background: var(--np-surface);
  border-radius: var(--np-radius);
  border: 1px solid var(--np-line);
  box-shadow: var(--np-shadow-xs);
}
.np-stepper .np-stage {
  display: flex; align-items: center;
  gap: 8px;
  padding: 4px 12px;
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
  background: var(--np-surface-2);
  color: var(--np-muted);
  font-weight: 700; font-size: 11px;
  border: 1px solid var(--np-line);
}
.np-stepper .np-stage.active .dot {
  background: var(--np-accent);
  color: #FFFFFF;
  border-color: var(--np-accent);
}
.np-stepper .np-arrow { color: var(--np-line); font-size: 14px; }

/* ── Status text ────────────────────────────────────────── */
#plate_map_status p { padding-left: 0 !important; font-size: 12px !important; }

/* ── Help / Methods page styling ───────────────────────── */
.np-help h3 {
  margin-top: 26px; margin-bottom: 10px;
  font-size: 16px; font-weight: 700;
  letter-spacing: -0.01em; color: var(--np-text);
}
.np-help p, .np-help li {
  font-size: 13.5px; line-height: 1.6;
  color: var(--np-text-2);
}
.np-help code {
  background: var(--np-surface-2);
  padding: 2px 6px; border-radius: 4px;
  font-size: 12.5px;
  border: 1px solid var(--np-line);
  font-family: 'JetBrains Mono', 'SF Mono', Consolas, monospace;
}
.np-help blockquote {
  border-left: 3px solid var(--np-accent);
  padding: 10px 16px; margin: 14px 0;
  background: var(--np-surface-2);
  border-radius: 0 8px 8px 0;
  color: var(--np-text-2); font-style: italic;
  font-size: 13px;
}

/* ── Empty state ───────────────────────────────────────── */
.np-empty {
  padding: 48px 24px;
  text-align: center;
  color: var(--np-muted);
  font-size: 13.5px;
}
.np-empty__icon {
  font-size: 28px;
  color: var(--np-line);
  margin-bottom: 12px;
}

/* ── Verbatim text output (config.dilution display) ────── */
pre.shiny-text-output {
  background: var(--np-surface-2) !important;
  border: 1px solid var(--np-line) !important;
  border-radius: var(--np-radius-sm) !important;
  font-size: 12.5px !important;
  color: var(--np-text) !important;
  padding: 10px 12px !important;
  font-family: 'JetBrains Mono','SF Mono',Consolas,monospace !important;
}

/* ── Scrollbar ──────────────────────────────────────────── */
::-webkit-scrollbar { width: 10px; height: 10px; }
::-webkit-scrollbar-thumb {
  background: rgba(0,0,0,0.18);
  border-radius: 5px;
}
::-webkit-scrollbar-thumb:hover { background: rgba(0,0,0,0.28); }
::-webkit-scrollbar-track { background: transparent; }

/* ── Misc shinydashboard residue cleanup ───────────────── */
.skin-blue .main-header .navbar .nav > li > a:hover,
.skin-blue .main-header .navbar .nav > li > a:focus {
  background-color: rgba(0,0,0,0.04) !important;
  color: var(--np-text) !important;
}
.tab-content > .tab-pane { padding-top: 0; }
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
  shiny::tags$div(
    class = "np-help",
    shiny::div(
      class = "col-sm-12",
      shiny::tags$section(
        class = "np-card",
        shiny::tags$div(
          class = "np-card__header",
          shiny::tags$span(class = "np-card__accent"),
          shiny::tags$span(class = "np-card__icon", shiny::icon("book")),
          shiny::tags$div(
            class = "np-card__titles",
            shiny::tags$h3(class = "np-card__title", "Help, Methods, and Statistical Basis"),
            shiny::tags$p(class = "np-card__subtitle",
                          "Workflow walkthrough and academic references")
          )
        ),
        shiny::tags$div(class = "np-card__body",
          tags$h3("Workflow"),
          tags$ol(
            tags$li(tags$b("Upload"), " a master FFU Excel file. Sheet names follow ",
                    tags$code("<SEROTYPE>_<PLATE>"), " e.g. ",
                    tags$code("DENV2_P1"), ", ",
                    tags$code("DENV4 Furin Plate 1"), ". An optional ",
                    tags$code("plate_map"), " sheet (or separate CSV) defines wells."),
            tags$li(tags$b("Configure"),
                    " starting concentration, fold dilution, step count, ",
                    "mixing volumes, and QC thresholds. Sliders are live."),
            tags$li(tags$b("Run Analysis"),
                    " (sticky button). Triggers parsing, %neutralization, ",
                    "4PL fitting, QC, and chart generation."),
            tags$li(tags$b("Review"), " each tab. The IC50 Charts tab includes ",
                    "Layout 1 (IC50 ng/mL), Layout 2 (1/IC50 potency), and the ",
                    "publication-style ", tags$em("IC50 Summary"), " bar chart."),
            tags$li(tags$b("Export"), " bold-Calibri ", tags$code(".xlsx"),
                    " spreadsheets and 300 DPI ", tags$code(".png"), " plots.")
          ),

          tags$h3("Plate layout"),
          tags$p("96-well plates support three orientations per sample, ",
                 "set in the ", tags$code("half"),
                 " column of the plate map:"),
          tags$ul(
            tags$li(tags$code("left"), "  -- columns 1 to 6 (descending or ascending series)."),
            tags$li(tags$code("right"), " -- columns 7 to 12."),
            tags$li(tags$code("full"), "  -- columns 1 through ", tags$code("n_steps"),
                    " stretched horizontally; supports up to 12 dilutions per row.")
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
          tags$p("Following the FDA Bioanalytical Method Validation guidance ",
                 "(2018) and ICH M10 (2022), IC50 values are reported using ",
                 "left/right-censoring conventions rather than substituting ",
                 "arbitrary numeric values:"),
          tags$ul(
            tags$li(tags$b("Quantified"),
                    ": IC50 lies inside the tested concentration range ",
                    "(LLOQ <= IC50 <= ULOQ). Reported as a numeric value."),
            tags$li(tags$b("< LLOQ"),
                    ": IC50 estimated below the lowest tested concentration. ",
                    "Reported as ", tags$code("\"< LLOQ\""), " (left-censored)."),
            tags$li(tags$b("> ULOQ"),
                    ": IC50 estimated above the highest tested concentration. ",
                    "Reported as ", tags$code("\"> ULOQ\""), " (right-censored)."),
            tags$li(tags$b("> Cap"),
                    ": IC50 above the user-set cap (default 20,000 ng/mL = ",
                    "2x highest tested). Reported as ",
                    tags$code("\"> 20000\""), "."),
            tags$li(tags$b("Solver pathology"),
                    ": IC50 below 1e-10 ng/mL (sub-attomolar). ",
                    "These are ALWAYS algorithmic failures and are reported as ",
                    tags$code("\"ND\""),
                    " (not determined). They are NOT treated as real ",
                    "measurements in any summary statistic."),
            tags$li(tags$b("Inactive"),
                    ": no neutralization detected. Reported as ",
                    tags$code("\"Inactive\""), ".")
          ),
          tags$blockquote(
            "Conflating solver failures with real below-detection results ",
            "yields biased downstream summary statistics and is rejected by ",
            "high-impact journals. NeutPipeline keeps the underlying numeric ",
            "value (capped) for plotting purposes only; every analyst-facing ",
            "table and export uses the censored string."
          ),

          tags$h3("Quality control"),
          tags$p("UNC neutralization QC criterion (per protocol document):"),
          tags$ul(
            tags$li("|Hill slope| > ", tags$code("min_hillslope"),
                    " (default 0.5). Below threshold -> ", tags$b("FAIL"), "."),
            tags$li("R\u00b2 >= ", tags$code("min_r2_mab"), " (0.85 mAb) or ",
                    tags$code("min_r2_poly"),
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
                    tags$em("Dose-Response Analysis Using R"),
                    ". PLOS ONE 10(12): e0146021 (2015)."),
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
      shinydashboard::menuItem("Raw Data and Plate Map", tabName = "tab_raw",    icon = icon("table")),
      shinydashboard::menuItem("Plate Heatmap",          tabName = "tab_heat",   icon = icon("th")),
      shinydashboard::menuItem("Dose Response Curves",   tabName = "tab_curves", icon = icon("chart-line")),
      shinydashboard::menuItem("IC50 Results Table",     tabName = "tab_ic50",   icon = icon("list-alt")),
      shinydashboard::menuItem("IC50 Charts",            tabName = "tab_charts", icon = icon("bar-chart")),
      shinydashboard::menuItem("QC Report",              tabName = "tab_qc",     icon = icon("check-circle")),
      shinydashboard::menuItem("Help & Methods",         tabName = "tab_help",   icon = icon("book"))
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
                   style = "width:calc(100% - 36px); margin: 0 18px 8px 18px;
                            font-size:12px;"),
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

    .np_step(5, "Curve Fitting"),
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

    .np_step(6, "QC Thresholds"),
    .np_tip(numericInput("min_hill", "Minimum |Hill slope|",
                         value = cfg$qc$min_hillslope, min = 0, step = 0.05),
            "UNC criterion: |Hill| < this value -> FAIL."),
    .np_tip(numericInput("min_r2_mab", "Minimum R\u00b2 (mAb)",
                         value = cfg$qc$min_r2_mab, min = 0, max = 1, step = 0.01),
            "UNC monoclonal antibody R-squared threshold."),
    .np_tip(numericInput("min_r2_poly", "Minimum R\u00b2 (polyclonal)",
                         value = cfg$qc$min_r2_poly, min = 0, max = 1, step = 0.01),
            "UNC polyclonal serum R-squared threshold."),
    .np_tip(numericInput("ic50_cap", "IC50 cap (ng/mL)",
                         value = cfg$qc$ic50_cap, min = 0),
            "Values above this are reported as '> cap' rather than as numeric measurements (FDA / ICH M10)."),
    numericInput("max_vc_cv",    "Max VC coefficient of variation (%)",
                 value = cfg$qc$max_vc_cv_pct, min = 0),
    numericInput("min_max_neut", "Min max %neut required to fit curve",
                 value = cfg$qc$min_max_neut_pct, min = 0),

    .np_step(7, "Virus Control"),
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
