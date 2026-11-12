# =============================================================================
# Chart24MS - GitHub Actions Plot Generator & GitHub Pages Builder
# Executes generate_plots() using the HDB HTTP API only (no ODBC), saves the
# PNGs and PDFs in output/<YYYY-MM>/, and updates the docs/ GitHub Pages site.
# =============================================================================

options(stringsAsFactors = FALSE)

source("generate_plots.R")

get_env_val <- function(name, default = "") {
  val <- Sys.getenv(name, unset = "")
  if (!nzchar(trimws(val))) default else trimws(val)
}

get_env_bool <- function(name, default = TRUE) {
  val <- Sys.getenv(name, unset = "")
  if (!nzchar(trimws(val))) return(default)
  toupper(trimws(val)) %in% c("TRUE", "1", "YES", "Y", "ON")
}

get_env_num <- function(name, default) {
  val <- Sys.getenv(name, unset = "")
  if (!nzchar(trimws(val))) return(default)
  num <- suppressWarnings(as.numeric(val))
  if (is.na(num)) default else num
}

# 1. Resolve Study Month (defaults to current month if blank)
study_month <- get_env_val("INPUT_STUDY_MONTH", "")
if (!nzchar(study_month)) {
  study_month <- format(Sys.Date(), "%Y-%m")
}
study_month <- validate_chart24ms_month(study_month, "Study month")

# 2. Resolve Plot End Month (optional)
end_month <- get_env_val("INPUT_END_MONTH", "")
if (nzchar(end_month)) {
  end_month <- validate_chart24ms_month(end_month, "Plot end month")
}

# 3. Resolve Scenarios and MRIDs
most_6_enabled <- get_env_bool("INPUT_MOST_6_ENABLED", TRUE)
most_7_enabled <- get_env_bool("INPUT_MOST_7_ENABLED", TRUE)

min_6_mrid <- get_env_num("INPUT_MIN_6_MRID", 3332)
most_6_mrid <- get_env_num("INPUT_MOST_6_MRID", 3331)
most_7_mrid <- get_env_num("INPUT_MOST_7_MRID", 3333)
max_7_mrid <- get_env_num("INPUT_MAX_7_MRID", 3328)

# Run months: Min-6 and Most Probable default to study month;
# Max-7 defaults to the most recent quarterly month (Jan, Apr, Aug, Oct).
min_6_run_date <- get_env_val("INPUT_MIN_6_RUN_DATE", study_month)
most_6_run_date <- get_env_val("INPUT_MOST_6_RUN_DATE", study_month)
most_7_run_date <- get_env_val("INPUT_MOST_7_RUN_DATE", study_month)

default_max7 <- chart24ms_latest_max7_run_month(study_month)
max_7_run_date <- get_env_val("INPUT_MAX_7_RUN_DATE", default_max7)

# 4. Y-axis elevation limits
powell_y_min <- get_env_num("INPUT_POWELL_Y_MIN", 3400)
powell_y_max <- get_env_num("INPUT_POWELL_Y_MAX", 3625)
mead_y_min   <- get_env_num("INPUT_MEAD_Y_MIN", 950)
mead_y_max   <- get_env_num("INPUT_MEAD_Y_MAX", 1100)

message("=== Starting Chart24MS GitHub Actions Run ===")
message("Study Month:        ", study_month)
message("End Month:          ", if (nzchar(end_month)) end_month else "(full 24 months)")
message("Most 6.0 maf:       ", most_6_enabled, " (MRID ", most_6_mrid, ", run ", most_6_run_date, ")")
message("Most 7.0 maf:       ", most_7_enabled, " (MRID ", most_7_mrid, ", run ", most_7_run_date, ")")
message("Probable Min 6.0:   MRID ", min_6_mrid, ", run ", min_6_run_date)
message("Probable Max 7.0:   MRID ", max_7_mrid, ", run ", max_7_run_date)
message("Powell Y Range:     ", powell_y_min, " - ", powell_y_max, " ft")
message("Mead Y Range:       ", mead_y_min, " - ", mead_y_max, " ft")
message("Data Source:        Reclamation HDB HTTP API only")

# 5. Generate plots using API only (no ODBC)
res <- generate_plots(
  run_date          = study_month,
  min_6_mrid        = min_6_mrid,
  most_6_mrid       = most_6_mrid,
  most_7_mrid       = most_7_mrid,
  max_7_mrid        = max_7_mrid,
  min_6_run_date    = min_6_run_date,
  most_6_run_date   = most_6_run_date,
  most_7_run_date   = most_7_run_date,
  max_7_run_date    = max_7_run_date,
  most_6_enabled    = most_6_enabled,
  most_7_enabled    = most_7_enabled,
  end_month         = if (nzchar(end_month)) end_month else NULL,
  powell_y_min      = powell_y_min,
  powell_y_max      = powell_y_max,
  mead_y_min        = mead_y_min,
  mead_y_max        = mead_y_max,
  data_source       = "api",
  project_dir       = getwd(),
  progress_callback = function(msg, val) {
    pct <- if (is.null(val)) "" else sprintf("[%3.0f%%] ", 100 * val)
    message(pct, msg)
  }
)

message("Plots generated successfully in: ", res$output_dir)

# 6. Prepare docs/ folder for GitHub Pages
docs_dir <- file.path(getwd(), "docs")
archive_dir <- file.path(docs_dir, "archive", study_month)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

# Copy current plots into docs/ (latest) and docs/archive/<study_month>/
file.copy(res$files$powell_png, file.path(docs_dir, "Powell24MS.png"), overwrite = TRUE)
file.copy(res$files$powell_pdf, file.path(docs_dir, "Powell24MS.pdf"), overwrite = TRUE)
file.copy(res$files$mead_png,   file.path(docs_dir, "Mead24MS.png"),   overwrite = TRUE)
file.copy(res$files$mead_pdf,   file.path(docs_dir, "Mead24MS.pdf"),   overwrite = TRUE)

file.copy(res$files$powell_png, file.path(archive_dir, "Powell24MS.png"), overwrite = TRUE)
file.copy(res$files$powell_pdf, file.path(archive_dir, "Powell24MS.pdf"), overwrite = TRUE)
file.copy(res$files$mead_png,   file.path(archive_dir, "Mead24MS.png"),   overwrite = TRUE)
file.copy(res$files$mead_pdf,   file.path(archive_dir, "Mead24MS.pdf"),   overwrite = TRUE)

# Discover any past archived study months to list in the page
archived_months <- character(0)
if (dir.exists(file.path(docs_dir, "archive"))) {
  archived_months <- sort(list.dirs(file.path(docs_dir, "archive"), full.names = FALSE, recursive = FALSE), decreasing = TRUE)
}

# 7. Generate GitHub Pages HTML
now_utc <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")

archive_options_html <- ""
if (length(archived_months) > 0) {
  opts <- vapply(archived_months, function(m) {
    selected <- if (identical(m, study_month)) " selected" else ""
    paste0('<option value="archive/', m, '/Powell24MS.png"', selected, '>', m, '</option>')
  }, character(1))
  archive_options_html <- paste0(
    '<div class="archive-bar"><label for="archive-select"><strong>Archived Runs:</strong></label> ',
    '<select id="archive-select" onchange="switchArchive(this.value)">',
    paste(opts, collapse = ""),
    '</select></div>'
  )
}

gh_repo <- Sys.getenv("GITHUB_REPOSITORY", "")
actions_link <- if (nzchar(gh_repo)) {
  paste0('&bull; To run a new study or update inputs, visit the <a href="https://github.com/', gh_repo, '/actions" target="_blank">Actions</a> tab in GitHub.')
} else {
  ""
}

html_template <- '<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Lake Powell &amp; Lake Mead 24-Month Study</title>
  <style>
    :root {
      --usbr-blue: #0e3964;
      --usbr-dark: #07223d;
      --usbr-gold: #ffdc70;
      --usbr-light: #f4f7fa;
      --card-bg: #ffffff;
      --text-color: #222222;
      --muted-color: #555555;
      --border-color: #dce3eb;
    }
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      background-color: var(--usbr-light);
      color: var(--text-color);
      line-height: 1.5;
      padding-bottom: 50px;
    }
    header {
      background: linear-gradient(135deg, var(--usbr-dark) 0%, var(--usbr-blue) 100%);
      color: #ffffff;
      padding: 36px 20px;
      text-align: center;
      border-bottom: 4px solid var(--usbr-gold);
      box-shadow: 0 4px 12px rgba(0,0,0,0.12);
    }
    header h1 {
      font-size: 2.1rem;
      font-weight: 700;
      letter-spacing: -0.5px;
      margin-bottom: 8px;
    }
    header .subtitle {
      font-size: 1.15rem;
      color: #e2ecf7;
      max-width: 850px;
      margin: 0 auto 14px auto;
    }
    .badge-bar {
      display: flex;
      flex-wrap: wrap;
      justify-content: center;
      gap: 12px;
      margin-top: 12px;
      font-size: 0.85rem;
    }
    .badge {
      background: rgba(255,255,255,0.15);
      border: 1px solid rgba(255,255,255,0.25);
      border-radius: 20px;
      padding: 5px 14px;
    }
    .container {
      max-width: 1200px;
      margin: 32px auto 0 auto;
      padding: 0 16px;
    }
    .archive-bar {
      margin-bottom: 24px;
      background: #ffffff;
      padding: 12px 20px;
      border-radius: 8px;
      border: 1px solid var(--border-color);
      display: flex;
      align-items: center;
      gap: 12px;
      font-size: 0.95rem;
    }
    .archive-bar select {
      padding: 6px 12px;
      border-radius: 4px;
      border: 1px solid #ccd5df;
      font-size: 0.9rem;
    }
    .plot-card {
      background: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 10px;
      margin-bottom: 32px;
      overflow: hidden;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
    }
    .card-header {
      padding: 18px 24px;
      border-bottom: 1px solid var(--border-color);
      background: #fafcff;
      display: flex;
      flex-wrap: wrap;
      justify-content: space-between;
      align-items: center;
      gap: 12px;
    }
    .card-header h2 {
      font-size: 1.4rem;
      color: var(--usbr-blue);
      font-weight: 700;
    }
    .card-header .btn-group {
      display: flex;
      gap: 8px;
    }
    .btn {
      display: inline-flex;
      align-items: center;
      padding: 8px 16px;
      font-size: 0.88rem;
      font-weight: 600;
      text-decoration: none;
      border-radius: 6px;
      transition: all 0.15s ease-in-out;
    }
    .btn-primary {
      background-color: var(--usbr-blue);
      color: #ffffff;
      border: 1px solid var(--usbr-blue);
    }
    .btn-primary:hover { background-color: var(--usbr-dark); }
    .btn-outline {
      background-color: transparent;
      color: var(--usbr-blue);
      border: 1px solid var(--usbr-blue);
    }
    .btn-outline:hover {
      background-color: var(--usbr-blue);
      color: #ffffff;
    }
    .image-wrapper {
      padding: 20px;
      background: #ffffff;
      text-align: center;
    }
    .image-wrapper img {
      max-width: 100%;
      height: auto;
      border-radius: 4px;
      box-shadow: 0 1px 6px rgba(0,0,0,0.1);
      display: block;
      margin: 0 auto;
    }
    .meta-card {
      background: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 10px;
      padding: 24px;
      margin-bottom: 32px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.06);
    }
    .meta-card h3 {
      font-size: 1.2rem;
      color: var(--usbr-blue);
      margin-bottom: 16px;
      border-bottom: 2px solid var(--border-color);
      padding-bottom: 8px;
    }
    table.meta-table {
      width: 100%;
      border-collapse: collapse;
      font-size: 0.92rem;
    }
    table.meta-table th, table.meta-table td {
      padding: 10px 14px;
      text-align: left;
      border-bottom: 1px solid #edf1f5;
    }
    table.meta-table th {
      background: #f7fafc;
      color: var(--muted-color);
      font-weight: 600;
      width: 30%;
    }
    footer {
      text-align: center;
      margin-top: 40px;
      font-size: 0.88rem;
      color: var(--muted-color);
    }
    footer a { color: var(--usbr-blue); text-decoration: none; font-weight: 600; }
    footer a:hover { text-decoration: underline; }
  </style>
  <script>
    function switchArchive(val) {
      if (!val) return;
      var dir = val.substring(0, val.lastIndexOf("/"));
      document.getElementById("powell-img").src = dir + "/Powell24MS.png";
      document.getElementById("powell-link").href = dir + "/Powell24MS.png";
      document.getElementById("powell-dl-png").href = dir + "/Powell24MS.png";
      document.getElementById("powell-dl-pdf").href = dir + "/Powell24MS.pdf";

      document.getElementById("mead-img").src = dir + "/Mead24MS.png";
      document.getElementById("mead-link").href = dir + "/Mead24MS.png";
      document.getElementById("mead-dl-png").href = dir + "/Mead24MS.png";
      document.getElementById("mead-dl-pdf").href = dir + "/Mead24MS.pdf";
    }
  </script>
</head>
<body>
  <header>
    <h1>Lake Powell &amp; Lake Mead 24-Month Study</h1>
    <div class="subtitle">Projections from {{SUBTITLE_MONTHS}} 24-Month Study Inflow Scenarios</div>
    <div class="badge-bar">
      <span class="badge">Study Month: <strong>{{STUDY_MONTH}}</strong></span>
      <span class="badge">Plot Window: <strong>{{STUDY_MONTH}} to {{END_MONTH}}</strong></span>
      <span class="badge">Generated: <strong>{{GEN_DATE_UTC}}</strong></span>
      <span class="badge">Data Source: <strong>Reclamation HDB API</strong></span>
    </div>
  </header>

  <div class="container">
    {{ARCHIVE_BAR}}

    <!-- Lake Powell Card -->
    <section class="plot-card">
      <div class="card-header">
        <h2>Lake Powell End-of-Month Elevations</h2>
        <div class="btn-group">
          <a id="powell-dl-png" class="btn btn-primary" href="Powell24MS.png" download="Powell24MS_{{STUDY_MONTH}}.png">Download PNG</a>
          <a id="powell-dl-pdf" class="btn btn-outline" href="Powell24MS.pdf" download="Powell24MS_{{STUDY_MONTH}}.pdf">Download PDF</a>
        </div>
      </div>
      <div class="image-wrapper">
        <a id="powell-link" href="Powell24MS.png" target="_blank" title="Click to view full resolution image">
          <img id="powell-img" src="Powell24MS.png" alt="Lake Powell 24-Month Study Elevation Plot" loading="eager">
        </a>
      </div>
    </section>

    <!-- Lake Mead Card -->
    <section class="plot-card">
      <div class="card-header">
        <h2>Lake Mead End-of-Month Elevations</h2>
        <div class="btn-group">
          <a id="mead-dl-png" class="btn btn-primary" href="Mead24MS.png" download="Mead24MS_{{STUDY_MONTH}}.png">Download PNG</a>
          <a id="mead-dl-pdf" class="btn btn-outline" href="Mead24MS.pdf" download="Mead24MS_{{STUDY_MONTH}}.pdf">Download PDF</a>
        </div>
      </div>
      <div class="image-wrapper">
        <a id="mead-link" href="Mead24MS.png" target="_blank" title="Click to view full resolution image">
          <img id="mead-img" src="Mead24MS.png" alt="Lake Mead 24-Month Study Elevation Plot" loading="eager">
        </a>
      </div>
    </section>

    <!-- Run Parameters -->
    <section class="meta-card">
      <h3>Study Run Parameters</h3>
      <table class="meta-table">
        <tr>
          <th>Study Month</th>
          <td>{{STUDY_MONTH}}</td>
        </tr>
        <tr>
          <th>Plot Range</th>
          <td>Through {{END_MONTH}}</td>
        </tr>
        <tr>
          <th>Probable Minimum (6.0 maf)</th>
          <td>MRID {{MIN_6_MRID}} (Run month: {{MIN_6_RUN}})</td>
        </tr>
        <tr>
          <th>Most Probable (6.0 maf)</th>
          <td>{{MOST_6_STATUS}}</td>
        </tr>
        <tr>
          <th>Most Probable (7.0 maf)</th>
          <td>{{MOST_7_STATUS}}</td>
        </tr>
        <tr>
          <th>Probable Maximum (7.0 maf)</th>
          <td>MRID {{MAX_7_MRID}} (Run month: {{MAX_7_RUN}})</td>
        </tr>
        <tr>
          <th>Lake Powell Y-Axis</th>
          <td>{{POWELL_Y_MIN}} to {{POWELL_Y_MAX}} ft</td>
        </tr>
        <tr>
          <th>Lake Mead Y-Axis</th>
          <td>{{MEAD_Y_MIN}} to {{MEAD_Y_MAX}} ft</td>
        </tr>
      </table>
    </section>

    <footer>
      U.S. Bureau of Reclamation &bull; 24-Month Study Plot Generator
      {{ACTIONS_LINK}}
    </footer>
  </div>
</body>
</html>'

subtitle_text <- if (!is.null(res$month_heading) && nzchar(res$month_heading)) res$month_heading else study_month
end_month_text <- if (!is.null(res$end_month) && nzchar(res$end_month)) res$end_month else end_month

most_6_status <- if (most_6_enabled) {
  paste0("MRID ", most_6_mrid, " (Run month: ", most_6_run_date, ")")
} else {
  "Disabled (Not included)"
}

most_7_status <- if (most_7_enabled) {
  paste0("MRID ", most_7_mrid, " (Run month: ", most_7_run_date, ")")
} else {
  "Disabled (Not included)"
}

replacements <- list(
  "{{SUBTITLE_MONTHS}}" = subtitle_text,
  "{{STUDY_MONTH}}"     = study_month,
  "{{END_MONTH}}"       = end_month_text,
  "{{GEN_DATE_UTC}}"    = now_utc,
  "{{ARCHIVE_BAR}}"     = archive_options_html,
  "{{MIN_6_MRID}}"      = as.character(min_6_mrid),
  "{{MIN_6_RUN}}"       = as.character(min_6_run_date),
  "{{MOST_6_STATUS}}"   = most_6_status,
  "{{MOST_7_STATUS}}"   = most_7_status,
  "{{MAX_7_MRID}}"      = as.character(max_7_mrid),
  "{{MAX_7_RUN}}"       = as.character(max_7_run_date),
  "{{POWELL_Y_MIN}}"    = as.character(as.integer(powell_y_min)),
  "{{POWELL_Y_MAX}}"    = as.character(as.integer(powell_y_max)),
  "{{MEAD_Y_MIN}}"      = as.character(as.integer(mead_y_min)),
  "{{MEAD_Y_MAX}}"      = as.character(as.integer(mead_y_max)),
  "{{ACTIONS_LINK}}"    = actions_link
)

html_content <- html_template
for (tag in names(replacements)) {
  html_content <- gsub(tag, replacements[[tag]], html_content, fixed = TRUE)
}

writeLines(html_content, file.path(docs_dir, "index.html"))
message("Updated docs/index.html successfully.")
message("=== Chart24MS GitHub Actions Run Complete ===")
