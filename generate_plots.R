# Chart24MS plot-generation engine
# Refactored from scripts/01-Powell-Mead-24MS-plots_P26_API_v2.R

.chart24ms_required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "lubridate", "rhdb", "zoo", "magick", "scales"
)

load_chart24ms_packages <- function() {
  missing <- .chart24ms_required_packages[
    !vapply(.chart24ms_required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]
  if (length(missing) > 0) {
    stop(
      "Missing required R package(s): ", paste(missing, collapse = ", "),
      ". Run Setup_Chart24MS.bat first.",
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(lubridate)
    library(rhdb)
    library(zoo)
    library(magick)
  })
}

validate_chart24ms_month <- function(x, field_name) {
  x <- trimws(as.character(x)[1])
  if (is.na(x) || !grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", x)) {
    stop(field_name, " must use YYYY-MM format (for example, 2026-09).", call. = FALSE)
  }
  x
}

validate_chart24ms_mrid <- function(x, field_name) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1 || is.na(value) || value <= 0 || value != floor(value)) {
    stop(field_name, " must be a positive whole-number MRID.", call. = FALSE)
  }
  as.integer(value)
}

# Add n months to a "YYYY-MM" string using base R only (no package needed).
chart24ms_add_months <- function(x, n) {
  idx <- as.integer(substr(x, 1, 4)) * 12L + (as.integer(substr(x, 6, 7)) - 1L) + as.integer(n)
  sprintf("%04d-%02d", idx %/% 12L, idx %% 12L + 1L)
}

# Resolve the last month shown on the plot. Blank/NULL means the full 24-month
# window (study month + 23). A custom month must fall within that window.
resolve_chart24ms_end_month <- function(end_month, run_date) {
  default_end <- chart24ms_add_months(run_date, 23)
  if (is.null(end_month) || length(end_month) == 0 ||
      is.na(end_month[1]) || !nzchar(trimws(as.character(end_month[1])))) {
    return(default_end)
  }
  end_month <- validate_chart24ms_month(end_month, "Plot end month")
  if (end_month < run_date) {
    stop("Plot end month (", end_month, ") cannot be earlier than the study month (", run_date, ").", call. = FALSE)
  }
  if (end_month > default_end) {
    stop("Plot end month (", end_month, ") cannot be later than ", default_end,
         ", the end of the 24-month study window.", call. = FALSE)
  }
  end_month
}

# Validate and resolve y-axis elevation limits (min, max) for a reservoir.
# If y_min / y_max are NULL or NA, fall back to the defaults.
resolve_chart24ms_ylim <- function(y_min, y_max, default_min, default_max, reservoir_label) {
  parse_one <- function(v, d, side_label) {
    if (is.null(v) || length(v) == 0 || is.na(v[1]) || !nzchar(trimws(as.character(v[1])))) {
      return(as.numeric(d))
    }
    num <- suppressWarnings(as.numeric(v[1]))
    if (is.na(num) || !is.finite(num)) {
      stop(reservoir_label, " ", side_label, " elevation must be a number.", call. = FALSE)
    }
    num
  }
  lo <- parse_one(y_min, default_min, "minimum y-axis")
  hi <- parse_one(y_max, default_max, "maximum y-axis")
  if (lo >= hi) {
    stop(
      reservoir_label, " y-axis minimum (", lo, " ft) must be less than its maximum (", hi, " ft).",
      call. = FALSE
    )
  }
  c(lo, hi)
}

chart24ms_month_heading <- function(x) {
  month_number <- as.integer(substr(x, 6, 7))
  paste(month.name[month_number], substr(x, 1, 4))
}

# Latest "Max-7" run month (Jan/Apr/Aug/Oct) on or before the given month.
# The Probable Maximum 7.0 maf trace is only run quarterly (Jan, Apr, Aug,
# Oct), so its default run month is the most recent of those four on or
# before the study month (wrapping back to October of the previous year
# when the study month is before January's run, i.e. never for a real month,
# but kept for safety).
chart24ms_latest_max7_run_month <- function(study_month) {
  study_month <- validate_chart24ms_month(study_month, "Study month")
  year <- as.integer(substr(study_month, 1, 4))
  month_num <- as.integer(substr(study_month, 6, 7))
  max7_months <- c(1L, 4L, 8L, 10L)
  candidates <- max7_months[max7_months <= month_num]
  if (length(candidates) > 0) {
    sprintf("%04d-%02d", year, max(candidates))
  } else {
    sprintf("%04d-%02d", year - 1L, max(max7_months))
  }
}

# Subtitle month text built from the distinct run months of the traces.
#   one month                    -> "November 2026"
#   two months, same year        -> "October and November 2026"
#   two months, different years  -> "December 2026 and January 2027"
#   three or more                -> "August, October and November 2026"
chart24ms_subtitle_months <- function(run_months) {
  months_sorted <- sort(unique(run_months))
  if (length(months_sorted) == 1) return(chart24ms_month_heading(months_sorted))
  years <- substr(months_sorted, 1, 4)
  names_only <- month.name[as.integer(substr(months_sorted, 6, 7))]
  labels <- if (length(unique(years)) == 1) {
    names_only
  } else {
    vapply(months_sorted, chart24ms_month_heading, character(1), USE.NAMES = FALSE)
  }
  n <- length(labels)
  text <- paste0(paste(labels[-n], collapse = ", "), " and ", labels[n])
  if (length(unique(years)) == 1) text <- paste(text, years[1])
  text
}

## -----------------------------------------------------------------------------
## HDB ODBC FALLBACK & CREDENTIAL SESSION (process-local, in-memory only)
## -----------------------------------------------------------------------------
# Follows the OpsTools HDB API-to-ODBC fallback pattern:
#   1. Try the HDB HTTP API (rhdb::hdb_query) first unless data_source == "odbc".
#   2. On API failure / empty response, fall back to a local 64-bit ODBC DSN
#      (default "vblchdb") connected to Oracle HDB (lchdb, with @uchdb2 / @yaohdb
#      database links for Upper Colorado and Yuma).
#   3. Cache working credentials in process memory only — never written to disk
#      and always redacted from error messages.

if (!exists(".hdb_odbc_session", inherits = FALSE)) {
  .hdb_odbc_session <- new.env(parent = emptyenv())
  .hdb_odbc_session$username <- ""
  .hdb_odbc_session$password <- ""
  .hdb_odbc_session$dsn <- "vblchdb"
}

hdb_session_store <- function(username, password, dsn = NULL) {
  username <- trimws(as.character(username)[1])
  password <- as.character(password)[1]
  if (is.na(username) || !nzchar(username) || is.na(password) || !nzchar(password)) {
    stop("Both HDB username and password are required.", call. = FALSE)
  }
  .hdb_odbc_session$username <- username
  .hdb_odbc_session$password <- password
  if (!is.null(dsn) && nzchar(trimws(as.character(dsn)[1]))) {
    .hdb_odbc_session$dsn <- trimws(as.character(dsn)[1])
  }
  invisible(TRUE)
}

hdb_session_available <- function() {
  nzchar(.hdb_odbc_session$username) && nzchar(.hdb_odbc_session$password)
}

hdb_session_status <- function() {
  list(
    available = hdb_session_available(),
    username  = .hdb_odbc_session$username,
    dsn       = .hdb_odbc_session$dsn
  )
}

hdb_session_clear <- function() {
  .hdb_odbc_session$username <- ""
  .hdb_odbc_session$password <- ""
  invisible(TRUE)
}

hdb_resolve_credentials <- function(username = NULL, password = NULL, dsn = NULL) {
  u <- if (is.null(username) || length(username) == 0 || is.na(username[1])) "" else trimws(as.character(username[1]))
  p <- if (is.null(password) || length(password) == 0 || is.na(password[1])) "" else as.character(password[1])
  d <- if (is.null(dsn) || length(dsn) == 0 || is.na(dsn[1]) || !nzchar(trimws(as.character(dsn[1])))) {
    .hdb_odbc_session$dsn
  } else {
    trimws(as.character(dsn[1]))
  }
  if (!nzchar(u) || !nzchar(p)) {
    if (hdb_session_available()) {
      u <- .hdb_odbc_session$username
      p <- .hdb_odbc_session$password
    }
  }
  list(username = u, password = p, dsn = d)
}

hdb_redact_error <- function(msg, password = NULL) {
  msg <- as.character(msg)
  for (pw in c(password, .hdb_odbc_session$password)) {
    if (!is.null(pw) && length(pw) == 1 && !is.na(pw) && nzchar(pw)) {
      msg <- gsub(pw, "***", msg, fixed = TRUE)
    }
  }
  msg <- gsub("(PWD\\s*=\\s*)[^;\\s]+", "\\1***", msg, ignore.case = TRUE, perl = TRUE)
  msg
}

# Build validated table reference from HDB server code ("lc"/"lchdb" -> local
# table on lchdb; "uc"/"uchdb2" -> table@uchdb2; "yao"/"yaohdb" -> table@yaohdb)
# and interval ("m" -> r_month for observed or m_month for model-run data).
hdb_odbc_table_ref <- function(svr, tstp = "m", has_mrid = FALSE) {
  svr_key <- tolower(trimws(as.character(svr)[1]))
  svr_map <- c(
    "lc" = "lchdb", "lchdb" = "lchdb",
    "uc" = "uchdb2", "uchdb2" = "uchdb2",
    "yao" = "yaohdb", "yaohdb" = "yaohdb"
  )
  if (is.na(svr_key) || !svr_key %in% names(svr_map)) {
    stop("Unsupported HDB server alias: ", svr, ". Allowed: lc/lchdb, uc/uchdb2, yao/yaohdb.", call. = FALSE)
  }
  resolved_svr <- unname(svr_map[svr_key])

  tstp_key <- tolower(trimws(as.character(tstp)[1]))
  prefix <- if (isTRUE(has_mrid)) "m_" else "r_"
  base_table <- switch(
    tstp_key,
    "m"  = , "mo" = , "mon" = paste0(prefix, "month"),
    "dy" = , "d"  = paste0(prefix, "day"),
    "hr" = , "h"  = paste0(prefix, "hour"),
    "in" = , "iv" = , "i" = paste0(prefix, "instant"),
    stop("Unsupported HDB interval: ", tstp, call. = FALSE)
  )

  # Double-check identifier characters before interpolating into FROM clause.
  table_ref <- if (identical(resolved_svr, "lchdb")) {
    base_table
  } else {
    paste0(base_table, "@", resolved_svr)
  }
  if (!grepl("^[a-z_]+(@[a-z0-9_]+)?$", table_ref)) {
    stop("Invalid HDB table reference: ", table_ref, call. = FALSE)
  }
  table_ref
}

# Convert a "YYYY-MM" month string to the start or end Date of that month.
hdb_month_boundary_date <- function(ym_str, end_of_month = FALSE) {
  ym_str <- validate_chart24ms_month(ym_str, "Query month")
  first_day <- as.Date(paste0(ym_str, "-01"))
  if (!isTRUE(end_of_month)) return(first_day)
  seq(first_day, by = "month", length.out = 2)[2] - 1
}

# Open an ODBC connection to DSN=vblchdb using DBI+odbc (preferred) or RODBC.
hdb_odbc_connect <- function(username = NULL, password = NULL, dsn = NULL) {
  creds <- hdb_resolve_credentials(username, password, dsn)
  if (!nzchar(creds$username) || !nzchar(creds$password)) {
    err <- simpleError(
      "HDB API was unavailable and no HDB ODBC credentials are set. Enter your HDB username and password to connect directly via ODBC."
    )
    class(err) <- c("chart24ms_odbc_auth_needed", class(err))
    stop(err)
  }
  if (!grepl("^[A-Za-z0-9_.-]+$", creds$dsn)) {
    stop("Invalid ODBC DSN name: ", creds$dsn, call. = FALSE)
  }

  conn_str <- paste0("DSN=", creds$dsn, ";UID=", creds$username, ";PWD=", creds$password)

  if (requireNamespace("DBI", quietly = TRUE) && requireNamespace("odbc", quietly = TRUE)) {
    con <- tryCatch(
      DBI::dbConnect(odbc::odbc(), .connection_string = conn_str),
      error = function(e) {
        stop(
          "HDB ODBC connection to DSN '", creds$dsn, "' failed: ",
          hdb_redact_error(conditionMessage(e), creds$password),
          call. = FALSE
        )
      }
    )
    hdb_session_store(creds$username, creds$password, creds$dsn)
    return(list(backend = "dbi", con = con, creds = creds))
  }

  if (requireNamespace("RODBC", quietly = TRUE)) {
    ch <- RODBC::odbcDriverConnect(conn_str, believeNRows = FALSE)
    if (identical(ch, -1L) || !inherits(ch, "RODBC")) {
      stop(
        "HDB ODBC connection to DSN '", creds$dsn,
        "' failed via RODBC. Check your HDB credentials and 64-bit DSN configuration.",
        call. = FALSE
      )
    }
    hdb_session_store(creds$username, creds$password, creds$dsn)
    return(list(backend = "rodbc", con = ch, creds = creds))
  }

  stop(
    "Neither the 'DBI'+'odbc' packages nor 'RODBC' are installed for HDB ODBC fallback. Run Setup_Chart24MS.bat first.",
    call. = FALSE
  )
}

hdb_odbc_disconnect <- function(handle) {
  if (is.null(handle)) return(invisible(NULL))
  tryCatch({
    if (identical(handle$backend, "dbi")) {
      DBI::dbDisconnect(handle$con)
    } else if (identical(handle$backend, "rodbc")) {
      RODBC::odbcClose(handle$con)
    }
  }, error = function(e) NULL)
  invisible(NULL)
}

# Execute a single HDB query over an open ODBC handle and return a dataframe
# with the exact columns returned by rhdb::hdb_query (sdi, time_step, value, mrid).
hdb_odbc_query_single <- function(handle, sdi, svr, tstp, t1, t2, mrid = NULL) {
  sdi_int <- validate_chart24ms_mrid(sdi, "SDI")
  has_mrid <- !is.null(mrid) && length(mrid) == 1 && !is.na(mrid) && nzchar(as.character(mrid))
  mrid_int <- if (has_mrid) validate_chart24ms_mrid(mrid, "MRID") else NA_integer_

  table_ref <- hdb_odbc_table_ref(svr = svr, tstp = tstp, has_mrid = has_mrid)
  start_str <- format(hdb_month_boundary_date(t1, end_of_month = FALSE), "%Y-%m-%d")
  end_str   <- format(hdb_month_boundary_date(t2, end_of_month = TRUE),  "%Y-%m-%d")

  # All values here are validated integers or YYYY-MM-DD dates; table_ref is
  # validated against an allow-list in hdb_odbc_table_ref().
  sql <- if (has_mrid) {
    sprintf(
      paste0(
        "SELECT site_datatype_id AS sdi, ",
        "TO_CHAR(start_date_time, 'YYYY-MM-DD HH24:MI:SS') AS time_step, ",
        "value, model_run_id AS mrid ",
        "FROM %s ",
        "WHERE site_datatype_id = %d ",
        "AND model_run_id = %d ",
        "AND start_date_time >= TO_DATE('%s 00:00:00', 'YYYY-MM-DD HH24:MI:SS') ",
        "AND start_date_time <= TO_DATE('%s 23:59:59', 'YYYY-MM-DD HH24:MI:SS') ",
        "ORDER BY start_date_time"
      ),
      table_ref, sdi_int, mrid_int, start_str, end_str
    )
  } else {
    sprintf(
      paste0(
        "SELECT site_datatype_id AS sdi, ",
        "TO_CHAR(start_date_time, 'YYYY-MM-DD HH24:MI:SS') AS time_step, ",
        "value ",
        "FROM %s ",
        "WHERE site_datatype_id = %d ",
        "AND start_date_time >= TO_DATE('%s 00:00:00', 'YYYY-MM-DD HH24:MI:SS') ",
        "AND start_date_time <= TO_DATE('%s 23:59:59', 'YYYY-MM-DD HH24:MI:SS') ",
        "ORDER BY start_date_time"
      ),
      table_ref, sdi_int, start_str, end_str
    )
  }

  raw <- tryCatch({
    if (identical(handle$backend, "dbi")) {
      DBI::dbGetQuery(handle$con, sql)
    } else {
      RODBC::sqlQuery(handle$con, sql, stringsAsFactors = FALSE, as.is = TRUE)
    }
  }, error = function(e) {
    stop(
      "HDB ODBC query on ", table_ref, " (SDI ", sdi_int, ") failed: ",
      hdb_redact_error(conditionMessage(e), handle$creds$password),
      call. = FALSE
    )
  })

  if (is.character(raw)) {
    stop(
      "HDB ODBC query on ", table_ref, " (SDI ", sdi_int, ") failed: ",
      hdb_redact_error(paste(raw, collapse = " "), handle$creds$password),
      call. = FALSE
    )
  }

  names(raw) <- tolower(names(raw))
  if (nrow(raw) == 0) {
    out <- data.frame(
      sdi = integer(0),
      time_step = character(0),
      value = numeric(0),
      stringsAsFactors = FALSE
    )
    if (has_mrid) out$mrid <- integer(0)
    return(out)
  }

  out <- data.frame(
    sdi       = as.integer(raw$sdi),
    time_step = as.character(raw$time_step),
    value     = as.numeric(raw$value),
    stringsAsFactors = FALSE
  )
  if (has_mrid) {
    out$mrid <- as.integer(raw$mrid)
  }
  out
}

# Run a batch of HDB queries using the API-first, ODBC-fallback strategy.
# Each entry in `queries` is list(sdi = ..., svr = "uc"/"lc", tstp = "m",
# t1 = "YYYY-MM", t2 = "YYYY-MM", mrid = NULL or integer).
# Returns list(data = bind_rows(...), source_used = "api" | "odbc", api_error = NULL | string).
hdb_fetch_batch <- function(
  queries,
  data_source = c("auto", "api", "odbc"),
  odbc_username = NULL,
  odbc_password = NULL,
  odbc_dsn = "vblchdb",
  progress = NULL
) {
  data_source <- match.arg(data_source)
  api_error_msg <- NULL

  if (!identical(data_source, "odbc")) {
    api_result <- tryCatch({
      rows <- lapply(queries, function(q) {
        res <- if (is.null(q$mrid) || is.na(q$mrid)) {
          hdb_query(q$sdi, q$svr, q$tstp, q$t1, q$t2)
        } else {
          hdb_query(q$sdi, q$svr, q$tstp, q$t1, q$t2, q$mrid)
        }
        if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
          stop(
            sprintf("HDB API returned no rows for SDI %s (%s, %s to %s).", q$sdi, q$svr, q$t1, q$t2),
            call. = FALSE
          )
        }
        res
      })
      bind_rows(rows)
    }, error = function(e) {
      api_error_msg <<- conditionMessage(e)
      NULL
    })

    if (!is.null(api_result) && nrow(api_result) > 0) {
      return(list(data = api_result, source_used = "api", api_error = NULL))
    }

    if (identical(data_source, "api")) {
      stop("HDB API download failed: ", api_error_msg, call. = FALSE)
    }

    message("HDB API unavailable (", api_error_msg, "). Falling back to direct ODBC connection...")
    if (is.function(progress)) {
      progress("HDB API unavailable; connecting via ODBC...", 0.22)
    }
  } else {
    if (is.function(progress)) {
      progress("Connecting to HDB via ODBC...", 0.18)
    }
  }

  # ODBC path (either forced or fallback after API error).
  # If credentials are missing, attach the API error message to the auth-needed
  # condition so the UI can explain why it is prompting for HDB credentials.
  handle <- tryCatch(
    hdb_odbc_connect(username = odbc_username, password = odbc_password, dsn = odbc_dsn),
    chart24ms_odbc_auth_needed = function(e) {
      if (!is.null(api_error_msg) && nzchar(api_error_msg)) {
        e$message <- paste0(
          "HDB API failed (", api_error_msg, "). ",
          "Enter your HDB username and password to download directly via ODBC."
        )
      }
      stop(e)
    }
  )
  on.exit(hdb_odbc_disconnect(handle), add = TRUE)

  odbc_rows <- lapply(queries, function(q) {
    hdb_odbc_query_single(
      handle = handle,
      sdi    = q$sdi,
      svr    = q$svr,
      tstp   = q$tstp,
      t1     = q$t1,
      t2     = q$t2,
      mrid   = q$mrid
    )
  })

  list(
    data        = bind_rows(odbc_rows),
    source_used = "odbc",
    api_error   = api_error_msg
  )
}

generate_plots <- function(
  run_date,
  min_6_mrid,
  most_6_mrid,
  most_7_mrid,
  max_7_mrid,
  min_6_run_date,
  most_6_run_date,
  most_7_run_date,
  max_7_run_date,
  most_6_enabled = TRUE,
  most_7_enabled = TRUE,
  end_month = NULL,
  powell_y_min = 3400,
  powell_y_max = 3625,
  mead_y_min = 950,
  mead_y_max = 1100,
  data_source = c("auto", "api", "odbc"),
  odbc_username = NULL,
  odbc_password = NULL,
  odbc_dsn = "vblchdb",
  project_dir = getwd(),
  output_dir = NULL,
  progress_callback = NULL
) {
  load_chart24ms_packages()

  progress <- function(message, value = NULL) {
    if (is.function(progress_callback)) {
      progress_callback(message, value)
    }
  }

  run_date <- validate_chart24ms_month(run_date, "Study month")
  end_date <- resolve_chart24ms_end_month(end_month, run_date)
  data_source <- match.arg(data_source)
  yy <- resolve_chart24ms_ylim(powell_y_min, powell_y_max, 3400, 3625, "Lake Powell")
  yy_m <- resolve_chart24ms_ylim(mead_y_min, mead_y_max, 950, 1100, "Lake Mead")

  most_6_enabled <- isTRUE(most_6_enabled)
  most_7_enabled <- isTRUE(most_7_enabled)
  if (!most_6_enabled && !most_7_enabled) {
    stop("At least one Most Probable scenario (6.0 maf or 7.0 maf) must be included.", call. = FALSE)
  }

  min_6_run_date <- validate_chart24ms_month(min_6_run_date, "Probable Minimum 6.0 maf run month")
  most_6_run_date <- if (most_6_enabled) {
    validate_chart24ms_month(most_6_run_date, "Most Probable 6.0 maf run month")
  } else NA_character_
  most_7_run_date <- if (most_7_enabled) {
    validate_chart24ms_month(most_7_run_date, "Most Probable 7.0 maf run month")
  } else NA_character_
  max_7_run_date <- validate_chart24ms_month(max_7_run_date, "Probable Maximum 7.0 maf run month")

  min_6_mrid <- validate_chart24ms_mrid(min_6_mrid, "Probable Minimum 6.0 maf MRID")
  most_6_mrid <- if (most_6_enabled) {
    validate_chart24ms_mrid(most_6_mrid, "Most Probable 6.0 maf MRID")
  } else NA_integer_
  most_7_mrid <- if (most_7_enabled) {
    validate_chart24ms_mrid(most_7_mrid, "Most Probable 7.0 maf MRID")
  } else NA_integer_
  max_7_mrid <- validate_chart24ms_mrid(max_7_mrid, "Probable Maximum 7.0 maf MRID")

  # A trace's run month may be earlier than the study month (its latest run is
  # carried forward), but never later.
  later_runs <- c(
    "Probable Minimum 6.0 maf" = min_6_run_date,
    if (most_6_enabled) c("Most Probable 6.0 maf" = most_6_run_date),
    if (most_7_enabled) c("Most Probable 7.0 maf" = most_7_run_date),
    "Probable Maximum 7.0 maf" = max_7_run_date
  )
  later_runs <- later_runs[later_runs > run_date]
  if (length(later_runs) > 0) {
    stop(
      "Run month cannot be later than the study month (", run_date, "): ",
      paste(names(later_runs), collapse = ", "), ".",
      call. = FALSE
    )
  }

  mrids <- c(min_6_mrid, most_6_mrid, most_7_mrid, max_7_mrid)
  mrids <- mrids[!is.na(mrids)]
  if (anyDuplicated(mrids)) {
    stop("The four MRIDs must be unique so each HDB run can be mapped to the correct trace.", call. = FALSE)
  }

  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_dir)

  if (!file.exists(file.path("data", "powellElevationVolume.csv")) ||
      !file.exists(file.path("data", "meadElevationVolume.csv"))) {
    stop("Required elevation-volume CSV files were not found in the data folder.", call. = FALSE)
  }

  if (is.null(output_dir)) {
    output_dir <- file.path(project_dir, "output", run_date)
  } else {
    output_dir <- path.expand(output_dir)
    if (!grepl("^([A-Za-z]:|/|\\\\)", output_dir)) {
      output_dir <- file.path(project_dir, output_dir)
    }
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  month_heading <- chart24ms_subtitle_months(c(
    min_6_run_date,
    if (most_6_enabled) most_6_run_date,
    if (most_7_enabled) most_7_run_date,
    max_7_run_date
  ))
  min_6_heading <- chart24ms_month_heading(min_6_run_date)
  most_6_heading <- if (most_6_enabled) chart24ms_month_heading(most_6_run_date) else ""
  most_7_heading <- if (most_7_enabled) chart24ms_month_heading(most_7_run_date) else ""
  max_7_heading <- chart24ms_month_heading(max_7_run_date)

  lab_names <- c(
    "Historical" = "Historical Elevations",
    "24MS Min-6maf" = paste0(min_6_heading, " Probable Minimum (6.0 maf release)"),
    if (most_6_enabled) c("24MS Most-6maf" = paste0(most_6_heading, " Most Probable (6.0 maf release)")),
    if (most_7_enabled) c("24MS Most-7maf" = paste0(most_7_heading, " Most Probable (7.0 maf release)")),
    "24MS Max-7maf" = paste0(max_7_heading, " Probable Maximum (7.0 maf release)")
  )

  active_traces <- names(lab_names)

  all_colors <- c(
    'Historical'       = 'grey20', 
    '24MS Max-7maf'    = '#0e3964',
    '24MS Most-7maf'   = '#2E8B57',
    '24MS Most-6maf'   = '#5ed45e',
    '24MS Min-6maf'    = '#DA3139'
  )

  trace_numbers <- c(
    '24MS Max-7maf'    = "",
    '24MS Most-7maf'   = "7",
    '24MS Most-6maf'   = "6",
    '24MS Min-6maf'    = ""
  )

  # NA (numeric) hides the circle for that break — safe, no character-coercion crash
  legend_circle_shape <- c(
    'Historical'       = NA,
    '24MS Min-6maf'    = NA,
    '24MS Most-6maf'   = 21,
    '24MS Most-7maf'   = 21,
    '24MS Max-7maf'    = NA
  )

  legend_circle_label <- c(
    'Historical'       = "",
    '24MS Min-6maf'    = "",
    '24MS Most-6maf'   = "6",
    '24MS Most-7maf'   = "7",
    '24MS Max-7maf'    = ""
  )

  # The real display order now comes from lab_names, not active_traces
  legend_order_keys <- names(lab_names)[names(lab_names) %in% active_traces]
  # ----------------------------------------

  all_sizes  <- setNames(rep(0.8, length(lab_names)), names(lab_names))
  all_alpha  <- setNames(rep(1, length(lab_names)), names(lab_names))
  all_lt     <- c(
    'Historical'       = 1,
    '24MS Min-6maf'    = 1,
    '24MS Most-6maf'   = 1,
    '24MS Most-7maf'   = 1,
    '24MS Max-7maf'    = 1
  )

  custom_colors <- all_colors[active_traces]
  custom_size   <- all_sizes[active_traces]
  custom_lt     <- all_lt[active_traces]
  custom_alpha  <- all_alpha[active_traces]

  names(custom_colors) <- names(custom_size) <- names(custom_lt) <- names(custom_alpha) <- unname(lab_names[active_traces])


  ## -----------------------------------------------------------------------------
  ## DATA IMPORT & PREPARATION (FROM HDB)
  ## -----------------------------------------------------------------------------
  progress("Downloading HDB data...", 0.15)
    print("Loading and preparing elevation data from HDB...")

  # end_date (last month plotted) was resolved above: study month + 23 by
  # default, or the user's earlier "Plot end month".

  # SDIs for Powell and Mead pool elevations
  sdis <- c("Mead.Pool Elevation" = 1930, "Powell.Pool Elevation" = 1928)

  # Map MRIDs directly to the trace names expected by the plotting aesthetics
  # (disabled traces have mrid = NA and are dropped here).
  mrid_to_trace <- c("24MS Min-6maf", "24MS Most-6maf", "24MS Most-7maf", "24MS Max-7maf")
  names(mrid_to_trace) <- c(min_6_mrid, most_6_mrid, most_7_mrid, max_7_mrid)
  mrid_to_trace <- mrid_to_trace[!is.na(names(mrid_to_trace))]

  # 2. Query Projection Traces (up to 4 MRIDs x 2 Reservoirs = up to 8 queries)
  proj_queries <- list()
  if (most_6_enabled) {
    proj_queries <- c(proj_queries, list(
      list(sdi = sdis["Powell.Pool Elevation"], svr = "uc", tstp = "m", t1 = most_6_run_date, t2 = end_date, mrid = most_6_mrid),
      list(sdi = sdis["Mead.Pool Elevation"],   svr = "lc", tstp = "m", t1 = most_6_run_date, t2 = end_date, mrid = most_6_mrid)
    ))
  }
  if (most_7_enabled) {
    proj_queries <- c(proj_queries, list(
      list(sdi = sdis["Powell.Pool Elevation"], svr = "uc", tstp = "m", t1 = most_7_run_date, t2 = end_date, mrid = most_7_mrid),
      list(sdi = sdis["Mead.Pool Elevation"],   svr = "lc", tstp = "m", t1 = most_7_run_date, t2 = end_date, mrid = most_7_mrid)
    ))
  }
  proj_queries <- c(proj_queries, list(
    list(sdi = sdis["Powell.Pool Elevation"], svr = "uc", tstp = "m", t1 = min_6_run_date, t2 = end_date, mrid = min_6_mrid),
    list(sdi = sdis["Mead.Pool Elevation"],   svr = "lc", tstp = "m", t1 = min_6_run_date, t2 = end_date, mrid = min_6_mrid),
    list(sdi = sdis["Powell.Pool Elevation"], svr = "uc", tstp = "m", t1 = max_7_run_date, t2 = end_date, mrid = max_7_mrid),
    list(sdi = sdis["Mead.Pool Elevation"],   svr = "lc", tstp = "m", t1 = max_7_run_date, t2 = end_date, mrid = max_7_mrid)
  ))

  proj_batch <- hdb_fetch_batch(
    queries       = proj_queries,
    data_source   = data_source,
    odbc_username = odbc_username,
    odbc_password = odbc_password,
    odbc_dsn      = odbc_dsn,
    progress      = progress
  )
  df_hdb <- proj_batch$data
  source_used <- proj_batch$source_used

  # Format the projection dataframe to match the plot requirements
  projections <- df_hdb %>%
    mutate(
      slot = names(sdis)[match(sdi, sdis)],
      run = run_date,
      Trace = mrid_to_trace[as.character(mrid)],
      value = as.numeric(value)
    ) %>%
    rename(Date = time_step) %>%
    mutate(Date = as.yearmon(parse_date_time(Date, c("m/d/y H:M:S", "Y-m-d H:M:S")))) %>%
    select(run, slot, Date, value, Trace)

  # A trace may come from an earlier run month than the study month (e.g., the
  # Max-7 trace is only run in Jan, Apr, Aug, and Oct). Keep that trace's latest
  # run, but only plot it from the study month forward. Months before the study
  # month are historical (observed) and must not come from an older projection.
  study_start <- as.yearmon(ym(run_date))
  projections <- projections %>% filter(Date >= study_start)

  empty_traces <- setdiff(unname(mrid_to_trace), unique(projections$Trace))
  if (length(empty_traces) > 0) {
    stop(
      "HDB returned no data on or after ", run_date, " for: ",
      paste(unname(lab_names[empty_traces]), collapse = ", "),
      ". Check the MRID and run month for each trace.",
      call. = FALSE
    )
  }

  # 3. Query Historical Data (7 months prior to run_date)
  hist_nMons <- 7 
  histStart_date <- format(ym(run_date) - months(hist_nMons), "%Y-%m")
  histEnd_date <- format(ym(run_date) - months(1), "%Y-%m")

  hist_queries <- list(
    list(sdi = sdis["Powell.Pool Elevation"], svr = "uc", tstp = "m", t1 = histStart_date, t2 = histEnd_date, mrid = NULL),
    list(sdi = sdis["Mead.Pool Elevation"],   svr = "lc", tstp = "m", t1 = histStart_date, t2 = histEnd_date, mrid = NULL)
  )
  # If projection queries already fell back to ODBC, stay on ODBC for history too.
  hist_source_mode <- if (identical(source_used, "odbc")) "odbc" else data_source
  hist_batch <- hdb_fetch_batch(
    queries       = hist_queries,
    data_source   = hist_source_mode,
    odbc_username = odbc_username,
    odbc_password = odbc_password,
    odbc_dsn      = odbc_dsn,
    progress      = progress
  )
  df_hist_raw <- hist_batch$data
  if (identical(hist_batch$source_used, "odbc")) {
    source_used <- "odbc"
  }

  df_hist <- df_hist_raw %>%
    mutate(
      slot = names(sdis)[match(sdi, sdis)],
      run = run_date,
      Trace = 'Historical',
      value = as.numeric(value)
    ) %>%
    rename(Date = time_step) %>%
    mutate(Date = as.yearmon(parse_date_time(Date, c("m/d/y H:M:S", "Y-m-d H:M:S")))) %>%
    select(run, slot, Date, value, Trace) %>% 
    na.omit()

  # 4. Seamlessly stitch the last historical point to the start of the projections
  df_hist_last <- df_hist %>% group_by(slot) %>% filter(Date == max(Date)) %>% ungroup()

  hist_to_append <- projections %>% 
    select(slot, Trace) %>% 
    distinct() %>% 
    left_join(df_hist_last %>% select(-Trace), by = "slot") %>% 
    select(run, slot, Date, value, Trace)

  # Combine everything into the final df_24MS dataframe used by the plotting code
  df_24MS <- bind_rows(projections, hist_to_append, df_hist) %>% 
    arrange(slot, Trace, Date) %>% 
    distinct()

  # Generate labels and attributes for ggplot
  max_hist_date <- max((df_24MS %>% filter(Trace == "Historical"))$Date, na.rm = TRUE)

  df_24MS_m = df_24MS %>% mutate(
    trace_labels = lab_names[Trace],
    trace_number = ifelse(Date > max_hist_date, unname(trace_numbers[Trace]), NA)
  )
  df_24MS_m$trace_labels = factor(df_24MS_m$trace_labels, levels = unname(lab_names))


  ## -----------------------------------------------------------------------------
  ## CLOUD (RIBBON) CALCULATIONS
  ## -----------------------------------------------------------------------------
  cloud_name <- '24MS Projections Range'
  cloud_color <- '#DDCBA4'
  cloud_cutoff <- ym(end_date) + months(1)   # ribbon stops at the plot end month

  # Dynamically calculate the min and max of whatever traces are active
  df_stat_unified <- df_24MS_m %>% 
    # Ribbon opens at the stitch point (last historical month = the black line,
    # i.e. the start of the study month, where all traces share one value) and
    # widens across the projected months. Older-run data before that is excluded.
    filter(Trace != "Historical", Date >= max_hist_date) %>%
    group_by(run, slot, Date) %>%
    summarise(cloud.max = max(value, na.rm = TRUE),
              cloud.min = min(value, na.rm = TRUE), .groups = "drop") %>%
    filter(as.Date(Date) < cloud_cutoff) %>% 
    mutate(Cloud = factor(cloud_name), Date = as.yearmon(Date)) 

  df_stat_p_unified <- df_stat_unified %>% filter(slot == 'Powell.Pool Elevation')
  df_stat_m_unified <- df_stat_unified %>% filter(slot == 'Mead.Pool Elevation')
  df_24MS_p <- df_24MS_m %>% filter(slot == 'Powell.Pool Elevation')
  df_24MS_m <- df_24MS_m %>% filter(slot == 'Mead.Pool Elevation')

  ## -----------------------------------------------------------------------------
  ## HELPER FUNCTIONS & SECONDARY AXIS DATA
  ## -----------------------------------------------------------------------------
  # eq_tier_df <- function() {
  #   Timestep = matrix(seq.Date(as.Date('2007-10-1'), as.Date('2026-09-1'), 'months') - 1, byrow = T, ncol = 12)
  #   Timestep = as.Date(as.vector(t(cbind(Timestep[,1], Timestep))), origin = '1970-01-01')[-1]
  #   powell_line = data.frame(Date = as.POSIXct(Timestep), Eq_elev = rep(c(3636, 3639, 3642, 3643, 3645, 3646, 3648, 3649, 3651, 3652, 3654, 3655, 3657, 3659, 3660, 3662, 3663, 3664, 3666), each=13)[1:length(Timestep)], check.names = F, stringsAsFactors = FALSE) %>% mutate(Date = as.yearmon(Date)) 
  #   dates_2 = as.yearmon(seq.Date(as.Date('2026-09-1'), as.Date('2029-09-1'), 'months'))
  #   postEQ = data.frame(Date = dates_2, Eq_elev = rep(3666, length(dates_2)))
  #   powell_line = rbind.data.frame(powell_line, postEQ)
  #   return(powell_line)
  # }
  # powell_line <- eq_tier_df()

  tryCatch({
    evTables <- list(
      powell = read.csv(file.path("data", "powellElevationVolume.csv")),
      mead = read.csv(file.path("data", "meadElevationVolume.csv"))
    )
  }, error = function(e) {
    message("Could not read elevation-volume tables. Creating dummy data.")
    evTables <<- list(
      powell = data.frame(Elevation = c(3370, 3700), Volume = c(2000000, 24000000)),
      mead = data.frame(Elevation = c(900, 1220), Volume = c(2000000, 26000000))
    )
  })

  elevation_to_storage <- function(elevation, reservoir) {
    e2vFunc <- stats::approxfun(evTables[[reservoir]][,1], evTables[[reservoir]][,2])
    e2vFunc(elevation)
  }

  shared_caption = ''

  # Numbered circle markers, drawn one trace at a time so stacking is controlled:
  # Most-6 first, Most-7 last (on top). Each circle has an opaque white fill, and
  # its number is drawn right after it, so nothing behind a Most-7 marker shows.
  marker_layers <- function(df) {
    marks <- filter(df, !is.na(trace_number) & trace_number != "")
    layers <- list()
    for (tr in c("24MS Most-6maf", "24MS Most-7maf")) {   # later = on top
      d <- filter(marks, Trace == tr)
      if (nrow(d) == 0) next
      layers <- c(layers, list(
        geom_point(data = d, aes(y = value, color = trace_labels),
                   shape = 21, fill = "white", size = 3.5, stroke = 0.8,
                   show.legend = FALSE),
        geom_text(data = d, aes(y = value, label = trace_number, color = trace_labels),
                  size = 2.5, show.legend = FALSE)
      ))
    }
    layers
  }

  ## -----------------------------------------------------------------------------
  ## PLOT 1: POWELL
  ## -----------------------------------------------------------------------------
  p_grid_lo <- min(3350, floor(yy[1] / 25) * 25)
  p_grid_hi <- max(3725, ceiling(yy[2] / 25) * 25)
  p_breaks <- seq(p_grid_lo, p_grid_hi, 25)
  p_breaks2 <- seq(p_grid_lo, p_grid_hi, 5)
  min_date_limit <- min(df_24MS$Date, na.rm = TRUE)
  max_date_limit <- max(df_24MS$Date, na.rm = TRUE)

    # Annual reference lines are derived from the data window so they never go stale:
    #   - Powell: every September (end of water year), except when September is
    #     the last month plotted (strictly `< max_date_limit`).
    #   - Mead:   every December (end of calendar year), except when December is
    #     the last month plotted (strictly `< max_date_limit`).
    plot_years <- seq(
      lubridate::year(as.Date(min_date_limit)),
      lubridate::year(as.Date(max_date_limit))
    )
    powell_reference_lines <- zoo::as.yearmon(sprintf("%04d-09", plot_years), "%Y-%m")
    powell_reference_lines <- powell_reference_lines[
      powell_reference_lines >= min_date_limit & powell_reference_lines < max_date_limit
    ]
    mead_reference_lines <- zoo::as.yearmon(sprintf("%04d-12", plot_years), "%Y-%m")
    mead_reference_lines <- mead_reference_lines[
      mead_reference_lines >= min_date_limit & mead_reference_lines < max_date_limit
    ]

  gg_p <- ggplot(mapping = aes(x = Date)) + 
    geom_hline(yintercept = c(3565, 3540, 3510), color = 'grey20', linetype = 'dashed', size = 0.5) +
    geom_hline(yintercept = 3490, color = 'grey20', linetype = 2, size = 0.5) +
    { if (length(powell_reference_lines) > 0) geom_vline(xintercept = powell_reference_lines, size = 0.7, color = "#ffdc70", alpha = 0.8) } +
    geom_ribbon(data = df_stat_p_unified, aes(ymin = cloud.min, ymax = cloud.max), fill = cloud_color, alpha = 0.2) +
    geom_line(data = df_24MS_p, aes(y = value, color = trace_labels, linetype = trace_labels, size = trace_labels)) +
    marker_layers(df_24MS_p) +
    # --- Legend-only glyphs: invisible in the panel, only used to size the legend key ---
    geom_point(data = filter(df_24MS_p, !is.na(trace_number) & trace_number != ""), 
              aes(y = value, color = trace_labels), 
              shape = 21, fill = "white", size = 3, stroke = 0.6, alpha = 0, 
              show.legend = TRUE) +
    geom_text(data = filter(df_24MS_p, !is.na(trace_number) & trace_number != ""), 
              aes(y = value, label = trace_number, color = trace_labels), 
              size = 1.8, alpha = 0, 
              show.legend = TRUE) +
    # geom_line(data = powell_line, aes(y = Eq_elev), colour = "black", linetype = 1, size = 0.6) +
    scale_color_manual(values = custom_colors, name = NULL) +
    scale_linetype_manual(values = custom_lt, name = NULL) +
    scale_size_manual(values = custom_size, name = NULL) +
    scale_x_yearmon(expand = c(0,0), breaks = unique(df_24MS$Date), minor_breaks = unique(df_24MS$Date), limits = c(min_date_limit, max_date_limit)) +
    scale_y_continuous(labels = scales::comma, breaks = p_breaks, minor_breaks = p_breaks2, limits = yy, expand = c(0,0), sec.axis = sec_axis(~elevation_to_storage(., "powell"), breaks = elevation_to_storage(p_breaks, "powell"), labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01), name = "Storage (maf)")) +
    labs(y = "Pool Elevation (ft)", x = NULL, title = bquote('Lake Powell End-of-Month'~Elevations), subtitle = paste('Projections from', month_heading, '24-Month Study Inflow Scenarios'), caption = shared_caption) +
    # geom_hline(yintercept = c(3565, 3540), color = 'black', linetype = 'dashed', size = 0.5) +
    # geom_hline(yintercept = 3490, color = 'grey50', linetype = 2, size = 0.5) +
    # geom_vline(xintercept = mead_reference_lines, size = 0.7, color = "#ffdc70", alpha = 0.8) +
    geom_vline(xintercept = as.yearmon(ym(run_date) %m-% months(1)), size = 0.5, color = "black") +
    annotate("text", x = as.yearmon(ym(run_date)- months(5)), 
             y=3487, label="Elevation 3,490 ft",  angle=00, size=3, hjust = 0) +
    annotate("text", x = as.yearmon(ym(run_date) - months(5)),
             y=3507, label="Elevation 3,510 ft", angle=00, size=3, hjust = 0) +
    annotate("text", x = as.yearmon(ym(run_date) - months(5)),
             y=3537, label="Elevation 3,540 ft", angle=00, size=3, hjust = 0) +
    annotate("text", x = as.yearmon(ym(run_date) - months(5)),
             y=3562, label="Elevation 3,565 ft", angle=00, size=3, hjust = 0) +
    guides(
      color = guide_legend(
        nrow = 2, byrow = TRUE,
        override.aes = list(
          shape = unname(legend_circle_shape[legend_order_keys]),
          label = unname(legend_circle_label[legend_order_keys]),
          alpha = 1
        )
      ),
      linetype = guide_legend(nrow = 2, byrow = TRUE),
      size = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    # theme_bw(base_size = 10) +
    # theme(legend.position = "bottom", plot.title = element_text(face="bold", hjust = 0.5, size = 13), plot.subtitle = element_text(hjust = 0.5, size = 11), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.caption = element_text(hjust = 0, size = 7, face = "italic"))
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.justification = "left",   # <-- left-aligns the legend block
      legend.box.spacing = unit(0.8, "cm"),          # gap between panel/x-axis and legend
      legend.margin = margin(t = 4, b = 6),          # extra padding above/below legend items
      plot.title = element_text(face="bold", hjust = 0.5, size = 13),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, margin = margin(t = 4)),
      plot.caption = element_text(hjust = 0, size = 7, face = "italic", margin = margin(t = 10)),
      plot.margin = margin(t = 10, r = 10, b = 8, l = 5)
    )

  progress("Rendering Lake Powell outputs...", 0.62)
    print("Saving individual Powell plot...")
  powell_png <- file.path(output_dir, "Powell24MS.png")
    powell_pdf <- file.path(output_dir, "Powell24MS.pdf")
    ggsave(powell_png, plot = gg_p, width = 11, height = 8.5, dpi = 300)
  tryCatch({
      logo_raw <- image_read(file.path("data","BofR-vert.png"))
      p_img <- image_read(powell_png)
      final_p <- image_composite(p_img, image_resize(logo_raw,"325"), gravity = "southeast", offset = "+30+100")
      image_write(final_p, powell_png)

      #Set density to 300x300 so PDF page size comes out correct (11 x 8.5 in)
      image_write(final_p, powell_pdf, format = "pdf", density="300x300")
  }, error = function(e) { 
    message("Could not apply logo to individual Powell plot. Saving plot without logo instead.") 
    message("Actual error: ", conditionMessage(e))
    ggsave(powell_pdf, plot = gg_p, width = 11, height = 8.5, device = "pdf")
  })

  ## -----------------------------------------------------------------------------
  ## PLOT 2: MEAD
  ## -----------------------------------------------------------------------------
  m_grid_lo <- min(900, floor(yy_m[1] / 25) * 25)
  m_grid_hi <- max(1250, ceiling(yy_m[2] / 25) * 25)
  m_breaks <- seq(m_grid_lo, m_grid_hi, 25)
  m_breaks2 <- seq(m_grid_lo, m_grid_hi, 5)

  gg_m <- ggplot(mapping = aes(x = Date)) + 
    geom_hline(yintercept = c(1025, 1000), colour = 'black', linetype = 'dashed', size = 0.5) +
    geom_hline(yintercept = c(1035, 1010), colour = 'black', linetype = 'dashed', size = 0.5) +
    { if (length(mead_reference_lines) > 0) geom_vline(xintercept = mead_reference_lines, size = 0.7, color = "#ffdc70", alpha = 0.8) } +
    geom_ribbon(data = df_stat_m_unified, aes(ymin = cloud.min, ymax = cloud.max), fill = cloud_color, alpha = 0.2) +
    geom_line(data = df_24MS_m, aes(y = value, color = trace_labels, linetype = trace_labels, size = trace_labels)) +
    marker_layers(df_24MS_m) +
    # --- Legend-only glyphs: invisible in the panel, only used to size the legend key ---
    geom_point(data = filter(df_24MS_m, !is.na(trace_number) & trace_number != ""), 
              aes(y = value, color = trace_labels), 
              shape = 21, fill = "white", size = 3, stroke = 0.6, alpha = 0, 
              show.legend = TRUE) +
    geom_text(data = filter(df_24MS_m, !is.na(trace_number) & trace_number != ""), 
              aes(y = value, label = trace_number, color = trace_labels), 
              size = 1.8, alpha = 0, 
              show.legend = TRUE) +
    scale_color_manual(values = custom_colors, name = NULL) +
    scale_linetype_manual(values = custom_lt, name = NULL) +
    scale_size_manual(values = custom_size, name = NULL) +
    scale_x_yearmon(expand = c(0,0), breaks = unique(df_24MS$Date), minor_breaks = unique(df_24MS$Date), limits = c(min_date_limit, max_date_limit)) +
    scale_y_continuous(labels = scales::comma, breaks = m_breaks, minor_breaks = m_breaks2, limits = yy_m, expand = c(0,0), sec.axis = sec_axis(~elevation_to_storage(., "mead"), breaks = elevation_to_storage(m_breaks, "mead"), labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01), name = "Storage (maf)")) +
    labs(y = "Pool Elevation (ft)", x = NULL, title = bquote('Lake Mead End-of-Month'~Elevations), subtitle = paste('Projections from', month_heading, '24-Month Study Inflow Scenarios'), caption = shared_caption) +
    # geom_hline(yintercept = c(1025, 1000), colour = 'black', linetype = 'dashed', size = 0.5) +
    # geom_vline(xintercept = mead_reference_lines, size = 0.7, color = "#ffdc70", alpha = 0.8) +
    geom_vline(xintercept = as.yearmon(lubridate::ym(run_date) %m-% months(1)), size = 0.5, color = "black") +
    annotate("text", x = as.yearmon(ym(run_date) - months(5)), colour = 'black',
             y=997, label="Elevation 1,000 ft", angle=00, size=3, hjust = 0) +
    annotate("text", x = as.yearmon(ym(run_date) - months(5)), colour = 'black',
             y=1022, label="Elevation 1,025 ft", angle=00, size=3, hjust = 0) +
    annotate("text", x = as.yearmon(ym(run_date) - months(5)), colour = 'black',
             y=1032, label="Elevation 1,035 ft", angle=00, size=3, hjust = 0) +
    annotate("text", x = as.yearmon(ym(run_date) - months(5)), colour = 'black',
             y=1007, label="Elevation 1,010 ft", angle=00, size=3, hjust = 0) +
    guides(
      color = guide_legend(
        nrow = 2, byrow = TRUE,
        override.aes = list(
          shape = unname(legend_circle_shape[legend_order_keys]),
          label = unname(legend_circle_label[legend_order_keys]),
          alpha = 1
        )
      ),
      linetype = guide_legend(nrow = 2, byrow = TRUE),
      size = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    # theme_bw(base_size = 10) +
    # theme(legend.position = "bottom", plot.title = element_text(face="bold", hjust = 0.5, size = 13), plot.subtitle = element_text(hjust = 0.5, size = 11), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.caption = element_text(hjust = 0, size = 7, face = "italic"))
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.justification = "left",   # <-- left-aligns the legend block
      legend.box.spacing = unit(0.8, "cm"),          # gap between panel/x-axis and legend
      legend.margin = margin(t = 4, b = 6),          # extra padding above/below legend items
      plot.title = element_text(face="bold", hjust = 0.5, size = 13),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, margin = margin(t = 4)),
      plot.caption = element_text(hjust = 0, size = 7, face = "italic", margin = margin(t = 10)),
      plot.margin = margin(t = 10, r = 10, b = 8, l = 5)
    )

  progress("Rendering Lake Mead outputs...", 0.82)
    print("Saving individual Mead plot...")
  mead_png <- file.path(output_dir, "Mead24MS.png")
    mead_pdf <- file.path(output_dir, "Mead24MS.pdf")
    ggsave(mead_png, plot = gg_m, width = 11, height = 8.5, dpi = 300)
  tryCatch({
      logo_raw <- image_read(file.path("data","BofR-vert.png"))
      m_img <- image_read(mead_png)
      final_m <- image_composite(m_img, image_resize(logo_raw,"325"), gravity = "southeast", offset = "+30+100")
      image_write(final_m, mead_png)
    
      #Set density to 300x300 so PDF page size comes out correct (11 x 8.5 in)
      image_write(final_m, mead_pdf, format = "pdf", density="300x300")
  }, error = function(e) { 
    message("Could not apply logo to individual Mead plot. Saving plot without logo instead.") 
    message("Actual error: ", conditionMessage(e))
    ggsave(mead_pdf, plot = gg_m, width = 11, height = 8.5, device = "pdf")
  })


  # Verify the four promised outputs were produced.
  output_files <- c(powell_png, powell_pdf, mead_png, mead_pdf)
  missing_outputs <- output_files[!file.exists(output_files)]
  if (length(missing_outputs) > 0) {
    stop(
      "Plot generation finished, but these output files are missing: ",
      paste(basename(missing_outputs), collapse = ", "),
      call. = FALSE
    )
  }

  progress("Complete", 1)

  invisible(list(
    output_dir = output_dir,
    files = list(
      powell_png = powell_png,
      powell_pdf = powell_pdf,
      mead_png = mead_png,
      mead_pdf = mead_pdf
    ),
    labels = lab_names,
    study_month = run_date,
    end_month = end_date,
    most_6_enabled = most_6_enabled,
    most_7_enabled = most_7_enabled,
    powell_ylim = yy,
    mead_ylim = yy_m,
    data_source_used = source_used,
    month_heading = month_heading
  ))
}
