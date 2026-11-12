# Local Shiny interface for Chart24MS

required_app_packages <- c("shiny")
missing_app_packages <- required_app_packages[
  !vapply(required_app_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_app_packages) > 0) {
  stop("The shiny package is not installed. Run Setup_Chart24MS.bat first.", call. = FALSE)
}

library(shiny)

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(project_dir, "generate_plots.R"), local = FALSE)

# Smart defaults, computed when the app starts.
#   - Study month defaults to the current month.
#   - Probable Minimum and both Most Probable run months default to the study month.
#   - Probable Maximum (quarterly: Jan/Apr/Aug/Oct) defaults to the most recent
#     of those four months on or before the study month.
default_study_month <- format(Sys.Date(), "%Y-%m")
default_max7_month <- chart24ms_latest_max7_run_month(default_study_month)

# Month/year picker. Built on shiny::dateInput, with the calendar limited to
# a month grid (click the year in the header to jump between years). The
# widget returns the first day of the chosen month; month_value() below turns
# it into the "YYYY-MM" text that generate_plots() expects.
#   value    "YYYY-MM" text, or NA for an empty box
#   optional adds a Clear button to the picker
month_picker <- function(input_id, label, value = NA, optional = FALSE) {
  start <- if (is.na(value)) NA else as.Date(paste0(value, "-01"))
  picker <- dateInput(
    input_id,
    label,
    value = start,
    format = "yyyy-mm",
    startview = "year",
    autoclose = TRUE,
    width = "100%"
  )
  # Find the text <input> inside the widget (at any depth) and set the
  # bootstrap-datepicker options on it.
  # Only HTML tags (and tag lists) are visited. Anything else, such as the
  # html_dependency objects that load the picker's JavaScript/CSS, is returned
  # untouched; rewriting those makes them print on the page as text.
  set_month_mode <- function(x) {
    if (inherits(x, "shiny.tag")) {
      if (identical(x$name, "input")) {
        x$attribs[["data-date-min-view-mode"]] <- "months"
        x$attribs[["data-date-max-view-mode"]] <- "years"
        x$attribs[["placeholder"]] <- "Select month"
        if (optional) x$attribs[["data-date-clear-btn"]] <- "true"
      } else if (length(x$children) > 0) {
        for (i in seq_along(x$children)) {
          x$children[i] <- list(set_month_mode(x$children[[i]]))
        }
      }
    } else if (inherits(x, "shiny.tag.list")) {
      for (i in seq_along(x)) {
        x[i] <- list(set_month_mode(x[[i]]))
      }
    }
    x
  }
  picker <- set_month_mode(picker)
  picker
}

# Picker value -> "YYYY-MM" text ("" when the box is empty).
month_value <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("")
  format(as.Date(x[1]), "%Y-%m")
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body { background: #f5f6f7; }\n      .container-fluid { max-width: 1240px; margin: 0 auto; }\n      .app-title { margin: 24px 0 6px 0; font-weight: 650; }\n      .app-subtitle { margin-bottom: 20px; color: #555; }\n      .control-card, .preview-card {\n        background: white; border: 1px solid #ddd; border-radius: 8px;\n        padding: 18px; margin-bottom: 18px;\n      }\n      .trace-row { border-top: 1px solid #eee; padding-top: 12px; margin-top: 8px; }\n      .trace-row:first-of-type { border-top: 0; }\n      .status-ready { padding: 12px; background: #f2f5f7; border-radius: 6px; margin-bottom: 12px; }\n      .btn-generate { width: 100%; font-weight: 600; margin-top: 8px; }\n      /* Keep previews at the same 11 x 8.5 aspect ratio as the generated plots. */\n      .plot-preview { width: 100%; aspect-ratio: 11 / 8.5; position: relative; }\n      .plot-preview .shiny-image-output { width: 100% !important; height: 100% !important; }\n      .plot-preview img { display: block; width: 100% !important; height: 100% !important; object-fit: contain; }\n      .download-row .btn { margin: 4px 6px 4px 0; }\n      code { color: #333; background: #f2f2f2; }\n    "))
  ),

  h2("Mead & Powell 24-Month Study Plot Generator", class = "app-title"),
  div(
    "Enter the monthly HDB run information, then generate the Lake Powell and Lake Mead PNG/PDF outputs locally.",
    class = "app-subtitle"
  ),

  fluidRow(
    column(
      width = 4,
      div(
        class = "control-card",
        h4("Monthly inputs"),
        month_picker("run_date", "Study month", default_study_month),
        helpText("Pick a month from the calendar. Plot labels are created automatically from these months."),
        month_picker("end_month", "Plot end month (optional)", NA, optional = TRUE),
        uiOutput("end_month_help"),

        div(class = "trace-row",
          h5("Probable Minimum - 6.0 maf"),
          fluidRow(
            column(6, numericInput("min_6_mrid", "MRID", value = 3332, min = 1, step = 1)),
            column(6, month_picker("min_6_run_date", "Run month", default_study_month))
          )
        ),

        div(class = "trace-row",
          h5("Most Probable - 6.0 maf"),
          checkboxInput("most_6_enabled", "Include this scenario", value = TRUE),
          conditionalPanel(
            condition = "input.most_6_enabled",
            fluidRow(
              column(6, numericInput("most_6_mrid", "MRID", value = 3331, min = 1, step = 1)),
              column(6, month_picker("most_6_run_date", "Run month", default_study_month))
            )
          )
        ),

        div(class = "trace-row",
          h5("Most Probable - 7.0 maf"),
          checkboxInput("most_7_enabled", "Include this scenario", value = TRUE),
          conditionalPanel(
            condition = "input.most_7_enabled",
            fluidRow(
              column(6, numericInput("most_7_mrid", "MRID", value = 3333, min = 1, step = 1)),
              column(6, month_picker("most_7_run_date", "Run month", default_study_month))
            )
          )
        ),

        div(class = "trace-row",
          h5("Probable Maximum - 7.0 maf"),
          fluidRow(
            column(6, numericInput("max_7_mrid", "MRID", value = 3328, min = 1, step = 1)),
            column(6, month_picker("max_7_run_date", "Run month", default_max7_month))
          )
        ),

        div(class = "trace-row",
          div(
            style = "display: flex; justify-content: space-between; align-items: baseline;",
            h5("Y-Axis Elevation Range (ft)", style = "margin-bottom: 6px;"),
            actionLink("reset_ylim", "Reset defaults", style = "font-size: 12px;")
          ),
          div(style = "font-weight: 600; font-size: 12px; color: #555; margin-bottom: 4px;", "Lake Powell"),
          fluidRow(
            column(6, numericInput("powell_y_min", "Min (ft)", value = 3400, step = 25)),
            column(6, numericInput("powell_y_max", "Max (ft)", value = 3625, step = 25))
          ),
          div(style = "font-weight: 600; font-size: 12px; color: #555; margin-bottom: 4px;", "Lake Mead"),
          fluidRow(
            column(6, numericInput("mead_y_min", "Min (ft)", value = 950, step = 25)),
            column(6, numericInput("mead_y_max", "Max (ft)", value = 1100, step = 25))
          )
        ),

        div(class = "trace-row",
          h5("HDB Data Source"),
          selectInput(
            "data_source",
            "Connection mode",
            choices = c(
              "Auto (HDB API, fallback to ODBC)" = "auto",
              "Direct ODBC (DSN vblchdb)" = "odbc",
              "HDB HTTP API only" = "api"
            ),
            selected = "auto",
            width = "100%"
          ),
          uiOutput("odbc_session_ui")
        ),

        actionButton("generate", "Generate Plots", class = "btn-primary btn-generate"),
        br(), br(),
        actionButton("open_folder", "Open Output Folder", class = "btn-default", width = "100%")
      ),

      div(
        class = "control-card",
        h4("Status"),
        uiOutput("status"),
        conditionalPanel(
          condition = "output.hasResults === true",
          div(class = "download-row",
            downloadButton("download_powell_png", "Powell PNG"),
            downloadButton("download_powell_pdf", "Powell PDF"),
            downloadButton("download_mead_png", "Mead PNG"),
            downloadButton("download_mead_pdf", "Mead PDF")
          )
        )
      )
    ),

    column(
      width = 8,
      div(
        class = "preview-card",
        h4("Lake Powell preview"),
        uiOutput("powell_placeholder"),
        conditionalPanel(
          condition = "output.hasResults === true",
          div(
            class = "plot-preview",
            imageOutput("powell_preview", width = "100%", height = "100%")
          )
        )
      ),
      div(
        class = "preview-card",
        h4("Lake Mead preview"),
        uiOutput("mead_placeholder"),
        conditionalPanel(
          condition = "output.hasResults === true",
          div(
            class = "plot-preview",
            imageOutput("mead_preview", width = "100%", height = "100%")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  result <- reactiveVal(NULL)
  status_message <- reactiveVal("Ready. Enter the monthly inputs and click Generate Plots.")
  status_type <- reactiveVal("ready")
  odbc_session_tick <- reactiveVal(0L)
  odbc_modal_error <- reactiveVal(NULL)

  output$hasResults <- reactive(!is.null(result()))
  outputOptions(output, "hasResults", suspendWhenHidden = FALSE)

  output$odbc_session_ui <- renderUI({
    odbc_session_tick()
    st <- hdb_session_status()
    if (isTRUE(st$available)) {
      div(
        style = "font-size: 12px; color: #2b542c; margin-bottom: 6px;",
        tags$span(
          paste0("ODBC credentials cached in memory (", st$username, " @ ", st$dsn, "). ")
        ),
        actionLink("clear_odbc_creds", "Clear")
      )
    } else {
      helpText("If the HDB API is down, you will be prompted for your HDB username and password to query DSN 'vblchdb' directly.")
    }
  })

  observeEvent(input$clear_odbc_creds, {
    hdb_session_clear()
    odbc_session_tick(odbc_session_tick() + 1L)
    showNotification("Cleared cached HDB ODBC credentials from session memory.", type = "message", duration = 4)
  })

  observeEvent(input$reset_ylim, {
    updateNumericInput(session, "powell_y_min", value = 3400)
    updateNumericInput(session, "powell_y_max", value = 3625)
    updateNumericInput(session, "mead_y_min",   value = 950)
    updateNumericInput(session, "mead_y_max",   value = 1100)
  })

  show_odbc_credentials_modal <- function(reason_text = NULL) {
    st <- hdb_session_status()
    err_msg <- odbc_modal_error()
    showModal(modalDialog(
      title = "HDB Direct Database Login (ODBC Fallback)",
      if (!is.null(reason_text) && nzchar(reason_text)) {
        div(class = "alert alert-warning", style = "margin-bottom: 12px;", reason_text)
      },
      if (!is.null(err_msg) && nzchar(err_msg)) {
        div(class = "alert alert-danger", style = "margin-bottom: 12px;", err_msg)
      },
      p("Enter your HDB database credentials to query HDB directly through the local 64-bit ODBC DSN."),
      textInput("odbc_modal_dsn", "ODBC DSN", value = st$dsn, width = "100%"),
      textInput("odbc_modal_user", "HDB Username", value = st$username, width = "100%"),
      passwordInput("odbc_modal_pass", "HDB Password", value = "", width = "100%"),
      helpText("Credentials are kept only in R process memory for this session and are never written to disk."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("odbc_modal_submit", "Connect & Generate Plots", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  }

  run_plot_generation <- function(force_source = NULL, odbc_user = NULL, odbc_pass = NULL, odbc_dsn = NULL) {
    status_type("ready")
    status_message("Generating plots...")

    chosen_source <- if (!is.null(force_source)) force_source else input$data_source
    if (is.null(chosen_source) || !nzchar(chosen_source)) chosen_source <- "auto"

    tryCatch({
      generated <- withProgress(message = "Generating 24-Month Study plots", value = 0, {
        generate_plots(
          run_date = month_value(input$run_date),
          min_6_mrid = input$min_6_mrid,
          most_6_mrid = input$most_6_mrid,
          most_7_mrid = input$most_7_mrid,
          max_7_mrid = input$max_7_mrid,
          min_6_run_date = month_value(input$min_6_run_date),
          most_6_run_date = month_value(input$most_6_run_date),
          most_7_run_date = month_value(input$most_7_run_date),
          max_7_run_date = month_value(input$max_7_run_date),
          most_6_enabled = input$most_6_enabled,
          most_7_enabled = input$most_7_enabled,
          end_month = month_value(input$end_month),
          powell_y_min = input$powell_y_min,
          powell_y_max = input$powell_y_max,
          mead_y_min = input$mead_y_min,
          mead_y_max = input$mead_y_max,
          data_source = chosen_source,
          odbc_username = odbc_user,
          odbc_password = odbc_pass,
          odbc_dsn = if (!is.null(odbc_dsn) && nzchar(odbc_dsn)) odbc_dsn else "vblchdb",
          project_dir = project_dir,
          progress_callback = function(message, value) {
            if (!is.null(value)) setProgress(value = value, detail = message)
          }
        )
      })

      odbc_session_tick(odbc_session_tick() + 1L)
      result(generated)
      status_type("success")
      via_label <- if (identical(generated$data_source_used, "odbc")) {
        " (downloaded via direct HDB ODBC connection)"
      } else {
        " (downloaded via HDB HTTP API)"
      }
      status_message(paste0(
        "Complete", via_label, ". Four files were saved to: ", generated$output_dir
      ))
    }, chart24ms_odbc_auth_needed = function(e) {
      result(NULL)
      status_type("ready")
      status_message("Waiting for HDB ODBC credentials...")
      odbc_modal_error(NULL)
      show_odbc_credentials_modal(reason_text = conditionMessage(e))
    }, error = function(e) {
      result(NULL)
      status_type("error")
      status_message(conditionMessage(e))
    })
  }

  output$status <- renderUI({
    msg <- status_message()
    type <- status_type()
    if (identical(type, "error")) {
      div(class = "alert alert-danger", strong("Error: "), msg)
    } else if (identical(type, "success")) {
      div(class = "alert alert-success", msg)
    } else {
      div(class = "status-ready", msg)
    }
  })

  output$end_month_help <- renderUI({
    study <- month_value(input$run_date)
    if (nzchar(study)) {
      helpText(paste0(
        "Leave blank for the full 24 months (through ",
        chart24ms_add_months(study, 23),
        "). Enter an earlier month to shorten the plot."
      ))
    } else {
      helpText("Leave blank for the full 24 months, or enter an earlier month to shorten the plot.")
    }
  })

  # Smart defaults: when the study month changes, carry it onto the Probable
  # Minimum and both Most Probable run months, and set the Probable Maximum
  # run month to the most recent quarterly month (Jan/Apr/Aug/Oct) on or
  # before it. The user can still edit any run month afterward.
  observeEvent(input$run_date, {
    study <- month_value(input$run_date)
    if (!nzchar(study)) return()
    study_date <- as.Date(paste0(study, "-01"))
    updateDateInput(session, "min_6_run_date", value = study_date)
    updateDateInput(session, "most_6_run_date", value = study_date)
    updateDateInput(session, "most_7_run_date", value = study_date)
    updateDateInput(
      session, "max_7_run_date",
      value = as.Date(paste0(chart24ms_latest_max7_run_month(study), "-01"))
    )
  }, ignoreInit = TRUE)

  # Only offer end months inside the study window (study month .. +23 months).
  observeEvent(input$run_date, {
    study <- month_value(input$run_date)
    if (!nzchar(study)) return()
    last_month <- chart24ms_add_months(study, 23)
    last_day <- seq(as.Date(paste0(last_month, "-01")), by = "month", length.out = 2)[2] - 1
    updateDateInput(
      session, "end_month",
      min = as.Date(paste0(study, "-01")),
      max = last_day
    )
  })

  output$powell_placeholder <- renderUI({
    if (is.null(result())) p("Generate the plots to display a preview.", style = "color:#777;")
  })
  output$mead_placeholder <- renderUI({
    if (is.null(result())) p("Generate the plots to display a preview.", style = "color:#777;")
  })

  observeEvent(input$generate, {
    run_plot_generation()
  })

  observeEvent(input$odbc_modal_submit, {
    u <- trimws(as.character(input$odbc_modal_user))
    p <- as.character(input$odbc_modal_pass)
    d <- trimws(as.character(input$odbc_modal_dsn))
    if (!nzchar(u) || !nzchar(p)) {
      odbc_modal_error("Please enter both your HDB username and password.")
      show_odbc_credentials_modal()
      return()
    }
    removeModal()
    odbc_modal_error(NULL)
    run_plot_generation(
      force_source = "odbc",
      odbc_user    = u,
      odbc_pass    = p,
      odbc_dsn     = if (nzchar(d)) d else "vblchdb"
    )
  })

  output$powell_preview <- renderImage({
    req(result())
    list(src = result()$files$powell_png, contentType = "image/png", alt = "Lake Powell plot")
  }, deleteFile = FALSE)

  output$mead_preview <- renderImage({
    req(result())
    list(src = result()$files$mead_png, contentType = "image/png", alt = "Lake Mead plot")
  }, deleteFile = FALSE)

  make_download <- function(file_key, content_type) {
    downloadHandler(
      filename = function() {
        req(result())
        basename(result()$files[[file_key]])
      },
      contentType = content_type,
      content = function(file) {
        req(result())
        file.copy(result()$files[[file_key]], file, overwrite = TRUE)
      }
    )
  }

  output$download_powell_png <- make_download("powell_png", "image/png")
  output$download_powell_pdf <- make_download("powell_pdf", "application/pdf")
  output$download_mead_png <- make_download("mead_png", "image/png")
  output$download_mead_pdf <- make_download("mead_pdf", "application/pdf")

  observeEvent(input$open_folder, {
    target <- if (!is.null(result())) {
      result()$output_dir
    } else {
      file.path(project_dir, "output")
    }

    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    target <- normalizePath(target, winslash = "\\", mustWork = TRUE)

    if (.Platform$OS.type == "windows") {
      # shell.exec() can silently fail for folders on some Windows/R setups.
      # Launch Explorer directly instead.
      tryCatch({
        windows_dir <- Sys.getenv("WINDIR", unset = "C:\\Windows")
        explorer <- file.path(windows_dir, "explorer.exe")
        if (!file.exists(explorer)) explorer <- "explorer.exe"

        system2(
          explorer,
          args = shQuote(target),
          wait = FALSE,
          stdout = FALSE,
          stderr = FALSE
        )

        showNotification(
          paste("Opened output folder:", target),
          type = "message",
          duration = 5
        )
      }, error = function(e) {
        showNotification(
          paste0(
            "Windows could not open the output folder. Folder: ",
            target,
            " | Error: ",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      })
    } else {
      showNotification(paste("Output folder:", target), type = "message", duration = 8)
    }
  })
}

shinyApp(ui, server)
