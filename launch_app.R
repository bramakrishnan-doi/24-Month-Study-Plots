source("user_library.R", local = TRUE)
user_lib <- chart24ms_use_user_library()

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop(
    "The shiny package is missing from the Chart24MS user library. ",
    "Run Setup_Chart24MS.bat first. Expected library: ", user_lib,
    call. = FALSE
  )
}

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
message("Starting Chart24MS from: ", project_dir)
message("Using R package library: ", user_lib)
message("Close this command window when you are finished using the app.")
shiny::runApp(project_dir, launch.browser = TRUE, host = "127.0.0.1")
