options(repos = c(CRAN = "https://cloud.r-project.org"))

source("user_library.R", local = TRUE)
user_lib <- chart24ms_use_user_library()
message("Using writable user R library: ", user_lib)
message("System R library will not be modified: ", R.home("library"))

cran_packages <- c(
  "shiny", "httr", "fs", "tidyr", "dplyr", "ggplot2",
  "lubridate", "scales", "zoo", "magick", "remotes",
  "DBI", "odbc"
)

installed <- rownames(installed.packages(lib.loc = .libPaths()))
missing <- setdiff(cran_packages, installed)

if (length(missing) > 0) {
  message("Installing CRAN packages: ", paste(missing, collapse = ", "))
  install.packages(
    missing,
    lib = user_lib,
    dependencies = NA
  )
} else {
  message("All required CRAN packages are already installed.")
}

# Match the package version used by the original GitHub Actions workflow.
rhdb_ok <- requireNamespace("rhdb", quietly = TRUE) &&
  identical(as.character(utils::packageVersion("rhdb")), "0.3.0")

if (!rhdb_ok) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("The remotes package could not be installed, so rhdb cannot be installed.", call. = FALSE)
  }

  message("Installing rhdb v0.3.0 from BoulderCodeHub/rhdb...")
  remotes::install_github(
    "BoulderCodeHub/rhdb",
    ref = "v0.3.0",
    lib = user_lib,
    upgrade = "never",
    dependencies = NA
  )
} else {
  message("rhdb v0.3.0 is already installed.")
}

required <- c(
  "shiny", "httr", "fs", "tidyr", "dplyr", "ggplot2",
  "lubridate", "scales", "zoo", "magick", "remotes", "rhdb",
  "DBI", "odbc"
)

still_missing <- required[
  !vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(still_missing) > 0) {
  stop(
    "Setup did not complete. Missing package(s): ",
    paste(still_missing, collapse = ", "),
    call. = FALSE
  )
}

message("")
message("Setup complete.")
message("Packages are installed in: ", user_lib)
message("You can now run Start_Chart24MS.bat.")
