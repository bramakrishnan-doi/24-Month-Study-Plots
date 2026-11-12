# Select a writable per-user R package library.
# This avoids trying to install packages under C:/Program Files/R/..., which
# normally requires Administrator permissions on Windows.

chart24ms_use_user_library <- function() {
  minor_part <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  r_series <- paste(R.version$major, minor_part, sep = ".")

  if (.Platform$OS.type == "windows") {
    local_app_data <- Sys.getenv("LOCALAPPDATA", unset = "")
    if (nzchar(local_app_data)) {
      user_lib <- file.path(local_app_data, "R", "win-library", r_series)
    } else {
      user_lib <- file.path(path.expand("~"), "R", "win-library", r_series)
    }
  } else {
    user_lib <- file.path(path.expand("~"), "R", paste0("library-", r_series))
  }

  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)

  if (!dir.exists(user_lib)) {
    stop("Could not create the user R package library: ", user_lib, call. = FALSE)
  }

  # Confirm the directory is writable before attempting any package installs.
  probe <- tempfile(pattern = "chart24ms-write-test-", tmpdir = user_lib)
  writable <- tryCatch({
    ok <- file.create(probe)
    if (isTRUE(ok)) unlink(probe)
    isTRUE(ok)
  }, error = function(e) FALSE)

  if (!writable) {
    stop(
      "The user R package library is not writable: ", user_lib,
      ". Check your Windows permissions or contact your IT administrator.",
      call. = FALSE
    )
  }

  Sys.setenv(R_LIBS_USER = user_lib)
  .libPaths(unique(c(user_lib, .libPaths())))
  user_lib
}
