#' Enable colored output in Windows
#'
#' Sets the crayon.enabled option to TRUE when running on Windows,
#' which enables ANSI color support in R console on Windows.
#'
#' @return NULL invisibly
#' @export
enable_windows_colors <- function() {
  if (.Platform$OS.type == "windows") {
    options(crayon.enabled = TRUE)
  }
  invisible(NULL)
}

#' Get a standard path for the common log file
#'
#' Returns a standardized path for the common log file based on
#' environment variables or default locations. Creates the
#' parent directory if it doesn't exist.
#'
#' @return Character string with the full path to the log file
#' @export
get_common_log_file <- function() {
  # Use R_LOG_DIR environment variable if set, otherwise default to a logs directory in R user folder
  log_dir <- Sys.getenv("R_LOG_DIR", file.path(Sys.getenv("R_USER"), "logs"))

  # Create the directory if it doesn't exist
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

  # Return the full path to the system log file
  file.path(log_dir, "system.log")
}
