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
  file.path(log_dir, paste0("t-", format(Sys.Date(),"%Y%m%d"),".log"))
}


### Internal function for easier testing
read_config <- function() {
  cfg <- config::get(config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
                     file = Sys.getenv("R_CONFIG_FILE", "config.yml"))
  return(cfg)
}


### Internal for easier testing
### This will overwrite current config.yml file
### In case any non-default config - will be then lost
write_config <- function(config) {
  cfg <- list(default = config)
  yaml::write_yaml(cfg, Sys.getenv("R_CONFIG_FILE"))
  return(Sys.getenv("R_CONFIG_FILE"))
}

#' Get common logger settings
#'
#' Returns a list comprised of \code{level} default threshold and log file directory \code{log_dir}.
#'
#' If no config found display a message and returns NULL
#'
#' @return a list including level and log_dir strings
#' @export
get_config <- function() {
  log_config <- read_config()
  if (is.null(log_config)) {
    message("No YAML config found!")
    return(NULL)
  }
  else return(list(level=log_config$logging$level, log_dir=log_config$logging$log_dir))
}

#' Get namespace logger settings
#'
#' Returns a list comprised of alll package specific settings, e.g. \code{file_level} current threshold and
#'   \code{console_level} threshold, or any other existing settings.
#'
#' If no config found display a message and returns NULL
#'
#' @return a list including all package_specific settings
#' @export
get_config_namespace <- function(namespace = NULL) {
  ### Read config file
  log_config <- read_config()

  if (is.null(log_config)) {
    message("No YAML logging config found!")
    return(NULL)
  }
  else {
    pkg_specific <- log_config$logging$package_specific[[namespace]]
    return(pkg_specific)
  }
}



#' Get namespace logger settings
#'
#' Returns a list comprised of all package specific settings, e.g. \code{file_level} current threshold and
#'   \code{console_level} threshold, or any other existing settings.
#'
#' If no config found display a message and returns NULL
#'
#' @return a list including all package_specific settings
#' @export
set_config_namespace <- function(namespace = NULL, file_level = NULL, console_level = NULL) {
  log_config <-  read_config()

  if (!is.null(file_level)) log_config$logging$package_specific[[namespace]]$file_level = file_level
  if (!is.null(console_level)) log_config$logging$package_specific[[namespace]]$console_level = console_level

  invisible(write_config(log_config))
}

