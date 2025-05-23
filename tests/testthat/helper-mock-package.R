# In tests/testthat/helper-mock-package.R

#' Create a mock package for testing
#'
#' @param pkg_name Name of the mock package
#' @return List with package environment and functions
create_test_package <- function(pkg_name) {
  # Create package environment
  pkg_env <- new.env(parent = .GlobalEnv)

  # Add logging utilities
  pkg_env$init_logging <- function(
    console_level = logger::INFO,
    file_level = logger::DEBUG,
    log_file = TRUE)

  {
    Tlogger::setup_namespace_logging(
        namespace = pkg_name,
        console_level = console_level,
        file_level = file_level
      )
  }

  # Add convenience wrappers
  pkg_env$log_info <- function(msg, ...) {
    logger::log_info(msg, ..., namespace = pkg_name)
  }

  pkg_env$log_debug <- function(msg, ...) {
    logger::log_debug(msg, ..., namespace = pkg_name)
  }

  pkg_env$log_warn <- function(msg, ...) {
    logger::log_warn(msg, ..., namespace = pkg_name)
  }

  pkg_env$log_error <- function(msg, ...) {
    logger::log_error(msg, ..., namespace = pkg_name)
  }

  # Sample package function that logs
  pkg_env$sample_function <- function(x) {
    pkg_env$log_info(paste("Processing input:", x))
    result <- x * 2
    pkg_env$log_debug(paste("Result calculated:", result))
    return(result)
  }

  # Initialize logging
  pkg_env$init_logging()

  # Return the mock package environment
  return(pkg_env)
}
