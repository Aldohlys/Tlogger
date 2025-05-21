# In R/setup.R of loggingTools package

#' Set up logging for a specific namespace
#' 
#' @param namespace The namespace to configure (usually package name)
#' @param console_level Log level for console output (NULL to disable console logging)
#' @param file_level Log level for file output (NULL to disable file logging)
#' @param log_file Path to log file, TRUE for default, or NULL to disable
#' @param formatter Formatter function to use
#' @export
setup_namespace_logging <- function(
    namespace,
    console_level = logger::INFO,
    file_level = logger::DEBUG,
    log_file = TRUE,
    formatter = logger::formatter_pander
) {
  # Validate inputs
  if (is.null(namespace) || namespace == "") {
    namespace <- "global"
  }
  
  # Enable Windows colors if needed
  enable_windows_colors()
  
  # CONSOLE LOGGER (index 1)
  # ------------------------
  if (!is.null(console_level)) {
    # Set console formatter
    logger::log_formatter(formatter, namespace = namespace, index = 1)
    
    # Set console layout
    logger::log_layout(custom_colored_layout, namespace = namespace, index = 1)
    
    # Set console appender
    logger::log_appender(logger::appender_console, namespace = namespace, index = 1)
    
    # Set console threshold
    logger::log_threshold(console_level, namespace = namespace, index = 1)
  }
  
  # FILE LOGGER (index 2) - only if file path is provided and file level is not NULL
  # -------------------
  if (!is.null(file_level) && !is.null(log_file)) {
    # If TRUE, use default common log file
    if (identical(log_file, TRUE)) {
      log_file <- get_common_log_file()
    }
    
    # Set file formatter
    logger::log_formatter(formatter, namespace = namespace, index = 2)
    
    # Set file layout
    logger::log_layout(plain_layout, namespace = namespace, index = 2)
    
    # Set file appender
    logger::log_appender(logger::appender_file(log_file), namespace = namespace, index = 2)
    
    # Set file threshold
    logger::log_threshold(file_level, namespace = namespace, index = 2)
  }
  
  # Log successful initialization (only if console logging is enabled)
  if (!is.null(console_level)) {
    logger::log_info(
      sprintf("Logging initialized for namespace '%s'", namespace), 
      namespace = namespace
    )
  }
  
  invisible(NULL)
}

#' Update log threshold for a namespace
#' 
#' @param namespace The namespace to update
#' @param level New log level threshold
#' @param console_only Update only console threshold
#' @param file_only Update only file threshold
#' @export
update_log_level <- function(namespace, level, console_only = FALSE, file_only = FALSE) {
  if (is.null(namespace) || namespace == "") {
    namespace <- "global"
  }
  
  if (!file_only) {
    logger::log_threshold(level, namespace = namespace, index = 1)
  }
  
  if (!console_only) {
    if (logger::log_indices(namespace = namespace) >= 2) {
      logger::log_threshold(level, namespace = namespace, index = 2)
    }
  }
  
  invisible(NULL)
}

#' Get the common log file path 
#' @export
get_common_log_file <- function() {
  log_dir <- Sys.getenv("R_LOG_DIR", file.path(Sys.getenv("R_USER"), "logs"))
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  file.path(log_dir, "system.log")
}

#' Enable colors in Windows
#' @export
enable_windows_colors <- function() {
  if (.Platform$OS.type == "windows") {
    options(crayon.enabled = TRUE)
  }
}

# Include the custom_colored_layout and plain_layout functions as before