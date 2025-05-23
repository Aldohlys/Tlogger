# In R/setup.R of Tlogger package



### internal function for conversion
convert_str_to_level <- function(str_level) {
  switch(str_level,
         "FATAL" = logger::FATAL,
         "ERROR" = logger::ERROR,
         "WARN" = logger::WARN,
         "SUCCESS" = logger::SUCCESS,
         "INFO" = logger::INFO,
         "DEBUG" = logger::DEBUG,
         "TRACE" = logger::TRACE,
         logger::INFO
  )
}


#' Set up logging for a specific namespace
#'
#' @param namespace The namespace to configure (usually package name)
#' @param console_level string or Log level for console output (NULL is default, to disable console logging)
#' @param file_level string or Log level for file output (NULL is default, to disable file logging)
#' @param formatter Formatter function to use - default is \code{logger::formatter_pander}
#' @export
setup_namespace_logging <- function(namespace, console_level = NULL,  file_level = NULL,
                                    formatter = logger::formatter_pander) {
  # Validate inputs
  if (is.null(namespace) || namespace == "") {
    namespace <- "global"
  }

  if (is.null(console_level) && is.null(file_level)) {
    message("No logging for: ", namespace)
    invisible(NULL)
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
    if (is.character(console_level)) console_level <- convert_str_to_level(console_level)
    logger::log_threshold(console_level, namespace = namespace, index = 1)
  }

  # FILE LOGGER (index 2) - only if file level is not NULL
  # -------------------
  if (!is.null(file_level)) {
    log_file <- get_common_log_file()

    # Set file formatter
    logger::log_formatter(formatter, namespace = namespace, index = 2)

    # Set file layout
    logger::log_layout(plain_layout, namespace = namespace, index = 2)

    # Set file appender
    logger::log_appender(logger::appender_file(log_file), namespace = namespace, index = 2)

    # Set file threshold
    if (is.character(file_level)) file_level <- convert_str_to_level(file_level)
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
#' @param level string or log level type, new log level threshold
#' @param console_only Update only console threshold
#' @param file_only Update only file threshold
#' @param update_config will update the YAML file if set - FALSE is default value
#' @export
update_log_level <- function(namespace, level, console_only = FALSE, file_only = FALSE, update_config = FALSE) {

  # Validate parameters
  if (console_only && file_only) {
    stop("console_only and file_only cannot both be TRUE")
  }

  if (is.null(namespace) || namespace == "") {
    namespace <- "global"
  }
  #### level argument is a string like "INFO", "DEBUG",...
  if (is.character(level)) {
    level_logger <- convert_str_to_level(level)
    level_str <- level
  }

  #### level argument is like logger::INFO,...
  if (inherits(level, "loglevel")) {
    level_logger <- level
    level_str <- attr(level, "level")
  }

  if (console_only) {
    logger::log_threshold(level_logger, namespace = namespace, index = 1)
    if (update_config) set_config_namespace(namespace, file_level = NULL, console_level = level_str)
  } else if (file_only) {
    if (logger::log_indices(namespace = namespace) >= 2) {
      logger::log_threshold(level_logger, namespace = namespace, index = 2)
      if (update_config) set_config_namespace(namespace, file_level = level_str, console_level = NULL)
    }
  } else {
    # Default: update both
    logger::log_threshold(level_logger, namespace = namespace, index = 1)
    if (logger::log_indices(namespace = namespace) >= 2) {
      logger::log_threshold(level_logger, namespace = namespace, index = 2)
    }
    if (update_config) set_config_namespace(namespace, file_level = level_str, console_level = level_str)
  }


  invisible(NULL)
}


