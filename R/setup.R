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


f_glue_pander <- function(..., .logcall = sys.call(), .topcall = sys.call(-1), .topenv = parent.frame()) {
  args <- list(...)

  if (length(args) == 0) return("")

  # First argument with glue interpolation
  first_arg <- args[[1]]
  remaining_args <- args[-1]

  result_parts <- character()

  # Handle first argument (string with potential glue syntax)
  if (is.character(first_arg) && grepl("\\{.*\\}", first_arg)) {
    # Use glue for string interpolation
    interpolated <- logger::formatter_glue(first_arg, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
    result_parts <- c(result_parts, interpolated)
  } else if (is.character(first_arg)) {
    # Plain string, no interpolation needed
    result_parts <- c(result_parts, first_arg)
  } else {
    # First arg is an object, format with pander
    formatted <- logger::formatter_pander(first_arg, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
    result_parts <- c(result_parts, formatted)
  }

  # Handle remaining arguments with pander
  if (length(remaining_args) > 0) {
    for (arg in remaining_args) {
      if (is.character(arg) && length(arg) == 1) {
        # Simple string, add as-is
        result_parts <- c(result_parts, arg)
      } else {
        # Complex object, format with pander
        formatted <- logger::formatter_pander(arg, .logcall = .logcall, .topcall = .topcall, .topenv = .topenv)
        result_parts <- c(result_parts, formatted)
      }
    }
  }

  # Combine parts with newlines for readability
  paste(result_parts, collapse = "\n")
}


#' Set up logging for a specific namespace
#'
#' @param namespace The namespace to configure (usually package name)
#' @param console_level string or Log level for console output (NULL is default, to disable console logging)
#' @param file_level string or Log level for file output (NULL is default, to disable file logging)
#' @param formatter Formatter function to use - default is \code{logger::formatter_glue} also available are \code{logger::formatter_pander}
#' @export
setup_namespace_logging <- function(namespace, console_level = NULL,  file_level = NULL,
                                    formatter = f_glue_pander) {
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
  msg <- paste("Logger:", deparse(substitute(formatter)))

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
    msg <- paste(msg, "Console:", attr(console_level, "level"))
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
    msg <- paste(msg, " File:", attr(file_level, "level"))
  }

  # Log successful initialization (only if console logging is enabled)
  if (!is.null(console_level)) {
    logger::log_info(
      sprintf("%s init with %s", namespace, msg),
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


