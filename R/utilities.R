#' Identify logger function
#'
#' Necessary for namespace_summary function only
#' @noRd
#' @keywords internal
#' @param func formatter function to identify
#' @return NULL invisibly
identify_logger_function <- function(func) {
  # 1. Check generator first
  generator <- attr(func, "generator")
  if (!is.null(generator)) {
    return(as.character(generator))
  }

  # 2. Get environment info
  env_name <- environmentName(environment(func))

  # 3. Handle standard logger namespace
  if (!is.null(env_name) && env_name == "logger") {
    tryCatch({
      logger_ns <- asNamespace("logger")
      logger_names <- ls(logger_ns, all.names = TRUE)

      for (name in logger_names) {
        logger_func <- get(name, logger_ns)
        if (is.function(logger_func) && identical(func, logger_func)) {
          return(name)
        }
      }
      return("logger_function")
    }, error = function(e) {
      return("logger_function")
    })
  }

  # 4. Enhanced Tlogger namespace handling
  if (!is.null(env_name) && env_name == "Tlogger") {
    tryCatch({
      # First try exact matching
      tlogger_ns <- asNamespace("Tlogger")
      all_names <- ls(tlogger_ns, all.names = TRUE)

      for (name in all_names) {
        tlogger_func <- get(name, tlogger_ns)
        if (is.function(tlogger_func) && identical(func, tlogger_func)) {
          return(paste0(name, "()"))
        }
      }

      # If exact match fails, use code pattern matching
      func_body <- deparse(body(func))
      func_body_str <- paste(func_body, collapse = " ")

      # Pattern matching for custom_colored_layout
      if (grepl("crayon::", func_body_str) &&
          grepl("bold_color_fn", func_body_str) &&
          grepl("timestamp_c.*level_c.*metadata_c", func_body_str)) {
        return("custom_colored_layout()")
      }

      # Pattern matching for plain_layout
      if (grepl("sprintf.*%s %s %s : %s", func_body_str) &&
          !grepl("crayon::", func_body_str)) {
        return("plain_layout()")
      }

      # Check if function signature matches known patterns
      func_args <- names(formals(func))
      expected_args <- c("level", "msg", "namespace", ".logcall", ".topcall", ".topenv", ".timestamp")

      if (length(intersect(func_args, expected_args)) == length(expected_args)) {
        # Has the right signature - try to determine which one
        if (grepl("crayon", func_body_str)) {
          return("custom_colored_layout()")
        } else {
          return("plain_layout()")
        }
      }

      return("Tlogger_custom_function()")
    }, error = function(e) {
      return("Tlogger_function()")
    })
  }

  return("unknown_function()")
}

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

#' @noRd
#' @keywords internal
#' @return as per config, substring of config.yml file that fits with config configuration
read_config <- function() {
  cfg <- config::get(config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
                     file = Sys.getenv("R_CONFIG_FILE", "config.yml"))
  return(cfg)
}


### Internal for easier testing
### This will overwrite current config.yml file
### In case any non-default config - will be then lost
# write_config <- function(config) {
#
#   config_file = Sys.getenv("R_CONFIG_FILE")
#
#   ### Make a copy of the current config file
#   file.copy(from=config_file, to=file.path(Sys.getenv("R_USER"), "config.old"), overwrite=TRUE)
#
#   ### Add default as config package removes it
#   ### THis is tricky as config package could also read other tags, like production
#   ### or development which is R_CONFIG_ACTIVE value
#   cfg$default <- config
#
#   ### Rewrite into config_file after adding config string in it
#   yaml::write_yaml(cfg, config_file)
#   cat(readLines(config_file), sep="\n")
#   return(config_file)
# }

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
#' @param namespace string namespace to update. i.e. package name
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


#' Inspect namespace logger settings
#'
#' Returns a list comprised of all package specific settings, e.g. \code{file_level} current threshold and
#'   \code{console_level} threshold, or any other existing settings.
#'
#' If no config found display a message and returns NULL
#'
#' @return a list including all package_specific settings: \code{namespace, index, threshold, formatter, layout,
#' appender}
#' @param namespace string namespace to update. i.e. package name
#' @param index integer - index within namespace
#' @export
get_namespace_summary <- function(namespace = "global", index = 1) {

  ### logger:::namespaces - this will return an environment
  configs <- get(namespace, envir = logger:::namespaces)

  if (index > length(configs)) {
    stop(sprintf("Index %d not found in namespace '%s'", index, namespace))
  }

  config <- configs[[index]]

  list(
    namespace = namespace,
    index = index,
    threshold = as.character(config$threshold),
    formatter = identify_logger_function(config$formatter),
    layout = identify_logger_function(config$layout),
    appender = identify_logger_function(config$appender)
  )
}

#' Get summary of all logger configurations
#'
#' This will look at all namespaces registered into logger library.
#' Then it will list for each namespace and index: threshold, formatter, layout and appender.
#' Notice that as per Tlogger calls sequence to logger, index = 1 is for Console and index = 2 for File
#'
#' @return data.frame with all namespace configurations - for each namespace, list also indexes.
#' @export
summarize_all_loggers <- function() {
  namespaces <- logger::log_namespaces()

  results <- data.frame(
    namespace = character(),
    index = integer(),
    threshold = character(),
    formatter = character(),
    layout = character(),
    appender = character(),
    stringsAsFactors = FALSE
  )

  for (ns in namespaces) {
    # Get number of configs in this namespace
    # Triple colon (:::) - internal/private objects
    configs <- get(ns, envir = logger:::namespaces)

    for (i in seq_along(configs)) {
      tryCatch({
        summary <- get_namespace_summary(ns, i)
        results <- rbind(results, data.frame(summary, stringsAsFactors = FALSE))
      }, error = function(e) {
        # Skip if index doesn't exist
      })
    }
  }

  results
}

