#' Custom colored layout for console output
#'
#' Creates a colored log layout with the format:
#' "time level \[R\] namespace : msg"
#' where level is displayed in bold color.
#'
#' @param level Log level object
#' @param msg Log message
#' @param namespace The namespace (package) generating the log (defaults to "global")
#' @param .logcall The logging call being evaluated
#' @param .topcall R expression from which the logging function was called
#' @param .topenv Original frame of the .topcall calling function
#' @param .timestamp The timestamp when logging occurred
#'
#' @return Formatted log string with ANSI color codes
#' @export
custom_colored_layout <- function(level,
                                  msg,
                                  namespace = NA_character_,
                                  .logcall = sys.call(),
                                  .topcall = sys.call(-1),
                                  .topenv = parent.frame(),
                                  .timestamp = Sys.time()) {
  # Format timestamp without milliseconds
  timestamp <- format(.timestamp, "%Y-%m-%d %H:%M:%S")
  # Get level as string from attribute
  level_str <- attr(level, "level")
  # Use namespace or default to global
  ns <- ifelse(is.na(namespace) || namespace == "", "global", namespace)
  # Build metadata string with namespace
  metadata <- sprintf("[R] (%s)", ns)

  # Define color function based on level
  color_fn <- switch(level_str,
                     "INFO" = crayon::green,
                     "SUCCESS" = crayon::green,
                     "WARN" = crayon::yellow,
                     "ERROR" = crayon::red,
                     "DEBUG" = crayon::cyan,
                     "TRACE" = crayon::silver,
                     "FATAL" = crayon::magenta,
                     function(x) x  # Default - no coloring
  )

  # Define bold color function for the level
  bold_color_fn <- switch(level_str,
                          "INFO" = crayon::green$bold,
                          "SUCCESS" = crayon::green$bold,
                          "WARN" = crayon::yellow$bold,
                          "ERROR" = crayon::red$bold,
                          "DEBUG" = crayon::cyan$bold,
                          "TRACE" = crayon::silver$bold,
                          "FATAL" = crayon::magenta$bold,
                          function(x) x  # Default - no coloring
  )

  # Format the prefix parts
  timestamp_c <- color_fn(timestamp)
  level_c <- bold_color_fn(level_str)
  metadata_c <- color_fn(metadata)
  paste(timestamp_c, level_c, metadata_c, ":", msg)
}

#' Plain text layout for file output
#'
#' Creates a plain text log layout with the format:
#' "{time} {level} [R] {namespace} : {msg}"
#' without ANSI color codes, suitable for log files.
#'
#' @param level Log level object
#' @param msg Log message
#' @param namespace The namespace (package) generating the log (defaults to "global")
#' @param .logcall The logging call being evaluated
#' @param .topcall R expression from which the logging function was called
#' @param .topenv Original frame of the .topcall calling function
#' @param .timestamp The timestamp when logging occurred
#'
#' @return Formatted log string without color codes
#' @export
plain_layout <- function(level,
                         msg,
                         namespace = NA_character_,
                         .logcall = sys.call(),
                         .topcall = sys.call(-1),
                         .topenv = parent.frame(),
                         .timestamp = Sys.time()) {
  # Format timestamp without milliseconds
  timestamp <- format(.timestamp, "%Y-%m-%d %H:%M:%S")
  # Get level as string from attribute
  level_str <- attr(level, "level")
  # Use namespace or default to global
  ns <- ifelse(is.na(namespace) || namespace == "", "global", namespace)
  # Build metadata string with namespace
  metadata <- sprintf("[R] (%s)", ns)
  # Plain text format
  sprintf("%s %s %s : %s", timestamp, level_str, metadata, msg)
}
