library(logger)
library(crayon)


###################  Logger definition #############################

# Enable colors in Windows properly
if (.Platform$OS.type == "windows") {
  # Check if colors are enabled
  cat("Crayon has color:", crayon::has_color(), "\n")

  # For Windows, we should use this approach
  options(crayon.enabled = TRUE)

  # Verify that colors are now enabled
  cat("Crayon has color after setting option:", crayon::has_color(), "\n")
}

# Custom layout function using the lookup table and switch for colors
# Custom layout function with improved handling for complex objects
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
                     "INFO" = green,
                     "SUCCESS" = green,
                     "WARN" = yellow,
                     "ERROR" = red,
                     "DEBUG" = cyan,
                     "TRACE" = silver,
                     "FATAL" = magenta,
                     function(x) x  # Default - no coloring
  )

  # Define bold color function for the level
  bold_color_fn <- switch(level_str,
                          "INFO" = green$bold,
                          "SUCCESS" = green$bold,
                          "WARN" = yellow$bold,
                          "ERROR" = red$bold,
                          "DEBUG" = cyan$bold,
                          "TRACE" = silver$bold,
                          "FATAL" = magenta$bold,
                          function(x) x  # Default - no coloring
  )

  # Format the prefix parts
  timestamp_c <- color_fn(timestamp)
  level_c <- bold_color_fn(level_str)
  metadata_c <- color_fn(metadata)

  paste(timestamp_c, level_c, metadata_c, " : ", msg)

  # # Handle the message part based on its type
  # if (length(msg) > 1 || grepl("\n", msg)) {
  #   # For multi-line or vector messages, put the content on the next line
  #   paste0(log_header, "\n", msg)
  # } else {
  #   # For simple single-line messages
  #   paste(log_header, msg)
  # }
}

#### Define a plain text layout for the log file (no colors) ###
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

# Create a file path for the system log
system_log_file <- file.path(Sys.getenv("R_USER"), "system.log")

# Set formatter to pander for both loggers
log_formatter(formatter_pander, index = 1)
log_formatter(formatter_pander, index = 2)

# Set up the first logger with colored console output
log_layout(custom_colored_layout, index = 1)
log_appender(appender_console, index = 1)  # CORRECTED: don't call the function

# Set up the second logger with plain text file output
log_layout(plain_layout, index = 2)
log_appender(appender_file(system_log_file), index = 2)  # This is correct because appender_file() returns a function

# Set both loggers to the same threshold
log_threshold(INFO, index = 1)
log_threshold(INFO, index = 2)


########################  Logger usage ############################
# Set the custom layout
log_layout(custom_colored_layout)

# Set the formatter to handle complex objects
# log_formatter(formatter_sprintf)  # Option 1: basic formatter
log_formatter(formatter_pander)       # Option 2: more flexible

# Test with different types of objects
log_info("Simple message")

# Test with a data frame
log_info(head(iris))
log_info(head(iris), namespace="Tbasics")

# Test with a list
my_list <- list(a = 1:5, b = letters[1:5], c = list(x = 10, y = 20))
log_info(my_list)

# Test with a multi-line message
log_info("Line 1\nLine 2\nLine 3")

# Test with a vector
log_info(c("First element", "Second element", "Third element"))

# Test in different namespaces
log_info(head(mtcars), namespace = "Tbasics")
# Test with different namespaces
log_info("Test message")
log_info("Default namespace message")
log_info("Tbasics module message", namespace="Tbasics")
log_info("UI module message", namespace="UI")

# Set threshold for a specific namespace
log_threshold(logger::DEBUG, namespace="Tbasics")

# Now test with different log levels for that namespace
log_debug("Debug message for Tbasics", namespace="Tbasics")
log_info("Info message for Tbasics", namespace="Tbasics")
log_warn("Warning message for Tbasics", namespace="Tbasics")
log_debug("Debug message for default namespace", namespace="Tbasics")

