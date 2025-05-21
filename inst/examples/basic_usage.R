# inst/examples/basic_usage.R

library(Tlogger)
library(logger)

# Setup logging for two example packages
Tlogger::setup_namespace_logging(
  namespace = "package1",
  console_level = logger::INFO,
  file_level = logger::DEBUG
)

Tlogger::setup_namespace_logging(
  namespace = "package2",
  console_level = logger::WARN,  # Less verbose on console
  file_level = logger::INFO
)

# Log from different packages
logger::log_info("This is an informational message", namespace = "package1")
logger::log_debug("This is a debug message", namespace = "package1")
logger::log_error("This is an error message", namespace = "package1")

logger::log_info("This won't show on console", namespace = "package2")
logger::log_warn("This will show on console", namespace = "package2")

# Check the common log file
log_file <- Tlogger::get_common_log_file()
cat("Log file location:", log_file, "\n")
if (file.exists(log_file)) {
  cat("\nLog file contents:\n")
  cat(readLines(log_file), sep = "\n")
}
