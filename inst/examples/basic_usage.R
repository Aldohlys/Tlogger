# inst/examples/basic_usage.R

library(Tlogger)

# Setup logging for two example packages
setup_namespace_logging(
  namespace = "package1",
  console_level = logger::INFO,
  file_level = logger::DEBUG,
  formatter=logger::formatter_glue
)

setup_namespace_logging(
  namespace = "package2",
  console_level = logger::WARN,  # Less verbose on console
  file_level = logger::INFO
)


logger::log_namespaces()

summarize_all_loggers()

df = data.frame(x=1:10,y=runif(10))
x=100
v=c(1,3,5,6,10)

# Log from different packages
logger::log_info("This is an informational message", namespace = "package1")
logger::log_debug("This is a debug message", namespace = "package1")
logger::log_error("This is an error message", namespace = "package1")
logger::log_info("This is an info message with x2={x*x}", namespace = "package1")

logger::log_info("This won't show on console", namespace = "package2")
logger::log_warn("This will show on console", namespace = "package2")
logger::log_debug("This is a DEBUG message that will not be shown at all", author="Alexis", namespace = "package2")
logger::log_info("This is an info message with x2={x*x} and df=", df, namespace = "package2")
logger::log_error("This is an error message with x2={x*x} and df=", t(df), namespace = "package2")
logger::log_warn("This is a WARNING: vector {glue::glue_collapse(v, sep=', ')} is dangerous", namespace="package2")

### Log with parameters and evaluation
logger::log_info("This is an INFO message from {author} regarding this package", author="Alexis", namespace = "package1")

author="Alexis"
logger::log_error("This is an ERROR message from {author} about { 2+2; round(sqrt(5), 3)} = 5  regarding this package", namespace = "package2")

# Check the common log file
log_file <- Tlogger::get_common_log_file()
cat("Log file location:", log_file, "\n")
if (file.exists(log_file)) {
  cat("\nLog file contents:\n")
  cat(readLines(log_file), sep = "\n")
}

