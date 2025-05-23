## tests for package integration

library(logger)

# Mock package functions
create_mock_package <- function(name, console_level = logger::INFO, file_level = logger::DEBUG) {
  # Set up logging for this mock package
  Tlogger::setup_namespace_logging(
    namespace = name,
    console_level = console_level,
    file_level = file_level
  )
}

test_that("packages can log to log file", {
  # Look at log file
  log_file <- get_common_log_file()

  # Set up logging for two packages to the same file
  Tlogger::setup_namespace_logging(
    namespace = "pkg_common1",
    console_level = logger::INFO,
    file_level = logger::INFO
  )

  Tlogger::setup_namespace_logging(
    namespace = "pkg_common2",
    console_level = logger::INFO,
    file_level = logger::INFO
  )

  # Log from each package
  logger::log_info("Info from pkg_common1", namespace = "pkg_common1")
  logger::log_info("Info from pkg_common2", namespace = "pkg_common2")

  # Check common log file
  Sys.sleep(0.1)  # Wait for file write

  log_content <- readLines(log_file)
  log_text <- paste(log_content, collapse = "\n")

  # Should contain messages from both packages
  expect_match(log_text, "\\(pkg_common1\\).*Info from pkg_common1",
               info = "Common log should contain messages from pkg_common1")
  expect_match(log_text, "\\(pkg_common2\\).*Info from pkg_common2",
               info = "Common log should contain messages from pkg_common2")

})

test_that("packages maintain separate thresholds", {
  # Look at log file
  log_file <- get_common_log_file()

  # Set up logging for two packages with different thresholds
  Tlogger::setup_namespace_logging(
    namespace = "pkg_threshold1",
    console_level = logger::DEBUG,  # More verbose
    file_level = logger::INFO
  )

  Tlogger::setup_namespace_logging(
    namespace = "pkg_threshold2",
    console_level = logger::WARN,   # Less verbose
    file_level = logger::INFO
  )

  # pkg_threshold1 console output (DEBUG should appear)
  console_output1 <- capture.output({
    logger::log_debug("Debug from pkg_threshold1", namespace = "pkg_threshold1")
  }, type = "message")

  # pkg_threshold2 console output (DEBUG should not appear)
  console_output2 <- capture.output({
    logger::log_debug("Debug from pkg_threshold2", namespace = "pkg_threshold2")
  }, type = "message")

  # DEBUG message should appear only for pkg_threshold1
  expect_match(paste(console_output1, collapse = "\n"), "DEBUG.*pkg_threshold1",
               info = "DEBUG messages should appear for pkg_threshold1")
  expect_equal(length(console_output2), 0,
               info = "DEBUG messages should not appear for pkg_threshold2")

  # Log INFO messages to file from both packages
  logger::log_info("Info from pkg_threshold1", namespace = "pkg_threshold1")
  logger::log_info("Info from pkg_threshold2", namespace = "pkg_threshold2")

  # Check common log file
  Sys.sleep(0.1)  # Wait for file write

  log_content <- readLines(log_file)
  log_text <- paste(log_content, collapse = "\n")

  # Both INFO messages should be in the file
  expect_match(log_text, "\\(pkg_threshold1\\).*Info from pkg_threshold1",
               info = "File should contain INFO message from pkg_threshold1")
  expect_match(log_text, "\\(pkg_threshold2\\).*Info from pkg_threshold2",
               info = "File should contain INFO message from pkg_threshold2")

})
