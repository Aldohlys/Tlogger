## tests for package integration

library(logger)

# Mock package functions
create_mock_package <- function(name, console_level = logger::INFO, file_level = logger::DEBUG) {
  # Create a unique temp file for this mock package
  log_file <- tempfile(pattern = paste0(name, "_"), fileext = ".log")

  # Set up logging for this mock package
  Tlogger::setup_namespace_logging(
    namespace = name,
    console_level = console_level,
    file_level = file_level,
    log_file = log_file
  )

  # Return the log file path for testing
  return(log_file)
}

test_that("multiple packages can log to their own files", {
  # Create mock packages
  pkg1_file <- create_mock_package("pkg1", logger::INFO, logger::DEBUG)
  pkg2_file <- create_mock_package("pkg2", logger::WARN, logger::INFO)

  # Log from each package
  logger::log_info("Info from pkg1", namespace = "pkg1")
  logger::log_debug("Debug from pkg1", namespace = "pkg1")

  logger::log_info("Info from pkg2", namespace = "pkg2")
  logger::log_debug("Debug from pkg2", namespace = "pkg2") # Should not appear in pkg2 log

  # Check log files
  Sys.sleep(0.1)  # Wait for file write

  pkg1_content <- readLines(pkg1_file)
  pkg1_text <- paste(pkg1_content, collapse = "\n")

  pkg2_content <- readLines(pkg2_file)
  pkg2_text <- paste(pkg2_content, collapse = "\n")

  # pkg1 should have both messages (INFO and DEBUG)
  expect_match(pkg1_text, "INFO.*Info from pkg1",
               info = "pkg1 should log INFO messages")
  expect_match(pkg1_text, "DEBUG.*Debug from pkg1",
               info = "pkg1 should log DEBUG messages")

  # pkg2 should only have INFO message
  expect_match(pkg2_text, "INFO.*Info from pkg2",
               info = "pkg2 should log INFO messages")
  expect_false(grepl("Debug from pkg2", pkg2_text),
               info = "pkg2 should not log DEBUG messages")

  # Clean up
  if (file.exists(pkg1_file)) file.remove(pkg1_file)
  if (file.exists(pkg2_file)) file.remove(pkg2_file)
})

test_that("packages can log to the same file", {
  # Create a common log file
  common_file <- tempfile(fileext = ".log")

  # Set up logging for two packages to the same file
  Tlogger::setup_namespace_logging(
    namespace = "pkg_common1",
    console_level = logger::INFO,
    file_level = logger::INFO,
    log_file = common_file
  )

  Tlogger::setup_namespace_logging(
    namespace = "pkg_common2",
    console_level = logger::INFO,
    file_level = logger::INFO,
    log_file = common_file
  )

  # Log from each package
  logger::log_info("Info from pkg_common1", namespace = "pkg_common1")
  logger::log_info("Info from pkg_common2", namespace = "pkg_common2")

  # Check common log file
  Sys.sleep(0.1)  # Wait for file write

  log_content <- readLines(common_file)
  log_text <- paste(log_content, collapse = "\n")

  # Should contain messages from both packages
  expect_match(log_text, "\\(pkg_common1\\).*Info from pkg_common1",
               info = "Common log should contain messages from pkg_common1")
  expect_match(log_text, "\\(pkg_common2\\).*Info from pkg_common2",
               info = "Common log should contain messages from pkg_common2")

  # Clean up
  if (file.exists(common_file)) file.remove(common_file)
})

test_that("packages maintain separate thresholds", {
  # Create a common log file
  common_file <- tempfile(fileext = ".log")

  # Set up logging for two packages with different thresholds
  Tlogger::setup_namespace_logging(
    namespace = "pkg_threshold1",
    console_level = logger::DEBUG,  # More verbose
    file_level = logger::INFO,
    log_file = common_file
  )

  Tlogger::setup_namespace_logging(
    namespace = "pkg_threshold2",
    console_level = logger::WARN,   # Less verbose
    file_level = logger::INFO,
    log_file = common_file
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

  log_content <- readLines(common_file)
  log_text <- paste(log_content, collapse = "\n")

  # Both INFO messages should be in the file
  expect_match(log_text, "\\(pkg_threshold1\\).*Info from pkg_threshold1",
               info = "File should contain INFO message from pkg_threshold1")
  expect_match(log_text, "\\(pkg_threshold2\\).*Info from pkg_threshold2",
               info = "File should contain INFO message from pkg_threshold2")

  # Clean up
  if (file.exists(common_file)) file.remove(common_file)
})
