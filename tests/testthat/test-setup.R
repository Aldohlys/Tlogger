
library(logger)

#Look at log_file
log_file <- get_common_log_file()

# Clean up function to reset logger state
reset_logger <- function() {
  # Get all namespaces
  namespaces <- logger::log_namespaces()

  # Reset each namespace to defaults
  for (ns in namespaces) {
    logger::log_threshold(logger::INFO, namespace = ns, index = 1)
    if (logger::log_indices(namespace = ns) > 1) {
      logger::log_threshold(logger::INFO, namespace = ns, index = 2)
    }
  }
}

test_that("setup_namespace_logging configures console correctly", {
  # Clean up
  reset_logger()

  # Setup test namespace with console only
  Tlogger::setup_namespace_logging(
    namespace = "test_setup",
    console_level = logger::DEBUG,
    file_level = NULL  # No file logging
  )

  # Check configuration
  expect_true("test_setup" %in% logger::log_namespaces(),
              info = "Namespace should be registered")

  expect_equal(logger::log_indices(namespace = "test_setup"), 1,
               info = "Should have only one logger index (console)")

  # Test logging - redirect output to check
  output <- capture.output({
    logger::log_debug("Debug message", namespace = "test_setup")
  }, type = "message")

  expect_match(paste(output, collapse = "\n"), "DEBUG.*Debug message",
               info = "Should log debug messages to console")
})

test_that("setup_namespace_logging configures file correctly", {
  # Clean up
  reset_logger()

    # Setup test namespace with file only
  Tlogger::setup_namespace_logging(
    namespace = "test_pkg",
    console_level = NULL,  # No console logging
    file_level = logger::INFO
  )

  # Check configuration
  expect_true("test_pkg" %in% logger::log_namespaces(),
              info = "Namespace should be registered")

  # Modified expectation - we check both indices, but second one should be used for file
  expect_equal(logger::log_indices(namespace = "test_pkg"), 2,
               info = "Should have two logger indices")

  # Test logging to file - the file config should be in index 2
  logger::log_info("File test message", namespace = "test_pkg")

  # Check file content
  Sys.sleep(0.1)  # Wait for file write
  expect_true(file.exists(log_file), info = "Log file should be created")
  log_content <- readLines(log_file)
  expect_match(paste(log_content, collapse = "\n"), "INFO.*File test message",
               info = "Should log messages to file")
})

test_that("setup_namespace_logging configures both console and file", {
  # Clean up
  reset_logger()

  # Setup test namespace with both console and file
  Tlogger::setup_namespace_logging(
    namespace = "test_both",
    console_level = logger::WARN,
    file_level = logger::DEBUG
  )

  # Check configuration
  expect_true("test_both" %in% logger::log_namespaces(),
              info = "Namespace should be registered")

  expect_equal(logger::log_indices(namespace = "test_both"), 2,
               info = "Should have two logger indices")

  # Test console logging - should only show WARN+
  console_output_info <- capture.output({
    logger::log_info("Info console message", namespace = "test_both")
  }, type = "message")

  console_output_warn <- capture.output({
    logger::log_warn("Warn console message", namespace = "test_both")
  }, type = "message")

  expect_equal(length(console_output_info), 0,
               info = "INFO messages should not appear on console")
  expect_match(paste(console_output_warn, collapse = "\n"), "WARN.*Warn console message",
               info = "WARN messages should appear on console")

  # Test file logging - should show DEBUG+
  logger::log_debug("Debug file message", namespace = "test_both")
  logger::log_info("Info file message", namespace = "test_both")

  # Check file content
  Sys.sleep(0.1)  # Wait for file write
  log_content <- readLines(log_file)
  log_content_text <- paste(log_content, collapse = "\n")

  expect_match(log_content_text, "DEBUG.*Debug file message",
               info = "DEBUG messages should be logged to file")
  expect_match(log_content_text, "INFO.*Info file message",
               info = "INFO messages should be logged to file")
})

test_that("update_log_level adjusts thresholds correctly", {
  # Clean up
  reset_logger()

  # Setup test namespace with both console and file
  Tlogger::setup_namespace_logging(
    namespace = "test_update",
    console_level = logger::INFO,
    file_level = logger::INFO
  )

  # Update console level only
  Tlogger::update_log_level(
    namespace = "test_update",
    level = logger::DEBUG,
    console_only = TRUE
  )

  # Update file level only
  Tlogger::update_log_level(
    namespace = "test_update",
    level = logger::WARN,
    file_only = TRUE
  )

  # Test console logging - should show DEBUG+
  console_output <- capture.output({
    logger::log_debug("Debug update message", namespace = "test_update")
  }, type = "message")

  expect_match(paste(console_output, collapse = "\n"), "DEBUG.*Debug update message",
               info = "Console should show DEBUG messages after update")

  # Test file logging - should show only WARN+
  logger::log_info("Info should not appear", namespace = "test_update")
  logger::log_warn("Warn should appear", namespace = "test_update")

  # Check file content
  Sys.sleep(0.1)  # Wait for file write
  log_content <- readLines(log_file)
  log_content_text <- paste(log_content, collapse = "\n")

  expect_false(grepl("Info should not appear", log_content_text),
               info = "INFO messages should not be logged to file after update")
  expect_match(log_content_text, "WARN.*Warn should appear",
               info = "WARN messages should be logged to file after update")
})

test_that("get_common_log_file returns a valid path", {
  result <- Tlogger::get_common_log_file()

  expect_true(is.character(result) && length(result) == 1,
              info = "Should return a single character string")

  # Directory should exist
  dir_path <- dirname(result)
  expect_true(dir.exists(dir_path),
              info = "Directory for common log file should exist")

  # Check environment variable override
  old_env <- Sys.getenv("R_LOG_DIR")
  Sys.setenv(R_LOG_DIR = tempdir())

  # Should use environment variable path
  env_result <- Tlogger::get_common_log_file()
  expected_path <- file.path(tempdir(), paste0("t-",format(Sys.Date(),"%Y%m%d"),".log"))
  expect_equal(env_result, expected_path,
               info = "Should use R_LOG_DIR environment variable if set")

  # Reset environment
  Sys.setenv(R_LOG_DIR = old_env)
})

