### Test Mock package integration

test_that("mock packages can use Tlogger", {
  # Create two mock packages
  pkg_a <- create_test_package("pkg_a")
  pkg_b <- create_test_package("pkg_b")

  # Capture console output
  console_output <- capture.output({
    # Call functions in both packages
    result_a <- pkg_a$sample_function(10)
    result_b <- pkg_b$sample_function(20)
  }, type = "message")

  console_text <- paste(console_output, collapse = "\n")

  # Check results
  expect_equal(result_a, 20)
  expect_equal(result_b, 40)

  # Check that logs contain appropriate namespace
  expect_match(console_text, "\\(pkg_a\\).*Processing input: 10",
               info = "Log should contain message from pkg_a with correct namespace")
  expect_match(console_text, "\\(pkg_b\\).*Processing input: 20",
               info = "Log should contain message from pkg_b with correct namespace")
})

test_that("packages can interact with distinct log settings", {
  # Create mock packages with different settings
  pkg_verbose <- create_test_package("pkg_verbose")
  pkg_verbose$init_logging(console_level = logger::DEBUG)

  pkg_quiet <- create_test_package("pkg_quiet")
  pkg_quiet$init_logging(console_level = logger::WARN)

  # Function in pkg_verbose that calls pkg_quiet
  pkg_verbose$call_quiet <- function(x) {
    pkg_verbose$log_debug("Before calling pkg_quiet")
    result <- pkg_quiet$sample_function(x)
    pkg_verbose$log_info("After calling pkg_quiet")
    return(result)
  }

  # Capture console output
  console_output <- capture.output({
    result <- pkg_verbose$call_quiet(5)
  }, type = "message")

  console_text <- paste(console_output, collapse = "\n")

  # Check result
  expect_equal(result, 10)

  # Check that each package maintained its own log level
  expect_match(console_text, "DEBUG.*\\(pkg_verbose\\).*Before calling pkg_quiet",
               info = "pkg_verbose should log DEBUG messages")

  expect_match(console_text, "INFO.*\\(pkg_verbose\\).*After calling pkg_quiet",
               info = "pkg_verbose should log INFO messages")

  # pkg_quiet should not log INFO, only WARN+
  expect_false(grepl("Processing input: 5", console_text),
               info = "pkg_quiet should not log INFO messages")
})


