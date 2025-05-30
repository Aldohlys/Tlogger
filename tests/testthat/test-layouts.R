### Test Layout functions

library(logger)
library(crayon)

# Mock log level for testing
mock_level <- structure(
  100,
  level = "INFO",
  class = c("loglevel", "integer")
)

test_that("custom_colored_layout formats correctly", {
  # Prepare test environment
  crayon_enabled <- getOption("crayon.enabled")
  on.exit(options(crayon.enabled = crayon_enabled))
  options(crayon.enabled = TRUE)

  # Call function
  result <- Tlogger::custom_colored_layout(
    level = mock_level,
    msg = "Test message",
    namespace = "test_ns"
  )

  # Test results - we can't easily test colors, but we can test formatting
  expect_match(result, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}",
               info = "Should include timestamp without milliseconds")
  expect_match(result, "INFO", info = "Should include the level name")
  expect_match(result, "\\[R\\] \\(test_ns\\)",
               info = "Should include formatted namespace")
  expect_match(result, "Test message", info = "Should include the message")
})

test_that("plain_layout formats without colors", {
  # Call function
  result <- Tlogger::plain_layout(
    level = mock_level,
    msg = "Test message",
    namespace = "test_ns"
  )

  # Test that ANSI color codes are not present
  expect_false(grepl("\033\\[", result), info = "Should not contain ANSI color codes")

  # Test format
  expect_match(result, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} INFO \\[R\\] \\(test_ns\\)::\\(eval\\) : Test message",
               info = "Should format string correctly")
})

test_that("layouts handle missing namespace", {
  # Call with NA namespace
  colored_result <- Tlogger::custom_colored_layout(
    level = mock_level,
    msg = "Test message",
    namespace = NA_character_
  )

  plain_result <- Tlogger::plain_layout(
    level = mock_level,
    msg = "Test message",
    namespace = NA_character_
  )

  # Both should fall back to "global"
  expect_match(colored_result, "\\(global\\)", info = "Colored layout should use 'global' for NA namespace")
  expect_match(plain_result, "\\(global\\)", info = "Plain layout should use 'global' for NA namespace")
})

test_that("layouts handle complex objects", {
  # Create a multi-line message
  complex_msg <- "Line 1\nLine 2"

  # Test with colored layout
  colored_result <- Tlogger::custom_colored_layout(
    level = mock_level,
    msg = complex_msg,
    namespace = "test_ns"
  )

  # Test with plain layout
  plain_result <- Tlogger::plain_layout(
    level = mock_level,
    msg = complex_msg,
    namespace = "test_ns"
  )

  # Both should include the message
  expect_match(colored_result, "Line 1\\nLine 2", info = "Colored layout should preserve newlines")
  expect_match(plain_result, "Line 1\\nLine 2", info = "Plain layout should preserve newlines")
})
