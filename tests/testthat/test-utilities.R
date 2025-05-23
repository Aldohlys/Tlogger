### Test utility functions

test_that("enable_windows_colors sets the correct option", {
  # Store original value
  original <- getOption("crayon.enabled")
  on.exit(options(crayon.enabled = original))

  # Set to known state
  options(crayon.enabled = FALSE)

  # Test function
  Tlogger::enable_windows_colors()

  if (.Platform$OS.type == "windows") {
    expect_true(getOption("crayon.enabled"),
                info = "Should enable colors on Windows")
  } else {
    # Should not change on non-Windows platforms
    expect_false(getOption("crayon.enabled"),
                 info = "Should not change color setting on non-Windows platforms")
  }
})

test_that("get_common_log_file creates directory if needed", {
  # Create a temp location for testing
  test_dir <- file.path(tempdir(), "log_test_dir")

  # Clean up any existing directory
  if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)

  # Set environment variable
  old_env <- Sys.getenv("R_LOG_DIR")
  Sys.setenv(R_LOG_DIR = test_dir)

  # Call function - should create directory
  log_file <- Tlogger::get_common_log_file()

  # Check if directory was created
  expect_true(dir.exists(test_dir),
              info = "Should create log directory if it doesn't exist")

  # Check file path
  expect_equal(log_file, file.path(test_dir, paste0("t-",format(Sys.Date(), "%Y%m%d"),".log")),
               info = "Should return the correct file path")

  # Reset environment variable
  Sys.setenv(R_LOG_DIR = old_env)

  # Clean up
  if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
})

test_file <- file.path(Sys.getenv("R_LOG_DIR"),"test.log")
file.create(test_file)

write_config_test <- function(config) {
  cfg = list(default = config)
  yaml::write_yaml(cfg, test_file)
  return(test_file)
}

read_config_test <- function() {
  data <- yaml::read_yaml(test_file)
  return(data$default)
}

test_that("Able to write in YAML file package specific data",{
  with_mocked_bindings(
    write_config = write_config_test,
    read_config = read_config_test,
    {
      set_config_namespace("package1", file_level="INFO")
      pkg1 <- get_config_namespace("package1")
      print(pkg1)
      expect_identical(pkg1, list(file_level="INFO"))
    })
})

