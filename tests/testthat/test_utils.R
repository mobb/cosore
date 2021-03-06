# test-utils.R

context("utils")

test_that("rbind_list", {
  expect_equivalent(rbind_list(list(NULL)), data.frame())
  expect_equivalent(rbind_list(list(cars, NULL)), cars)
  expect_error(rbind_list(cars))

  x <- data.frame(a = 1)
  y <- data.frame(b = 2)
  z <- rbind_list(list(x, y))
  expect_is(z, "data.frame")
  expect_identical(nrow(z), nrow(x) + nrow(y))
  expect_identical(names(z), union(names(x), names(y)))
  expect_equivalent(rbind_list(list(x, x)), rbind(x, x))
})

test_that("split_licor_file", {
  tf <- tempfile()
  nr <- "LI-8100"  # new record delimiter

  # handles empty file
  cat("", sep = "\n", file = tf)
  expect_equal(split_licor_file(tf), 0)

  cat(rep(c(nr, rep("data", 10)), 2), sep = "\n", file = tf)

  # split_lines smaller than record size
  expect_equal(split_licor_file(tf, split_lines = 2), 2)
  # split_lines larger than record size
  expect_equal(split_licor_file(tf, split_lines = 20), 1)
  # split_lines larger than file size
  expect_equal(split_licor_file(tf, split_lines = 200), 1)

  # bad input
  expect_error(split_licor_file("nonexistent-file"))
  expect_error(split_licor_file(tf, split_lines = 0))
  expect_error(split_licor_file(tf, out_dir = "nonexistent-dir"))
})

test_that("fractional_doy", {
  y <- 2001
  d <- 100.5

  x <- fractional_doy(y, d)
  expect_is(x, "character")

  # Halfway through the day should be 12 o'clock
  expect_true(grepl("12:00:00", fractional_doy(y, 105.5),  fixed = TRUE))

  # The initial DOY in the return string should be the whole part of what we sent
  for(d in c(1.0, 2.12, 100, 311.2345)) {
    x <- fractional_doy(y, d)
    dback <- strsplit(x, " ")[[1]][1]
    expect_identical(as.integer(dback), as.integer(d))
  }

  # Bad input
  expect_error(fractional_doy(y, 0))
  expect_error(fractional_doy(y, 367))
})

test_that("csr_standardize_data", {
  # Handles bad input
  expect_error(csr_standardize_data(1, "", TRUE))
  expect_error(csr_standardize_data(list(), 1, TRUE))
  expect_error(csr_standardize_data(list(), "", 1))

  library(lubridate)
  data1 <- tibble(CSR_TIMESTAMP = ymd_hms(c("2020-01-01 12:34:56",
                                            "2021-01-01 12:34:56")))
  ds1 <- list(description = tibble(CSR_DATASET = "ds1"),
              diagnostics = tibble(),
              data = data1)
  td <- tempdir()

  # Error - data subdirectory doesn't exist
  expect_error(csr_standardize_data(ds1, td, create_dirs = FALSE),
               regexp = "does not exist")

  expect_null(csr_standardize_data(ds1, td, create_dirs = TRUE))
  td_dataset <- file.path(td, "ds1", "data")

  # Subdirectory was created
  expect_true(dir.exists(td_dataset))
  # Files exist
  expect_true(file.exists(file.path(td_dataset, "diag.RDS")))
  expect_true(file.exists(file.path(td_dataset, "data.RDS")))
  written_data <- readRDS(file.path(td_dataset, "data.RDS"))
  expect_identical(ds1$data, written_data)

  # Removing standardized files works
  csr_remove_stan_data(td, datasets = "ds1")
  expect_identical(list.files(td_dataset), character(0))
  expect_message(csr_remove_stan_data("directory_doesnt_exist", datasets = "ds1"))

  # Handles no-data dataset: should create directory but write no files
  ds2 <- list(description = tibble(CSR_DATASET = "ds2"))

  csr_standardize_data(ds2, td, create_dirs = TRUE)
  td_dataset <- file.path(td, "ds2", "data")
  expect_true(dir.exists(td_dataset))
  expect_identical(length(list.files(td_dataset)), 0L)

  csr_remove_stan_data(td, datasets = "ds2")
  expect_identical(list.files(td_dataset), character(0))
})

test_that("convert_and_qc_timestamp", {
  # Handles bad input
  expect_error(convert_and_qc_timestamp(1, "1", "1"))
  expect_error(convert_and_qc_timestamp("1", 1, "1"))
  expect_error(convert_and_qc_timestamp("1", "1", 1))

  ts <- c("2020-01-01 12:34:56", "2021-01-01 12:34:56")
  out <- convert_and_qc_timestamp(ts, "%Y-%m-%d %H:%M:%S", "UTC")
  expect_type(out, "list")
  expect_identical(length(out), 3L)
  expect_identical(length(out$new_ts), length(ts))

  # Behaves as expected with invalid format string
  out <- convert_and_qc_timestamp(ts, "not-a-valid-format-string", "UTC")
  expect_true(all(is.na(out$new_ts)))
  expect_true(all(out$na_ts))
})


test_that("compute_interval", {
  # Bad input
  expect_error(compute_interval(1))

  x <- tibble(CSR_TIMESTAMP_BEGIN = seq(ISOdate(2000, 1, 1), by = "day", length.out = 12),
              CSR_PORT = 0)
  y <- compute_interval(x)
  expect_s3_class(y, "data.frame")
  expect_identical(y$Interval, 60 * 24)  # minutes in a day
  expect_identical(y$N, nrow(x))

  # Sorting shouldn't matter
  xrev <- x[order(x$CSR_TIMESTAMP_BEGIN, decreasing = TRUE),]
  expect_identical(compute_interval(x), compute_interval(xrev))
})

test_that("csr_build", {
  # handles bad input
  expect_error(csr_build(dataset_names = 1))
  expect_error(csr_build(force_raw = 1))
  expect_error(csr_build(write_standardized = 1))
  expect_error(csr_build(standardized_path = 1))
  expect_error(csr_build(quiet = 1))

  # not sure how to test this effectively at the moment
  expect_message(csr_build(dataset_names = list_datasets()[1]), quiet = FALSE)
  expect_silent(csr_build(dataset_names = list_datasets()[1], quiet = TRUE))
})

test_that("check_dataset_names", {

  # handles bad input
  expect_error(check_dataset_names(1, list(), data.frame()))
  expect_error(check_dataset_names("", 1, data.frame()))
  expect_error(check_dataset_names("", list(), 1))

  # Returns correct entry count across tables
  dataset <- list(table1 = data.frame(field1 = 1, field2 = 2),
                  table2 = data.frame(field2 = 3, field3 = 4))
  metadata <- data.frame(Table_name = c("table1", "table1", "table2", "table2"),
                         Field_name = c("field1", "field2", "field2", "field3"),
                         Required = FALSE)
  expect_identical(check_dataset_names("", dataset, metadata), c(1, 2, 2, 1))

  # Warns when table field is missing in metadata
  dataset <- list(table1 = data.frame(field1 = 1, field2 = 2))
  metadata <- data.frame(Table_name = "table1",
                         Field_name = "field1",
                         Required = FALSE)
  expect_warning(check_dataset_names("", dataset, metadata), regexp = "field2")

  # Warns when required metadata field is missing in table
  dataset <- list(table1 = data.frame(field1 = 1))
  metadata <- data.frame(Table_name = "table1",
                         Field_name = c("field1", "field2"),
                         Required = c(FALSE, TRUE))
  expect_warning(check_dataset_names("", dataset, metadata), regexp = "field2")

  # Handles temperature and soil moisture fields correctly
  dataset <- list(table1 = data.frame(CSR_T2 = 1, CSR_SM37.1 = 1))
  metadata <- data.frame(Table_name = "table1",
                         Field_name = c("CSR_Tx", "CSR_SMx"),
                         Required = c(FALSE, FALSE))
  expect_silent(check_dataset_names("", dataset, metadata))
})

test_that("calc_timestamps", {
  # Handles bad input
  expect_error(calc_timestamps(1, 1, "1", "1"))
  expect_error(calc_timestamps(data.frame(), "1", "1", "1"))
  expect_error(calc_timestamps(data.frame(), 1, 1, "1"))
  expect_error(calc_timestamps(data.frame(), 1, "1", 1))

  # If data.frame doesn't enough information, error
  expect_error(calc_timestamps(data.frame(), 1, "1", "1"))

  # We're going to run these tests a bunch so...
  validate_list <- function(x, ml, description) {
    expect_type(x, "list")
    expect_identical(length(x), 4L)

    new_df <- x[["dsd"]]
    expect_s3_class(new_df, "data.frame")

    expect_s3_class(new_df$CSR_TIMESTAMP_BEGIN, c("POSIXct", "POSIXt"))
    expect_identical(sort(colnames(new_df)),
                     sort(c("CSR_TIMESTAMP_BEGIN", "CSR_TIMESTAMP_END")))
    difft <- difftime(new_df$CSR_TIMESTAMP_END, new_df$CSR_TIMESTAMP_BEGIN, units = "secs")
    expect_identical(as.numeric(difft), ml)
  }

  # Supply begin, get end
  ml <- 120
  df <- data.frame(CSR_TIMESTAMP_BEGIN = "2020-02-06 18:47", stringsAsFactors = FALSE)
  x <- calc_timestamps(df, ml = ml, tf = "%Y-%m-%d %H:%M", tz = "UTC")
  validate_list(x,  ml, "Supply begin, get end")

  # Supply end, get begin
  df <- data.frame(CSR_TIMESTAMP_END = "2020-02-06 18:47", stringsAsFactors = FALSE)
  x <- calc_timestamps(df, ml = ml, tf = "%Y-%m-%d %H:%M", tz = "UTC")
  validate_list(x,  ml, "Supply end, get begin")

  # Supply mid, get begin and end
  df <- data.frame(CSR_TIMESTAMP_MID = "2020-02-06 18:47", stringsAsFactors = FALSE)
  x <- calc_timestamps(df, ml = ml, tf = "%Y-%m-%d %H:%M", tz = "UTC")
  validate_list(x,  ml, "Supply mid, get begin and end")

  # Supply both begin and end
  df <- data.frame(CSR_TIMESTAMP_BEGIN = "2020-02-06 18:47",
                   CSR_TIMESTAMP_END = "2020-02-06 18:49", stringsAsFactors = FALSE)
  x <- calc_timestamps(df, ml = ml, tf = "%Y-%m-%d %H:%M", tz = "UTC")
  validate_list(x,  ml, "Supply begin and end")
})

test_that("rearrange_colunns", {
  # Handles bad input
  expect_error(rearrange_columns(1, "1"))
  expect_error(rearrange_columns(data.frame(), 1))

  # Puts required cols first and sorts the rest
  df <- data.frame(x = 1, y = 2, z = 3)
  expect_identical(colnames(rearrange_columns(df, "x")), c("x", "y", "z"))
  expect_identical(colnames(rearrange_columns(df, "y")), c("y", "x", "z"))
  expect_identical(colnames(rearrange_columns(df, "z")), c("z", "x", "y"))
  expect_identical(colnames(rearrange_columns(df, c("z", "x"))), c("z", "x", "y"))

  # Errors if 'required' cols not all in data frame already
  expect_error(rearrange_columns(df, "a"))
  expect_error(rearrange_columns(df, c("x", "x1")))

  # Handles data frame with no 'other' columns
  x <- rearrange_columns(df, colnames(df))
  expect_identical(df, x)

  # Handles no required columns at all
  df <- data.frame(z = 1, y = 2, x = 3)
  expect_identical(colnames(rearrange_columns(df, character(0))),
                   sort(colnames(df)))
})

test_that("check_dataset_names", {
  expect_error(TSM_change(1))

  # Transforms as expected
  expect_identical(TSM_change(c("CSR_T0", "CSR_SM2")), c("CSR_Tx", "CSR_SMx"))
  expect_identical(TSM_change(c("CSR_T2.5", "CSR_T", "CSR_0")), c("CSR_Tx", "CSR_T", "CSR_0"))
  expect_identical(TSM_change(c("blah", "blah2.5"), prefixes = "blah"), c("blah", "blahx"))
})

test_that("insert_line", {
  # Create a test file
  td <- tempdir()
  f <- "testfile.txt"
  fqfn <- file.path(td, f)
  fd <- c("Line1", "Line2", "Line3")

  # Handles zero matching files
  expect_message(insert_line("testfile.txt", pattern = "Line2",
                             newlines = "Line4", after = TRUE, path = td))

  # Insert after
  writeLines(fd, con = fqfn)
  insert_line("testfile.txt", pattern = "Line2", newlines = "Line4", after = TRUE, path = td)
  expect_identical(readLines(fqfn), c("Line1", "Line2", "Line4", "Line3"))
  # Warn if new lines already present
  expect_warning(insert_line("testfile.txt", pattern = "Line2",
                             newlines = "Line4", after = TRUE, path = td),
                 regexp = "already found")

  # Insert before
  writeLines(fd, con = fqfn)
  insert_line("testfile.txt", pattern = "Line2", newlines = "Line4", after = FALSE, path = td)
  expect_identical(readLines(fqfn), c("Line1", "Line4", "Line2", "Line3"))
  # Warn if new lines already present
  expect_warning(insert_line("testfile.txt", pattern = "Line2",
                             newlines = "Line4", after = FALSE, path = td),
                 regexp = "already found")

  # Warning if pattern not present
  expect_warning(insert_line("testfile.txt", pattern = "Line5",
                             newlines = "Line4", after = FALSE, path = td),
                 regexp = "No insert point found")

  # Beginning and end of file
  writeLines(fd, con = fqfn)
  insert_line("testfile.txt", pattern = "Line1", newlines = "Line4", after = FALSE, path = td)
  expect_identical(readLines(fqfn), c("Line4", "Line1", "Line2", "Line3"))
  writeLines(fd, con = fqfn)
  insert_line("testfile.txt", pattern = "Line3", newlines = "Line4", after = TRUE, path = td)
  expect_identical(readLines(fqfn), c("Line1", "Line2", "Line3", "Line4"))

  # Multiple insertion points
  fd <- c("Line1", "Line2", "Line3", "Line2")
  writeLines(fd, con = fqfn)
  expect_error(insert_line("testfile.txt", pattern = "Line2",
                           newlines = "Line4", after = FALSE, path = td),
               regexp = "more than one")

})

