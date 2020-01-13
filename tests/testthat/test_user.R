# test-.R

context("user")

test_that("user utilities work", {

  all_data <- list(list(data = cars, dataset_name = "a"),
                   list(data = cars, dataset_name = "b"),
                   list(data = cars, dataset_name = "c"))

  x <- csr_table(all_data, "data")
  expect_is(x, "data.frame")
  expect_identical(nrow(x), nrow(cars) * length(all_data))
})


test_that("csr_dataset", {

  ds1 <- list(data = cars, dataset_name = "DS1")
  all_data <- list(DS1 = ds1,
                   DS2 = list(data = cars, dataset_name = "DS2"),
                   DS3 = list(data = cars, dataset_name = "DS3"))

  expect_error(csr_dataset(all_data, "DS4"))
  expect_identical(csr_dataset(all_data, "DS1"), ds1)
})

