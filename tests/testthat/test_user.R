# test-.R

context("user")

test_that("csr_table", {

  expect_error(csr_table(table = 1))
  expect_error(csr_table(table = c("1", "2")))
  expect_error(csr_table(table = "test", datasets = 1))

})


test_that("csr_dataset", {

  expect_error(csr_table(dataset = 1))
  expect_error(csr_table(dataset = c("1", "2")))

})

