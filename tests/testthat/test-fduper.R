context("test-fduper.R")

test_that("fduper", {
  expect_is(fduper(), "fduper")
  expect_equal(fduper(c("a", "b", "c")), tibble(path=c("a", "b", "c")))
  expect_equal(as_fduper(tibble(path=c("a", "b", "c"))), fduper(c("a", "b", "c")))
  expect_equal(is_fduper(fduper()), TRUE)
  expect_error(fduper(sample_path()), NA)
})
