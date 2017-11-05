context("fduper")

test_that("fduper", {
  expect_is(fduper(), "fduper")
  expect_equal(fduper(c("a", "b", "c")), tibble(path=c("a", "b", "c")))
  expect_equal(as_fduper(tibble(path=c("a", "b", "c"))), fduper(c("a", "b", "c")))
  expect_equal(as_fduper(data.frame(path=c("a", "b", "c"), stringsAsFactors = FALSE)), fduper(c("a", "b", "c")))
  expect_equal(c(10,1), dim(fduper(sample_path())))
})
