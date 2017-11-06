context("test-helpers.R")

test_that("helper functions execute", {
  expect_error(fduper(sample_path()) %>%
                 mutate(
                   created = get_created(path),
                   directoryname = get_directoryname(path),
                   dos = get_dos(path),
                   ext = get_ext(path),
                   filename = get_filename(path),
                   hash = get_hash(path),
                   modified = get_modified(path),
                   size = get_size(path),
                   unix = get_unix(path)
                 ), NA
  )
})

test_that("hashing functions work", {
  expect_equal(
    get_hash_(paste0(sample_path(), "/root_file.txt")),
    get_unix_(paste0(sample_path(), "/root_file.txt")),
    get_dos_(paste0(sample_path(),  "/root_file.txt")),
    "d41d8cd98f00b204e9800998ecf8427e"
  )
  expect_equivalent(
    get_dos(paste0(sample_path(),  "/mixed_files/file.dos")),
    get_dos(paste0(sample_path(),  "/mixed_files/file.unix")),
    "e73484cf2c2676d02f357a0ef8f7cfe6"
  )
  expect_equivalent(
    get_unix(paste0(sample_path(),  "/mixed_files/file.dos")),
    get_unix(paste0(sample_path(),  "/mixed_files/file.unix")),
    "a6b8a0a9c676aa74371e81ade3b7340a"
  )
})

test_that("quick add functions execute", {
  expect_error(fduper(sample_path()) %>% add_created, NA)
  expect_error(fduper(sample_path()) %>% add_directoryname, NA)
  expect_error(fduper(sample_path()) %>% add_dos, NA)
  expect_error(fduper(sample_path()) %>% add_ext, NA)
  expect_error(fduper(sample_path()) %>% add_filename, NA)
  expect_error(fduper(sample_path()) %>% add_hash, NA)
  expect_error(fduper(sample_path()) %>% add_modified, NA)
  expect_error(fduper(sample_path()) %>% add_size, NA)
  expect_error(fduper(sample_path()) %>% add_unix, NA)
})

test_that("dplyr functions are accessible", {
  expect_error(fduper(sample_path()) %>% arrange(path), NA)
  expect_error(fduper(sample_path()) %>% filter(grepl(".txt$", path)), NA)
  expect_error(fduper(sample_path()) %>% group_by(path) %>%
                                         ungroup, NA)
  expect_error(fduper(sample_path()) %>% mutate(a = path), NA)
})

test_that("reducers", {
  # keep_files_by_pattern
  # remove_files_by_pattern
  # reduce
  # reduce_n
})

test_that("identify", {

})
