context("test-file-collection.R")

test_that("sample_file_structure_integrity", {
  expect_true(dir.exists(sample_path()))
})

test_that("get_files", {
  # path
  expect_equal(10, length(get_files(sample_path())))
  expect_equal(4, length(get_files(paste0(sample_path(), "/subfolder"))))
  # pattern
  expect_equal(5, length(get_files(sample_path(), ".txt$")))
  # recursive
  expect_equal(10, length(get_files(sample_path(), recursive = TRUE)))
  expect_equal(1, length(get_files(sample_path(), recursive = FALSE)))
  expect_equal(4, length(get_files(paste0(sample_path(), "/subfolder"), recursive = TRUE)))
  expect_equal(2, length(get_files(paste0(sample_path(), "/subfolder"), recursive = FALSE)))
  # all.files
  expect_equal(10, length(get_files(sample_path(), all.files = TRUE)))
  expect_equal(8, length(get_files(sample_path(), all.files = FALSE)))
  expect_equal(3, length(get_files(paste0(sample_path(), "/various"), all.files = TRUE)))
  expect_equal(1, length(get_files(paste0(sample_path(), "/various"), all.files = FALSE)))
  expect_equal(1, length(get_files(paste0(sample_path(), "/various/.other"), all.files = TRUE)))
  expect_equal(1, length(get_files(paste0(sample_path(), "/various/.other"), all.files = FALSE)))
  # ignore.case
  expect_equal(2, length(get_files(sample_path(), pattern="file$", ignore.case=FALSE)))
  expect_equal(3, length(get_files(sample_path(), pattern="file$", ignore.case=TRUE)))
  # unexpected input
  expect_error(get_files(sample_path(), full.name = TRUE))
  expect_error(get_files(sample_path(), include.dirs = TRUE))
  expect_error(get_files(sample_path(), no.. = TRUE))
  expect_error(get_files(sample_path(), zzz = TRUE))
})

test_that("get_dirs", {
  # path
  expect_equal(5, length(get_dirs(sample_path())))
  expect_equal(1, length(get_dirs(paste0(sample_path(), "/subfolder"))))
  # pattern
  expect_equal(1, length(get_dirs(sample_path(), "_files$")))
  # recursive
  expect_equal(5, length(get_dirs(sample_path(), recursive = TRUE)))
  expect_equal(3, length(get_dirs(sample_path(), recursive = FALSE)))
  # all.dirs
  expect_equal(5, length(get_dirs(sample_path(), all.dirs = TRUE)))
  expect_equal(4, length(get_dirs(sample_path(), all.dirs = FALSE)))
  expect_equal(1, length(get_dirs(paste0(sample_path(), "/various"), all.dirs = TRUE)))
  expect_equal(0, length(get_dirs(paste0(sample_path(), "/various"), all.dirs = FALSE)))
  # ignore.case
  expect_equal(5, length(get_dirs(sample_path(), ignore.case=FALSE)))
  # unexpected input
  expect_error(get_dirs(sample_path(), all.files = TRUE))
  expect_error(get_dirs(sample_path(), full.name = TRUE))
  expect_error(get_dirs(sample_path(), include.dirs = TRUE))
  expect_error(get_dirs(sample_path(), no.. = TRUE))
  expect_error(get_dirs(sample_path(), zzz = TRUE))
})

test_that("add_files", {
  expect_equal(1, fduper() %>% add_files(paste0(sample_path(), "/root_file.txt")) %>% nrow)
  expect_equal(5, fduper() %>%
                 add_files(list.files(sample_path(), pattern = "*.txt", full.names = TRUE, recursive = TRUE)) %>%
                 nrow)
  expect_equal(0, fduper() %>% add_files(sample_path()) %>% nrow)
})

test_that("add_dirs", {
  expect_equal(10, fduper() %>% add_dirs(sample_path()) %>% nrow)
  expect_equal(0, fduper() %>% add_dirs(paste0(sample_path(), "/root_file.txt")) %>% nrow)
  expect_equal(5, fduper() %>% add_dirs(sample_path(), pattern=".txt$") %>% nrow) # this isn't obvious
})

test_that("remove_dirs", {
  expect_equal(0, fduper() %>%
                 add_files(paste0(sample_path(), "/root_file.txt")) %>%
                 add_files(paste0(sample_path(), "/root_file.txt")) %>%
                 remove_files(paste0(sample_path(), "/root_file.txt")) %>%
                 nrow)
  expect_equal(9, fduper() %>% add_dirs(sample_path()) %>%
                 remove_files(paste0(sample_path(), "/various/file")) %>%
                 add_files(list.files(sample_path(), pattern = "*.txt", full.names = TRUE, recursive = TRUE)) %>%
                 nrow)
})
