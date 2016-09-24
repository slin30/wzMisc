context("count_sigfigs")


# test data load ----------------------------------------------------------

tst_path <- "../testdata/count_sigfigs.txt"
#tst_path <- "./tests/testdata/count_sigfigs.txt" #for local loading only
datclasses <- scan(tst_path, what = "character",
                   skip = 1, nlines = 1,
                   sep = ",", strip.white = TRUE
)
datclasses <- gsub("#Classes", "", datclasses)
datclasses <- gsub(" ", "", datclasses)

tst_dat <- read.table(tst_path, sep = ",",
                      header = TRUE, colClasses = datclasses,
                      stringsAsFactors = FALSE
)


# tests -------------------------------------------------------------------

test_that("countTrailing=TRUE fails with non-chr input", {
  x_num <- c(01.00, 1.010, 01E01)

  expect_error(count_sigfigs(x_num, countTrailing = TRUE))

})

test_that("All NA input throws error", {
  expect_error(count_sigfigs(c(NA, NA)))

})

test_that("All NA input throws error with countTrailing=TRUE", {
  expect_error(count_sigfigs(c(NA, NA), countTrailing = TRUE))

})

test_that("NA coercion throws a warning", {
  x   <- c(NA, 1, "NA", 0, "a", 1.0)

  expect_warning(count_sigfigs(x))

})

test_that("NA with non-NA input returns NA only for invalid or NA input", {
  x   <- c(NA, 1, "NA", 0, "a", 1.0)
  res <- c(NA, 1, NA, 0, NA, 1)

  expect_equal(suppressWarnings(count_sigfigs(x)), res)

})

test_that("result_zeroF and tst are NOT identical when digits = 9 and countTrailing=FALSE", {
  ref <- tst_dat$result_zeroF
  tst <- count_sigfigs(tst_dat[["value"]], digits = 7, countTrailing = FALSE)

  expect_false(identical(ref, tst))

})

test_that("result_zeroF and tst are identical when digits = 9 and countTrailing=FALSE", {
  ref <- tst_dat$result_zeroF
  tst <- count_sigfigs(tst_dat[["value"]], digits = 9, countTrailing = FALSE)

  expect_true(identical(ref, tst))

})

test_that("result_zeroF and tst are NOT identical when digits = 7 and countTrailing=TRUE", {
  ref <- tst_dat$result_zeroT
  tst <- count_sigfigs(tst_dat[["value"]], digits = 7, countTrailing = TRUE)

  expect_false(identical(ref, tst))

})

test_that("result_zeroF and tst are identical when digits = 9 and countTrailing=TRUE", {
  ref <- tst_dat$result_zeroT
  tst <- count_sigfigs(tst_dat[["value"]], digits = 9, countTrailing = TRUE)

  expect_true(identical(ref, tst))

})

