context("cols_class")

## TODO: Add more tests


df <- data.frame(int_col = 1L, num_col = 1,
                 chr_col1 = "1", logi_col = TRUE,
                 fctr_col = factor("1"),
                 chr_col2 = "2",
                 stringsAsFactors = FALSE)

find_chr <- cols_class("character")
find_fchr <- cols_class(c("character", "factor"))
find_messy <- cols_class(c("character", "factor"), "character", "character",
                         c("numeric", "factor"))
find_clean <- cols_class("character", "factor", "numeric")

find_missing_partial <- cols_class("character", "raw", "complex")
find_missing_all     <- cols_class("raw", "complex")


# tests -------------------------------------------------------------------


test_that("multiple columns of the same type are identified", {
  expect_true(all(find_chr(df) %in% c("chr_col2", "chr_col1")), TRUE)
})


test_that("duplicate inputs are handled as if unique, in order", {
  expect_equal(find_messy(df), find_clean(df))
})


test_that("missing classes in df along are handled as long as one targ is found", {
  res <- c("chr_col1", "chr_col2")
  expect_equal(find_missing_partial(df), res)
})

test_that("entirely missing class(es) in df throws an error", {
  expect_error(find_missing_all(df))
})

