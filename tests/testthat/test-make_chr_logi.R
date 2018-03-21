context("make_chr_logi")

# base test vector
x <- c("yes", "YES", NA, "no", "NO", "NA", "0")


# Positive controls, silent -----------------------------------------------

test_that(
  "simple input works", {
    expect_equal(
      make_chr_logi(x, c("yes", "YES")),
      c(TRUE, TRUE, NA, FALSE, FALSE, FALSE, FALSE)
    )
  }
)

test_that(
  "More complex examples work, silently", {
    expect_equal(make_chr_logi(x, c("yes", "YES"), preserve_NA = FALSE, NA_as_what = FALSE),
                 c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(make_chr_logi(x, c("yes", "YES"), preserve_NA = FALSE, NA_as_what = TRUE),
                 c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(make_chr_logi(x, c("yes", "YES", NA), preserve_NA = TRUE, NA_as_what = FALSE),
                 c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
    expect_equal(make_chr_logi(c(NA_character_, "A"), "", preserve_NA = FALSE, NA_as_what = TRUE),
                 c(TRUE, FALSE))
  }
)


# Positive controls, with warnings ----------------------------------------

test_that(
  "A warning is raised when NA_as_what is not NA but preserve_NA is TRUE", {
    res <- c(TRUE, TRUE, NA, FALSE, FALSE, FALSE, FALSE)
    expect_warning(
      make_chr_logi(x, c("yes", "YES"), preserve_NA = TRUE, NA_as_what = TRUE),
      "'NA_as_what' is ignored when 'preserve_NA' is TRUE"
    )

    expect_equal(
      suppressWarnings(make_chr_logi(x, c("yes", "YES"), NA_as_what = TRUE)),
      res
    )
  }
)

test_that(
  "A warning is raised when NA_as_what is NA but preserve_NA is FALSE", {
    res <- c(TRUE, TRUE, NA, FALSE, FALSE, FALSE, FALSE)
    expect_warning(
      make_chr_logi(x, c("yes", "YES"), preserve_NA = FALSE, NA_as_what = NA),
      "'preserve_NA' is FALSE, but 'NA_as_what' is still NA; NA will still be passed through"
    )

    expect_equal(
      suppressWarnings(make_chr_logi(x, c("yes", "YES"), preserve_NA = FALSE, NA_as_what = NA)),
      res
    )
  }
)

test_that(
  "A warning is raised when NA is included within val_as_TRUE", {
    res <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
    expect_warning(
      make_chr_logi(x, c("yes", "YES", NA), preserve_NA = FALSE, NA_as_what = NA),
      "'NA' included within val_as_TRUE; this overrides any options passed to 'preserve_NA' or 'NA_as_what'; NA will be set to TRUE"
    )

    expect_equal(
      suppressWarnings(make_chr_logi(x, c("yes", "YES", NA), preserve_NA = FALSE, NA_as_what = NA)),
      res
    )
  }
)


# Negative controls -------------------------------------------------------

test_that(
  "Non-character and invalid length val_as_TRUE inputs raise an error", {
    expect_error(make_chr_logi(c(NA_character_, "A"), val_as_TRUE = 0L),
                 "'val_as_TRUE' must be a character vector of length >= 1")
    expect_error(make_chr_logi(c(NA_character_, "A")),
                 "'val_as_TRUE' must be a character vector of length >= 1")
  }
)

test_that(
  "Non-character and invalid length x inputs raise an error", {
    expect_error(make_chr_logi(c(0L, 1L)),
                 "is.character\\(x\\) is not TRUE")
    expect_error(make_chr_logi(NULL),
                 "is.character\\(x\\) is not TRUE")
  }
)

test_that(
  "All NA inputs to x raise an error", {
    expect_error(make_chr_logi(x = rep(NA_character_, 4)),
                 "Only NA values found in input")
  }
)
