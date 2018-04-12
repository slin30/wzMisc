#' Check ISSN validity
#'
#' Calculate the check digit of ISSNs and compare to current digit
#'
#' @param x A \code{character} vector of ISSNs to check
#'
#' @details
#' Calculates the check digit of one or more input fundamentally-formatted ISSNs.
#' Fundamentally formatted refers to the input after whitespace-stripping, and only
#' keeping alphanumeric values Additionally, the final character checksum is performed
#' on a case-normalized basis.
#'
#' This means that input ISSNs can be quite ill-formatted and still be evaluated for
#' validity, and test as valid. See example.
#'
#' @return
#' A \code{list} of length 2 containing a data.frame of results and any ISSNs
#' that could not be checked due to invalid format; if no inputs are invalid, then
#' \code{NULL} for the latter.
#'
#'
#' @export
#'
#' @examples
#' # Note that dashes and whitespaces are
#' test_issns <- c("10543589", "00657743", "0091679X",
#'                 "1054-3589", "0065-7743", "0091-679X",
#'                 "1054--3589", " 0065-7743", "0091--679x-",
#'                 "0091679xx", "0091x79x", "0091X679")
#' check_issn(test_issns)
check_issn <- function(x) {

  if(!is.character(x)) {
    stop("x must be a character vector")
  }

  x_clean <- gsub("\\W", "", x)

  # first pass check on length
  x_invalidlen <- nchar(x_clean) != 8L
  if(all(x_invalidlen)) {
    stop("No valid length inputs detected")
  }

  # second pass on clean
  x_invalidsub <- suppressWarnings(
    is.na(
      as.integer(
        substr(x_clean, 1, 7)
      )
    )
  )
  if(all(x_invalidsub)) {
    stop("No valid format inputs detected")
  }

  # now can process
  testable <- !x_invalidlen & !x_invalidsub
  x_err <- x[!testable]
  if(length(x_err) == 0L) {
    x_err <- NULL
  }

  x_split <- strsplit(x_clean[testable], split = "", fixed = TRUE)
  x_mat <- do.call(rbind, x_split)
  # capture current check digit, for the clean input
  check_digit_curr <- toupper(x_mat[, 8])
  # integer matrix for calc checks
  x_mat_calc <- matrix(
    as.integer(x_mat[, 1:7]),
    nrow = length(x_split)
  )
  rownames(x_mat_calc) <- x[testable]
  colnames(x_mat_calc) <- paste0("S", seq_len(ncol(x_mat_calc)))

  x_mat_mult <- t( t(x_mat_calc) * seq(8L, 2L))

  rsum <- rowSums(x_mat_mult)
  modu <- rsum %% 11

  res <- rep(NA_character_, length(modu))
  res[modu == 0] <- "0"
  res[modu != 0] <-11 -  modu[modu != 0]
  res[res == "10"] <- "X"

  out <- data.frame(
    issn_orig = rownames(x_mat_calc),
    digit_orig = check_digit_curr,
    digit_new = res,
    check = res == check_digit_curr,
    stringsAsFactors = FALSE
  )

  list(
    result = out,
    invalid = x_err
  )

}
