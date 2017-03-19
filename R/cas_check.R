#' Check CAS format and checksum
#'
#' One-step check for CAS, via format and checksum; optionally preprocess input
#'
#' @family cas_functions
#'
#' @param x chr. A vector of values to check. Standard CAS notation using hyphens is fine, as
#' all non-digit characters are stripped for checksum calculation
#' @param preprocess logi. Trim leading and trailing whitespace and pare all consecutive (\code{-{2,}})
#' to (\code{-})? Defaults to \code{TRUE}
#' @param removeWS logi. Should all whitespace, be removed? Defaults to \code{TRUE}
#'
#' @details
#' This is mainly a convenience function that combines \code{\link{cas_detect}} and \code{\link{cas_checkSum}} with
#' an additional parameter to handle whitespaces. The use case for \code{cas_detect} is broader than this function, and
#' therefore an argument to explicitly handle whitespaces is not included there, but is here.
#'
#' Do note that because this implements both format and checksum checks, it is more stringent than either standalone function,
#' which may or may not be desirable. Please see the related functions if you require more granular control.
#'
#' @note
#' The standalone functions are natively vectorized, and since this encompasses the full functionality of
#' \code{\link[webchem]{is.cas}}, it is arguably a \emph{vectorized} alternative.
#'
#' @return
#' A \code{logical} vector of length \emph{x} denoting whether each \emph{x} is a valid CAS by both
#' format and the last-digit checksum calculation \code{NA} input values will remain \code{NA}.
#' @export
#'
#' @examples
#' # preprocessing demo
#' x_casOnly <- c("598-42-5", "19438-61-0", "20730-39-6") # all 'real' CAS RNs
#' x_messy   <- c("casrn:  598-42-5", "19438-61 - 0", "20730--39-6")
#' cas_check(x_casOnly, preprocess = FALSE) # already known good, preprocess not needed
#' cas_check(x_messy, preprocess = FALSE) # removeWS not enough to handle '--'
#' # NA handling
#' cas_check(c(NA, x_messy, NA)) # NA passed through
#' # NULL handling
#' cas_check(c(NA, NULL, x_messy, NULL, NA)) # NULL removed, length == length(x)-length(is.null(x))
cas_check <- function(x, preprocess = TRUE, removeWS = TRUE) {

  if(removeWS) {
    x <- gsub("\\s", "", x)
  }

  # The first check via cas_detect
  fmt_chk <- wzMisc::cas_detect(x, preprocess = preprocess, output = "check")

  fmt_good <- which(fmt_chk[TRUE] & !is.na(fmt_chk))
  fmt_bad  <- which(!fmt_chk & !is.na(fmt_chk))

  # subset into this for output
  out <- rep(NA, length(x))

  # The second check, using subset that pass first
  # Note checkLEN not needed since format checking upstream is more comprehensive
  chsum_chk <- wzMisc::cas_checkSum(x[fmt_good], checkLEN = FALSE)

  # subset output with x that pass both checks
  out[fmt_good] <- chsum_chk

  #handle FALSE here
  if(length(fmt_bad) > 0L) {
    out[fmt_bad]  <- FALSE
  }
  out
}
