#' Generate a function that calculates a hash from one or more columns
#'
#' Make functions that calculate hashed keys from the collapsed values of one or more columns in a data.frame or data.table
#'
#' @importFrom openssl md5
#'
#' @param nms A vector of names to hash. The output function will check that
#'        all \code{nms} are present in the \code{data.frame} or \code{data.table}.
#' @param as_char Should the output be coerced to \code{character} or kept as
#'        class \code{hash}? Defaults to \code{TRUE}
#'
#' @details
#' This function is meant to condense information for each row into a compact, meaningful
#' md5 hash, using the \code{\link[openssl]{md5}} function. The outputs can be useful keys to compare
#' across tables where a primary key is not readily available.
#'
#' Collisions are of course possible, but the chances of this happening should be exceedingly
#' rare.
#'
#' @return
#' A vector of hashed values based on input \code{nms}. By default, will be coerced to
#' \code{character}, or \code{hash} if \code{as_char=FALSE}.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#' chr = LETTERS[1:10],
#' fctr = factor(LETTERS[1:10]),
#' num = as.numeric(1:10),
#' int = 1:10L,
#' date = seq.Date(from = Sys.Date() - 9, to = Sys.Date(), by = "day"),
#' stringsAsFactors = FALSE
#' )
#'
#' f1 <- hashIDfun(names(df))
#' f2 <- hashIDfun(names(df), as_char = FALSE)
#'
#' x1 <- f1(df)
#' x2 <- f2(df)
#'
#' all.equal(x1, x2, check.attributes = FALSE) # class difference
#'
#' # order of input matters, since we are collapsing
#' f1_rev <- hashIDfun(rev(names(df)))
#' x1_rev <- f1_rev(df)
#'
hashIDfun <- function(nms, as_char = TRUE) {
  function(x) {
    if(!is.data.frame(x)) {
      stop("data.frame or data.table input required")
    }
    if(is.data.table(x)) {
      fvars <- x[, c(nms), with = FALSE]
    } else {
      fvars <- x[, c(nms)]
    }

    flist <- as.list(fvars)

    .calc_hkey(fvars, as_char = as_char)
  }
}

NULL
# md5 id generator on cols passed to ...
.calc_hkey <- function(..., as_char = TRUE) {
  to_coll <- c(...)
  coll <- do.call(paste, c(to_coll, sep = ""))
  keys <- md5(coll)
  if(as_char) {
    keys <- paste(keys)
  }
  keys
}
