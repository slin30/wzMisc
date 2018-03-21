#' Coerce one or values in a character vector to logical TRUE
#'
#' Transform a character into logical with denoted values as TRUE, with NA handling options
#'
#' @param x Input character vector to coerce to logical
#' @param val_as_TRUE A character vector containing the values to coerce to TRUE. Including \code{NA} will
#' override both \emph{preserve_NA} and \emph{NA_as_what}
#' @param preserve_NA Should \code{NA} be preserved? Acts as a safety flag, and \emph{NA_as_what} is ignored
#' unless this is set to \code{FALSE}; defaults to \code{TRUE}
#' @param NA_as_what How should \code{NA} be handled? One of \code{TRUE, FALSE, NA}; defaults to \code{NA}.
#' Ignored if \emph{preserve_NA} is \code{TRUE}
#'
#' @details
#' This function is most useful for its explicit handling of \code{NA}. By default, \code{NA} is
#' passed-through. If \code{NA} is included within \emph{val_as_TRUE}, then all \code{NA} will be
#' set to \code{TRUE}. Alternatively, if \emph{preserve_NA} is set to \code{FALSE} and \emph{NA_as_what}
#' is set to either \code{TRUE} or \code{FALSE}, \code{NA} will be set as requested.
#'
#' Non-character inputs to either \emph{x} or \emph{val_as_TRUE} will raise an error. Furthermore,
#' if \emph{x} is all \code{NA}, an error is raised.
#'
#' @return
#' A logical vector of length \emph{x}
#' @export
#'
#' @examples
#' x <- c("yes", "YES", NA, "no", "NO", "NA", "0")
#' make_chr_logi(x, c("yes", "YES"))
#' make_chr_logi(x, c("yes", "YES", NA)) # overrides preserve_NA or NA_as_what
make_chr_logi <- function(x, val_as_TRUE = NULL, preserve_NA = TRUE, NA_as_what = NA) {

  stopifnot(is.character(x))

  u.check <- length(unique(x[!is.na(x)]))
  if(u.check == 0L) {
    stop("Only NA values found in input")
  }

  if(length(val_as_TRUE) == 0L || !is.character(val_as_TRUE)) {
    stop("'val_as_TRUE' must be a character vector of length >= 1")
  }

  # if NA is included within val_as_TRUE, then preserve_NA and NA_as_what should be ignored
  if(anyNA(val_as_TRUE) && !preserve_NA) {
    warning("'NA' included within val_as_TRUE; this overrides any options passed to ",
            "'preserve_NA' or 'NA_as_what'; NA will be set to TRUE")
    preserve_NA <- FALSE
    NA_as_what <- TRUE
  }

  if(length(NA_as_what) > 1L || !is.logical(NA_as_what)) {
    stop("'NA_as_what' must be a length 1 logical vector")
  }

  if(preserve_NA && !is.na(NA_as_what) && !anyNA(val_as_TRUE)) {
    warning(
      "'NA_as_what' is ignored when 'preserve_NA' is TRUE"
    )
  }

  if(!preserve_NA && is.na(NA_as_what)) {
    warning(
      "'preserve_NA' is FALSE, but 'NA_as_what' is still NA; NA will still be passed through"
    )
  }

  out <- rep(NA, length = length(x))

  x_to_TRUE <- x %in% val_as_TRUE
  x_to_FALSE <- !x %in% val_as_TRUE & !is.na(x)


  out[x_to_TRUE] <- TRUE
  out[x_to_FALSE] <- FALSE
  if(!preserve_NA & !is.na(NA_as_what)) {
    out[is.na(out)] <- NA_as_what
  }

  out

}
