#' Count number significant figures
#'
#' Count number of significant digits (figures), option to include trailing zeros
#'
#' @importFrom magrittr %>%
#' @param x numeric or character vector; only character input is allowed if \code{countTrailing=TRUE}.
#' @param digits integer of length 1. Optional. Default uses value from \code{getOptions("digits")}. Will
#' warn if this is set higher than 15, for good reason. See notes.
#' @param countTrailing logical. Should trailing zeros be counted? Defaults to \code{FALSE}.
#' @details
#' Count number of significant digits, either from a (measurement) precision or a (mathematical) numeric
#' perspective via the \emph{countTrailing} option. When set to \code{TRUE} and provided a \code{character} input
#' \emph{x}, this option will count trailing zeros as significant digits. Leading zeros are (almost) never counted; the
#' single exception is when \code{countTrailing=TRUE} and \emph{x} contains values that would be coerced to \code{0}
#' via e.g. \code{as.integer} or \code{as.numeric}; see Notes.
#'
#' By default, \code{countTrailing=FALSE}; this counts significant digits from a (mathematical) numeric perspective.
#' Setting to \code{TRUE} counts significant digits from a (measurement) precision perspective.
#'
#' Because \emph{R} (and most other analysis tools/languages) will automatically strip trailing zeros for
#'  \code{integer} or \code{numeric} data types, this function will throw an error if \code{countTrailing=TRUE} and
#'  \code{class(x) != "character"}.
#'
#' This function does not perform any explicit rounding or truncation of input within reasonable limits;
#' if input precision exceeds system limit, standard R rounding rules will apply, silently.
#' @note
#' Zero is a special case. If \code{countTrailing=FALSE}, will return zero. If \code{countTrailing=TRUE}, all
#' digits are counted, excluding digits following the exponent notation (\code{e} or \code{E}) if applicable. See examples
#'
#' This function assumes that if you provide character inputs that would evaluate to \code{0} if treated as a number, this is
#' in fact meaningful from a precision/measurement perspective.
#' @return
#' An integer vector of significant digit counts equal in length to input vector.
#' @export
#' @examples
#' set.seed(1)
#' x <- Map(signif, rnorm(10), 1:10)
#' count_sigfigs(x)
#'
#' #Also works for scientific notation
#' count_sigfigs(c(1E10, -1E10, -000001E-10)) #all 1
#' count_sigfigs(c("1.0001", 1.0001, 0001.0001)) #all 5
#'
#' #digits option
#' #By default, will use local number of digits, which is 7
#' getOption("digits") #default should be 7
#' count_sigfigs(0.1234567) #7
#' count_sigfigs(0.12345678) #8
#' count_sigfigs(0.123456789) #8
#' count_sigfigs(0.123456789, digits = 8) #9
#'
#' #countTrailing
#' count_sigfigs("1.0100", countTrailing = TRUE)
#' count_sigfigs("1.0100E10", countTrailing = TRUE)
#'
#' #zero handling
#' zeros_decimal  <- c("0", "0.0", "00.0", "00.00", "0.000")
#' zeros_exponent <- c("0E0", "0.0E0", "00.0E0", "00.00E0", "0.000E0")
#' count_sigfigs(zeros_decimal) #all zero
#' count_sigfigs(zeros_decimal, countTrailing = TRUE) #1,2,3,4,4
#' count_sigfigs(zeros_exponent) #all zero
#' count_sigfigs(zeros_exponent, countTrailing = TRUE) #also 1,2,3,4,4
#'
#' \dontrun{
#' #compare:
#' count_sigfigs(123.12345679, digits = 7)  #need higher digits option
#' count_sigfigs(123.12345679, digits = 15) #what you expected
#' count_sigfigs(123.12345679, digits = 16) #probably not what you expected
#'
#' #also
#' count_sigfigs(1000000001.12345, digits = 15) #pushing it
#' }
count_sigfigs <- function (x, digits = getOption("digits"), countTrailing = FALSE) {
  if(all(is.na(x))) {
    stop("All inputs are NA")
  }
  if (countTrailing && !is.character(x)) {
    stop(paste("countTrailing is only possible with chr inputs of x"))
  }
  if (digits > 15 ) {
    warning(paste("Setting digits above 15 is dangerous",
                  "and results may be inaccurate.", "Proceed at your own risk!"))
  }

  #Capture NA input
  na_input <- is.na(x) | is.na(as.numeric(x))
  #prepare to handle all zeros downstream
  xtype       <- class(x)
  digitCounts <- stringr::str_extract(x, "\\d*\\.?\\d*") %>% stringr::str_count("\\d")
  zeroCheck   <- as.numeric(x) == 0
  zeroCheck[is.na(zeroCheck)] <- FALSE

  if (!countTrailing) {
    extra <- 0
  }
  if (is.character(x)) {
    #strip E or e
    x <- stringr::str_replace(x, "^-?\\$?(\\d+\\.?\\d*)(?=E?|e?)(.*)", "\\1")
    if (!countTrailing) {
      x <- as.numeric(x)
    }
    else {
      #Reverse clean string first, then split (V2 is before decimal, V1 after)
      x_rev <- sapply(x, function(f) paste(rev(substring(f, 1:nchar(f), 1:nchar(f))), collapse = ""))
      xmat <- stringr::str_split_fixed(x_rev, "\\.", 2)
      #transform blank in V2 to NA
      xmat[, 2] <- .helper_blankToNA(xmat[, 2])
      #Count zeros
      xmat_true <- apply(xmat, 2, function(f) .helper_zeroCount(f))
      xmat_true <- matrix(xmat_true, ncol = 2) #handle single input coercion
      xmat_true[, 1] <- .helper_zeroToNA(xmat_true[, 1]) #prevent extra counts

      extra <- apply(xmat_true, 1, function(f) sum(f))
      extra[is.na(extra)] <- 0

      x <- as.numeric(x) #and transform back to numeric after capturing extra
    }
  }
  if (is.numeric(x)) {
    digits <- as.integer(digits)
    x      <- sprintf(paste0("%0.", digits, "e"), x)
  }
  targ   <- stringr::str_replace(x, "^-?\\$?(\\d+\\.?\\d*)(?=E?|e?)(.*)", "\\1")
  splits <- stringr::str_match(targ, "^0*([0-9]*)\\.?([0-9]*[^0$])") %>%
    apply(X = ., 2, list) %>% unlist(recursive = FALSE)

  splits_nch <- lapply(splits, function(f) stringr::str_count(f, "[0-9]"))
  out        <- splits_nch[[2]] + splits_nch[[3]] + extra

  if(xtype == "character" && countTrailing) {
    out[zeroCheck] <- digitCounts[zeroCheck]
  }

  out[na_input] <- NA
  as.integer(out)

}

.helper_blankToNA <- function(x) {
  x[x==""] <- NA
  x
}

.helper_zeroToNA <- function(x) {
  x[x==0] <- NA
  x
}

.helper_zeroCount <- function(x) {
  n <- stringr::str_extract(x, "^0?0*")
  nchar(n)
}


