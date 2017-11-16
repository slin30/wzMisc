#' Make a lookup vector consisting of unique elements after applying fun
#'
#' For a vector x, apply fun and output result, named by unique x
#'
#' @param x (required) An input atomic vector
#' @param dropNA Should \code{NA} be excluded, post-uniquification? Defaults to \code{TRUE}
#' @param fun A character vector of length 1, denoting the function to apply to \code{x}
#' @param ... Additional (ideally named) arguments to pass to \code{fun}, if applicable
#'
#' @details
#' This is a convenient way to create lookup vectors, that can be used to transform the
#' original vector via character subscripting, which can be more efficient (in some cases
#' significantly) than applying the transformation on the input directly.
#'
#' The (any) improvement in performance is highly dependent on the complexity of the
#' operation (\code{fun} and any parameters defined within \code{...}), as well as the length and
#' cardinality of \code{x}.
#'
#' @note
#' \code{fun} should be passed as an explicitly named argument, i.e. \code{fun="gsub"} due to the
#' \code{dropNA} order. Furthermore, it may be necessary to also explicitly pass \code{x} in the input
#' within \code{...} if you wish to call a function (\code{fun}) that does not accept \code{x} as
#' the first argument, AND where the argument name is something other than \code{x}.
#'
#' @return
#' A named vector the length of \code{unique(x)}, or \code{length(unique(x[!is.na(x)]))}
#' if \code{dropNA==TRUE}. Additionally, a message communicating the reduction in
#' cardinality. If no reduction in cardinality was detected, i.e. \code{x} was already
#' entirely unique, then a message stating that this function effectively has no benefit.
#' @export
#'
#' @examples
#' # an ideal scenario
#' set.seed(10)
#' vec_x <- rep(
#'   replicate(1E4,
#'             paste0(
#'               paste(sample(LETTERS[1:4], 3, replace = TRUE), collapse = ""),
#'               sample(100L:1000L, 1)
#'             )),
#'   200
#' )
#'
#' # using a lookup table to subscript
#' system.time(
#'   via_subscript <- make_ref_unique(vec_x, fun = "sub", pattern = "ABC", replacement = "")[vec_x]
#' )
#' # versus direct application
#' system.time(via_direct <- sub(x = vec_x, pattern = "ABC", replacement = ""))
#'
#' # check
#' identical(unname(via_subscript), via_direct)
make_ref_unique <- function(x, dropNA = TRUE, fun, ...) {

  if(!is.atomic(x)) {
    stop("x must be an atomic vector")
  }

  if(all(is.na(x))) {
    stop("x is all NA")
  }

  if(!is.character(fun) || length(fun) != 1) {
    stop("fun must be a character vector of length 1")
  }

  u <- unique(x)

  if(dropNA) {
    u <- u[!is.na(u)]
  }

  # report how much you potentially save
  reportmsg <- TRUE
  xlen <- length(x)
  lendelt <- xlen - length(u)
  rem <- xlen - lendelt

  if(lendelt == 0) {
    message("Input is all unique")
    reportmsg <- FALSE
  }

  v <- match.fun(fun)(u, ...)

  dff <- round(lendelt/length(x) * 100, 1)
  if(as.integer(dff) >= 99L) {
    dff_msg <- paste0(">99")
  } else if (as.integer(dff) <= 1L) {
    dff_msg <- paste0("<1")
  } else {
    dff_msg <- paste0(dff)
  }

  msg <- paste0("Cardinality reduced by ",
                paste0(
                  dff_msg, "%"
                ),
                " (removed ",
                lendelt, "/",
                xlen,
                ", keeping ",
                rem,
                ")"
  )

  if(reportmsg) {
    message(msg)
  }

  setNames(v, nm = u)
}
