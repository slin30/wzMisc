#' Return the first non-missing value from two or more atomic vectors
#'
#' Given two or more atomic vectors, fold each vector element-wise left-to-right, carrying forward
#' the first non-missing value
#'
#' @param ... Atomic vectors or a \code{list} containing atomic vectors of the same type and length.
#' Inputs of length \code{1} are allowed and potentially recycled to the length of the longest input,
#' but this behavior varies depending on the input order. See details.
#'
#' @details
#' This is very similar to SQL's \code{coalesce} function, but is intentionally named to avoid clashing
#' with the \code{dplyr} implementation. This is strictly written in base \code{R} and follows the same
#' class-handling rules as implemented by \link[base]{ifelse}.
#'
#' The function will exit when it detects that no additional \code{NA} values are present or when all
#' inputs have been evaluated, whichever comes first. If you supply e.g. a non-missing vector of length 1
#' as the first input, then the function will only ever return that supplied value because the one of the
#' two aforementioned conditions will have been satisfied. See examples
#'
#' @return
#' An atomic vector of the same type as the inputs (although any class attributes will be stripped). The
#' length will be either \code{1} if the first supplied input is a non-missing atomic vector of length 1
#' or the length of the inputs -- assuming input lengths are otherwise equivalent (but greater than 1).
#'
#' @note
#' The non-exported helper function \code{.fold_na_base()} has slightly different behavior when handling
#' vectors of length 1 combined with vectors of length > 1, regardless of argument order. The helper
#' ensures outputs are always the length of the longest input(s). If this is desirable, then you can
#' effectively achieve the same result by calling the helper along with e.g. \link[base]{Reduce}. If so, make
#' sure you pass the inputs as a (flat) \code{list}.
#'
#' @export
#'
#' @examples
#' x <- 1:10L
#' y <- c(1L, NA, 3L, NA, 5L, NA, 7L, NA, 9L, NA)
#' z <- c(NA, 2L, NA, 4L, NA, 6L, NA, 8L, NA, 10L)
#'
#' fold_na(x) # returns x
#'
#' fold_na(y, z) # regenerates x
#' fold_na(list(y, z)) # same
#'
#' # works the same on data.frames
#' df <- data.frame(y, z, x)
#' fold_na(df)
#' fold_na(df[, c("y", "z")])
#'
#' # These are not equivalent
#' fold_na(y, 0L) # NA in y substituted with 0
#' fold_na(0L, y) # only returns zero as vector of length 1!

fold_na <- function(...) {

  l = c(...)

  if(!is.list(l)) {
    l = list(...)
  }

  nremain <- length(l)

  x = l[[1]]

  if(nremain == 1L || !anyNA(x)) {
    if(is.atomic(x)) {
      return(x)
    }
  }

  y = l[[2]]

  if(!is.atomic(x) || !is.atomic(y)) {
    stop("x and y must both be atomic")
  }

  res = .fold_na_base(x, y)

  l_new <- c(list(res), l[-c(1:2)])

  fold_na(l_new)

}

NULL

# helper
.fold_na_base <- function(x, y) {

  v <- list(x, y)

  # this is redundant since main FUN performs this now, but less code to maintain
  #  to keep this here so we can call the base version if needed
  is_all_atomic <- all(vapply(v, is.atomic, logical(1)))
  if(!is_all_atomic) {
    stop(
      "x and y must both be atomic"
    )
  }

  len_x <- length(x)
  len_y <- length(y)

  any_single <- len_x == 1L | len_y == 1L
  is_same_length <- len_x == len_y

  if(!is_same_length && !any_single) {
    stop(
      "x and y must either be the same length or at least one must be of length 1;
         arbitrary recycling not supported"
    )
  }

  is_same_class <- Reduce(identical, vapply(v, typeof, character(1)))
  if(!is_same_class) {
    stop(
      "x and y must be the same type;\n",
      "x is ", typeof(x), " and ",
      "y is ", typeof(y)
    )
  }

  # if we get to this point, we can do some actual work

  # expand to longest if single and not is_same_length
  if(!is_same_length && any_single) {
    nmax <- max(len_x, len_y)
    if(len_x != nmax) {
      x <- rep(x, times = nmax)
    } else {
      y <- rep(y, times = nmax)
    }

  }

  ifelse(is.na(x), y, x)
}
