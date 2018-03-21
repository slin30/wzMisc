#' Clean column names from a data.frame or chr vector
#'
#' Remove non-alphanum, replace whitespace with uscore, optionally lower, dedupe
#'
#' @param x A character vector or a data.frame. If the latter, will extract names
#' @param lower Lowercase the output? Defaults to \code{TRUE}
#'
#' @details
#' This is a simple utility that replaces any non-alphanumeric characters in \emph{x} with an
#' underscore, ensuring that runs of underscores > 1 are truncated to length 1, stripping any
#' trailing underscores, and optionally lowercasing the output (default \code{TRUE}).
#'
#' Furthermore, any duplicated values after the above are handled via \code{make.unique()}
#'
#' @return
#' A character vector of cleaned-up names. Any duplicated names are handled by \code{make.unique()}.
#' @export
#'
#' @examples
#' x <- c("This is (a dirty) name  with ", "A", "a")
#' fix_tbl_names(x)
#' fix_tbl_names(x, lower = FALSE)
#'
#' xdf <- data.frame(1:10, LETTERS[1:10], letters[1:10])
#' names(xdf) <- x
#' fix_tbl_names(xdf)
fix_tbl_names <- function(x, lower = TRUE) {

  if(is.data.frame(x)) {
    x <- names(x)
  }

  stopifnot(is.character(x))

  x_nospec <- gsub("\\W", "_", x)
  x_clean  <- gsub("_{2,}", "_", x_nospec)

  out <- sub("_$", "", x_clean)
  if(lower) {
    out <- tolower(out)
  }

  if(anyDuplicated(out)) {
    out <- make.unique(out)
  }

  out
}
