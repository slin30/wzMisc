#' Append a combination of head and tail elements to x
#'
#' Create a combination of patterned outputs of x based on head and/or tail elements
#'
#' @param x A vector of values you wish to append to; will be coerced to \code{character} if not.
#' @param head (optional) A vector of values you wish to append to the head of x.
#' @param tail (optional) A vector of values you wish to append to the tail of x.
#'
#' @return
#' A \code{list} of length \emph{x}, with elements the length of the cartesian product of
#' \code{length(head)} and \code{length(tail)}.
#'
#' @export
#'
#' @examples
#' x <- replicate(10, paste(sample(letters, 3), collapse = ""))
#' append_combn(x, head = ":", tail = "-")
#' append_combn(x, head = c(":", "|"), tail = "-")
#' append_combn(x, head = "_A_")
#' append_combn(x, head = paste0(LETTERS[1:10], "_"), tail = c("-", "_"))
append_combn <- function(x, head = NULL, tail = NULL) {

  if(is.null(head) && is.null(tail)) {
    return(x)
  }

  if(!is.character(x)) {
    x <- as.character(x)
  }

  if(!is.null(head)) {
    x <- outer(head, x, FUN = paste0)
  }

  if(!is.null(tail)) {
    x <- outer(x, tail, FUN = paste0)
  }

  x_flat <- apply(x, 2, c)
  if(is.null(dim(x_flat))) {
    return(x_flat)
  }
  x_list <- apply(x_flat, 2, list)
  Map(unlist, x_list, recursive = FALSE)
}
