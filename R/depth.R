#' depth
#'
#' Recursively determine depth of a list
#' @name depth
#' @param lst input list to determine depth of
#' @param d optional default starting or return value if \emph{this} is not a list; defaults to \code{0L}
#'
#' @details There are two versions of this function. \code{depth} is a recursive version, and
#' \code{depth_while} is an imperitive version. Both achieve the same end results, although one might
#' be more performant that the other, depending on the usage scenario.
#'
#' In practice, it is unlikely performance will ever be an issue with this function, as meaningful differences
#' between the two approaches will only be apparent with tens of thousands of runs, and checking list depth
#' is almost always a one-off, usually as a part of function input validation.
#' @note
#' This is mainly used as a helper function to check inputs, but generally useful enough to include as
#' a standalone
#'
#' Credit to: \url{http://stackoverflow.com/a/13433689/1270695} for \code{depth}
#'
#' @return An integer of length 1
#'
#' @examples
#' depth(list(1:3))
#' depth(list(list(1:3)))
#' depth(list(1:3, "a", list(1:2)))
#' depth(list(1:3, "a", list(list(1:2))))
NULL

#' @rdname depth
#' @export
depth <- function(lst, d = 0L) {
  if(!is.list(lst)) {
    return(as.integer(d))
  } else {
    return(as.integer(
      max(unlist(lapply(lst, depth, d = d+1L))))
    )
  }
}
NULL

#' @rdname depth
#' @export
depth_while <- function(lst, d = 0L) {
  if(!is.list(lst)) {
    return(d)
  }
  while(is.list(lst)){
    d <- d + 1L
    lst <- unlist(lst, recursive = FALSE)
    lst
  }
  as.integer(d)
}
