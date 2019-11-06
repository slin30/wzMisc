#' Recursively uniquify a list of vectors from left (top) to right (bottom)
#'
#' Ensure each successive list element is a distinct set relative to all previous elements
#'
#' @param lst A list, typically of length > 1 with elements comprising atomic vectors
#' @param curr.iter Not used; internal counter
#' @param out Not used; result accumulator
#'
#' @details There is no type checking for list elements, so standard coercion rules apply,
#' and this may result in unexpected results. You should ensure that types are homogeneous.
#' @return A list of identical length to the input, with element lengths equal to or
#' shorter than each initial element length, depending on the degree of overlap and/or
#' if any duplication was present within each element.
#' @export
#'
#' @examples
#' lst <- list(
#'   v1 = 1:10L,
#'   v2 = 1:12L,
#'   v3 = 1:13L
#' )
#' recur_unique(lst)
recur_unique <- function(lst, curr.iter = 0L, out = list()) {

  if(!is.list(lst)) {
    stop("lst must be a list")
  }

  lst_len <- length(lst)

  curr_depth <- lst_len - curr.iter

  if(curr_depth == 0L) {
    return(out)
  }

  to_check <- lst[[lst_len - curr.iter]]
  to_fold  <- unname(unlist(lst[seq_len(lst_len - curr.iter + -1)], recursive = TRUE))

  overlap <- intersect(to_fold, to_check)
  out[[lst_len - curr.iter]] <- setdiff(to_check, overlap)

  recur_unique(lst = lst, curr.iter = curr.iter + 1L, out = out)

}
