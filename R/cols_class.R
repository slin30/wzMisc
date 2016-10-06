#' Create function(s) to identify column by class
#'
#' @param ... vector of (quoted) column classes to identify
#'
#' @details For >1 class, \code{...} accepts either a vector of target classes or multiple
#' vectors of length \code{1}, or a combination. See examples.
#'
#' Note that as long as one of the classes in \code{...} is found in \emph{x}, the closure
#' output will return a value. If no clases in \code{...} are found in \emph{x}, the closure
#' output will throw an error.
#'
#' @return A function that identifies columns in \emph{x} according to \code{...}
#' @export
#'
#' @examples
#' df <- data.frame(int_col = 1L, num_col = 1,
#' chr_col = "1", logi_col = TRUE,
#' fctr_col = factor("1"),
#' chr_col2 = "2",
#' stringsAsFactors = FALSE)
#'
#' find_chr <- cols_class("character")
#' find_fct <- cols_class("factor")
#' find_chr.fct <- cols_class(c("character", "factor"))
#' find_chr.fct_alt <- cols_class("character", "factor") #same

cols_class <- function(...) {
  function(x) {
    classlst <- lapply(x, class)
    cols_filt <- vapply(classlst, function(f) any(f %in% c(...)), logical(1))
    if(all(!cols_filt)) {
      stop("No columns meet critiera")
    }

    names(x)[cols_filt]
  }
}
