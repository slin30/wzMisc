#' Extract a function by name with or without namespace prefix
#'
#' @param fun A character vector of length one, denoting the function to extract. May be
#'        prefixed with a namespace (or not).
#'
#' @return
#' A function matching \emph{fun}, or \code{NULL} (with a message) if an error.
#'
#' @details
#' Most useful to create one-off functions where one wishes to call a specific function without
#' loading the library, and also to access non-exported functions. See examples.
#'
#' @note
#' See (and heed) documentation within \code{\link[utils]{getFromNamespace}}, which this function
#' uses.
#'
#' @export
#'
#' @examples
#' getFun("getFun")
#' getFun("getFun()") # paren pairs are stripped if included
#' getFun("stats::acf()")
#' getFun("stats:::[[.dendrogram")
getFun <- function(fun) {

  if(!is.character(fun) || length(fun) != 1L) {
    stop("fun must be a quoted character of length 1")
  }

  fun <- sub("(\\(\\)$)", "", fun)

  if(grepl("(?<=\\D)+:{2,}", fun, perl = TRUE)) {
    fun_split <- strsplit(fun, "(?<=\\D)+:{2,}", perl = TRUE)
    fun_envir <- fun_split[[1]][[1]]
    fun_name  <- fun_split[[1]][[2]]
    return(getFromNamespace(fun_name, ns = fun_envir))
  }

  tryCatch(
    get(fun),
    error = function(e) {
      message(e,
              "Perhaps library not loaded? Try appending the package namespace",
              "\n  i.e. 'package_where_fun_is_exported::", fun)
    },
    finally = NULL
  )

}
