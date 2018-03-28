#' Collapse multiple possible matches between two data.tables
#'
#' Cascading character matching for sets of columns across two data.tables
#'
#' @import data.table
#'
#' @param dt_x A data.table where you wish to find corresponding index values in \emph{dt_y}
#' @param dt_y A data.table where matching values in \emph{dt_x} should be found, and indices from \emph{dt_y} returned
#' @param xs A chr vector of names in \emph{dt_x} to match upon.
#' Intrinsic order is potentially important, and must correspond to order and length of \emph{ys}. Non-character columns
#' will be coerced to character via \code{as.character}, with a warning.
#' @param ys A chr vector of names in \emph{dt_y} that correspond to \emph{xs} in scope and length. Non-character columns
#' will be coerced to character via \code{as.character}, with a warning.
#' @param reverse Logi. Do you wish to perform the match in reverse, that is find indices in \emph{dt_x} where corresponding
#' values in \emph{dt_y} match? Defaults to \code{FALSE}
#' @param ... Additional arguments to pass to \code{Reduce()}
#'
#' @details
#' Addresses a common merge use case between two tables where a single, robust key is not available, and one must rely on
#' one or more fields between the two tables to make a best-attempt merge. In this scenario, the order of values in \emph{xs},
#' and the corresponding order in \emph{ys} is critical, and should correspond to one's best-guess (expectation) of specificity,
#' since this function calls \code{Reduce} to collapse the list of match results into a single vector.
#'
#' If the \code{fastmatch} package is loaded, will use \code{fastmatch::fmatch} for performance, else
#' will use \code{base::match}.
#'
#' @note
#' At the moment, this function coerces both \emph{xs} and \emph{ys} to lowercase via \code{tolower}. This is likely to be an
#' option in the near future, perhaps with a default of \code{TRUE}. Additionally, \code{match} is called as
#' \code{match(x, y, nomatch = NA_integer_, incomparables = NULL)}, and this is enforced, with no plans to make optional.
#'
#' @return
#' An \code{integer} vector of length \emph{xs} or length \emph{ys} (since it is required that \code{length(xs) == length(ys)})
#' containing matching indices, else \code{NA}. The indices by default denote the positions of values in \emph{dt_y} that
#' match \emph{dt_x}, unless \code{reverse = TRUE}, in which case the reverse.
#'
#' Additionally, a console message containing match statistics.
#' @export
#'
#' @examples
#' set.seed(10)
#' # dt_x is the table you want to append to
#' dt_x <- data.table(
#'   key_a = sample(LETTERS[1:15], replace = FALSE)
#' )
#' # dt_y has one or more target fields you wish to pull, using the keys
#' # from dt_x.
#' dt_y <- data.table(
#'   col_a = unlist(Map(c, LETTERS[seq(1, 10, by = 2)], list(NA_character_))),
#'   col_b = unlist(Map(c, list(NA_character_), LETTERS[seq(2, 11, by = 2)])),
#'   targ = sample(1:100L, 10, replace = FALSE)
#' )
#' # this is used for indexing results out
#' mvec <- match_xs_ys(dt_x, dt_y, c("key_a", "key_a"), c("col_a", "col_b"))
#' # pull over results
#' dt_x[, targ := dt_y$targ[mvec]]
match_xs_ys <- function(dt_x, dt_y, xs, ys, reverse = FALSE, ...) {
  stopifnot(is.data.table(dt_x) && is.data.table(dt_y))

  if(!is.character(xs) || ! is.character(ys)) {
    stop("xs and ys must be character vectors!")
  }


  if(length(xs) != length(ys)) {
    stop("Lengths of 'xs' and 'ys' are not equal")
  }
  if(!all(xs %in% names(dt_x))) {
    stop("All names in 'xs' are not found in ", substitute(dt_x))
  }
  if(!all(ys %in% names(dt_y))) {
    stop("All names in 'ys' are not found in ", substitute(dt_y))
  }

  if(reverse) {
    tmp.dt_x <- dt_x
    tmp.dt_y <- dt_y
    dt_y <- tmp.dt_x
    dt_x <- tmp.dt_y

    tmp.xs <- xs
    tmp.ys <- ys
    xs <- tmp.ys
    ys <- tmp.xs
  }

  x_lst <- lapply(xs, function(f) dt_x[[f]])
  y_lst <- lapply(ys, function(f) dt_y[[f]])

  out <- Map(.lowerMatch, x_lst, y_lst)

  coll <- Reduce(function(a, b) ifelse(is.na(a), b, a), out, ...)

  hit <- length(coll[!is.na(coll)])
  tot <- length(coll)
  pct <- sprintf("%.1f%%", 100*(hit/tot))
  message(
    paste(hit, "of", tot,
          paste0(
            "(", pct, ")"
          ),
          "matched"
    )
  )

  return(coll)
}

NULL

#helper match, tolower with nonmatch NA, incomparables NULL
.lowerMatch <- function(x, y) {
  if("fastmatch" %in% installed.packages())
  {
    fun = getFun("fastmatch::fmatch")
  } else {
    fun = getFun("base::match")
  }

  if(!is.character(x)) {
    warning("x must be of class 'character'. Coercing, which may not be lossless")
    x <- as.character(x)
  }
  if(!is.character(y)) {
    warning("y must be of class 'character'. Coercing, which may not be lossless")
    y <- as.character(y)
  }

  fun(tolower(x), tolower(y), nomatch = NA_integer_, incomparables = NULL)
}
