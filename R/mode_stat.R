#' Mode (as in statistical 'mode') function
#'
#' Calculates the mode of x.
#' @param x input vector
#' @details In case of ties, or if \code{length(x) == 1}, will return the first value.
#' The standard caveats apply to floats; see examples.
#' @note Credit: http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#' @return The most frequent value in \emph{x}, or see details for exception cases.
#' Will throw a warning if \code{length(unique(x) == length(x))}.
#' @export
#' @examples
#' mode_stat(sample(1:10, 100, replace = TRUE))
#' mode_stat(sample(LETTERS[1:3], 20, replace = TRUE))
#'
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' x[1] == mode_stat(x)
#' x <- signif(x, 2)
#' x[1] == mode_stat(x) #better, but note about ties applies
#' }
mode_stat <- function(x) {
  ux <- unique(x)
  if(length(x) == length(ux)) {
    warning("Unique count of x identical to count of x;
            will return the first value of x, which may
            not be what you intended")
  }
  ux[which.max(tabulate(match(x, ux)))]
}

