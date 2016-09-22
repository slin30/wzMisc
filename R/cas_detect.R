#' Detect if something is (likely) a validly formatted CAS RN
#'
#' Simple format check for valid Chemical Abstracts Service Registry Number
#'
#' @param x chr. A vector of values to check
#' @param preprocess logi. Trim leading and trailing whitespace and pare all consecutive (\code{-{2,}})
#' to (\code{-})? Defaults to \code{FALSE}. Does not carry over to return values, nor modify input in any way. See details.
#' @param output chr. What should the function return? Defaults to \code{check}
#'
#' @details
#' A quick, though highly imperfect way to determine if something has the basic formatting characteristics of a CAS RN.
#' Checks for CAS basic formatting rules only, i.e.
#' \itemize{
#' \item{Three sections of digits}
#' \item{Each separated by a single hyphen (\code{-})}
#' \item{Section 1 length \code{>=2} and \code{<=7}}
#' \item{Section 2 length \code{==2}}
#' \item{Section 3 length \code{==1}}
#' }
#'
#' If \emph{preprocess=TRUE}, note that \link[stringr]{str_trim} is called with default options, i.e.
#' \code{sides = "both"}, and this is the only whitespace modification performed on the intermediate vector. If you
#' wish to ensure that all whitespaces are remoted, you should perform this upstream, i.e. this function will
#' not strip all whitespaces for you.
#'
#' @note
#' There is a significant difference between something that looks like a CAS and is a (valid) CAS. This function
#' should have a low false negative rate, i.e. if it flags \emph{x} as \code{FALSE}, it is almost certainly the case.
#' A result of \code{TRUE}, however, only means that the input meets the aforementioned criteria (see details).
#'
#' Use \code{\link{cas_checkSum}} to determine if inputs determined as being CAS(-like) also pass the last-digit
#' checksum check. See examples.
#' @return
#' By default, a \code{logical} vector of equal length to \emph{x}.\cr
#' If \code{output == "count"}, an \code{integer} vector of length equal to \emph{x}.\cr
#' If \code{output == "result"}, a list of length equal to \emph{x} containing the extracted result(s) for each element
#'
#' @export
#' @examples
#' toCheck <- c("cas rn: 123-45-6", "123-45-6", " 123-45-6", "123--45-6", "123- 45 -6")
#' cas_detect(toCheck)
#' cas_detect(toCheck, preprocess = TRUE)
#' cas_detect(toCheck, output = "result")
#' cas_detect(toCheck, preprocess = TRUE, output = "result")
#' cas_detect(gsub("[^\\w-]", "", toCheck, perl = TRUE), preprocess = TRUE, output = "result")

cas_detect <- function(x, preprocess = FALSE, output = c("check", "count", "result")) {
  stopifnot(is.character(x))

  if(missing(output)) {
    output <- "check"
  }

  if(preprocess) {
    x <- stringr::str_trim(x)
    x <- stringr::str_replace_all(x, "-{2,}", "-")
  }

  n <- stringr::str_count(x, "\\d{2,7}-\\d{2}-\\d{1}")

  if(output == "check") return(n > 0)
  if(output == "count") return(n)
  else stringr::str_extract_all(x, "\\d{2,7}-\\d{2}-\\d{1}")
}

