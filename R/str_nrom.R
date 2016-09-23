#' String normalize-transform
#'
#' Strips special chars, lowercases, optionally re-encode to ASCII
#' @param x character vector, or something coercible to chr
#' @param lower Lowercase the output? Defaults to \code{FALSE}
#' @param ... args to str_replace_all, i.e. pattern and replacement
#' @param to_ASCII Logical. Should function first transform input to ASCII (this is often helpful for
#' otherwise stubborn special characters)? Defaults to \code{TRUE}
#' @details
#' This is a convenience function designed to streamline e.g. fuzzy chr matching, particularly with
#' scraped text. Therefore, some options are intentionally hard-coded, i.e.
#' any whitespace repeats >1 are truncated to 1, and the output is ws-trimmed on both sides.
#'
#' If no args are passed to \code{...} for \link[stringr]{str_replace_all}, generic defaults are used.
#' These defaults are meant to provide a potentially more useful output than just an error message,
#' but this practice somewhat violates error handling paradigms by still trying to return something,
#' which might not be expected As such, this behavior might change in future versions, and explicit arguments to
#' \link[stringr]{str_replace_all} (again, via \code{...}) should always be provided.
#' @return
#' A \code{character} vector normalized according to input args and ws-normalized of length equal to
#' \emph{x}. See details for what ws-normalized means.
#' @export
#' @examples
#' x <- "Corrosion Survey Database (CORâ€¢SUR)"
#' str_norm(x, "\\W", " ")
#' str_norm(x, "\\s", " ") #keep parentheses
#' str_norm(x, "\\W", " ", to_ASCII = FALSE) #iconv option not used
#' str_norm(x, "[A-Za-z]", " ", to_ASCII = FALSE) #inverse
#'
#' str_norm(Sys.Date(), "\\W", " ")
#' str_norm(1:10, "\\d", "-")
#'
#' \dontrun{
#' str_norm(x) #will try to use default pattern and replacement. Read the error message!
#' }
str_norm <- function(x, lower = FALSE, ..., to_ASCII = TRUE) {
  if(class(x) != "character") {
    x <- as.character(x)
  }
  if(to_ASCII) {
    x <- iconv(x, to = "ASCII", sub = " ")
  }

  if(lower=="TRUE") {
    xnorm <- tolower(x)
  } else {
    xnorm <- x
  }

  out <- tryCatch(
    {
      stringr::str_replace_all(xnorm, ...)
    },
    error=function(e) {
      message(paste(e))
      message(paste("Using default pattern '\\s' and replacement ' '",
              "Please supply BOTH explicit args if these are not suitable",
              sep = "\n")
              )
      stringr::str_replace_all(xnorm, "\\s", " ")
    },
    warning=function(w) {
      message(w)
    },
    finally={
      invisible(xnorm)
    }
  )
  out <- stringr::str_replace_all(out, "\\s{2,}", " ")
  stringr::str_trim(out)
}
