#' Clean up xml or html markup tags and formatting
#'
#' Useful when dealing with '< >' enclosed parts of strings in a vector
#'
#' @importFrom xml2 xml_text read_html read_xml
#'
#' @param x A character; the input you wish to unescape
#' @param what_ml One of \code{xml, html} to denote if content should be handled as such. Defaults to \code{html}
#' @param toASCII A logical vector of length 1. Should the input be encoded as ASCII via \code{iconv}?
#' @param ... Optional. Additional args to \code{iconv} and used when \emph{toASCII} is \code{TRUE}
#'
#' @return A character vector the same length of \emph{x}, with \code{<x>} unescaped. If no unescaping was
#' required, will return \emph{x} as is, by default.
#'
#' @description
#' This is a minor modification of
#' http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r,
#' and all credit is due.
#'
#' This function will call either \code{\link[xml2]{read_xml}} or \code{\link[xml2]{read_html}},
#' depending on the value passed to the argument. The default, if not specified, is \code{html}.
#'
#' If called with \emph{toASCII} == \code{TRUE}, \emph{x} is processed by \code{\link{iconv}},
#' which may or may not change \emph{x}. In both the spirit of minimizing surprises, and with
#' particular note to the potential of an early return if no unescaping is
#' required, \emph{toASCII} is \code{FALSE} by default.
#'
#' @note
#' The \code{xml2} functions this relies upon are not vectorized (this is a different use case, so
#' no criticism is implied re: the functions themselves). The actual function handles vector inputs of
#' length >1 through \code{vapply()}, and should maintain a reasonable level of performance by first
#' subsetting only those elements of \emph{x} where \code{<.+>} is present. Therefore, if there are only
#' a few elements of \emph{x} that require this function, performance should be acceptable, runtimes
#' should increase on an as-needed basis.
#'
#' @export
#'
#' @examples
#' str <- "<i>in-situ</i> electron microscopy"
#' unescape_markup(str)
unescape_markup <- function(x, what_ml = c("html", "xml"), toASCII = FALSE, ...){

  if(!is.character(x)) {
    stop("x must be a character input")
  }

  if(toASCII) {
    x <- iconv(x, to = "ASCII", ...)
  }

  needs_fixing <- grepl("<.+>", x)

  if(all(!needs_fixing)) {
    return(x)
  }

  to_fix <- x[needs_fixing]
  out_vec <- rep(NA_character_, length(x))

  fun <- switch(match.arg(what_ml),
                xml = read_xml,
                html = read_html
  )

  fixed <- vapply(to_fix, function(f)
    xml_text(
      fun(
        paste0(
          "<x>", f, "</x>"
        )
      )
    ), FUN.VALUE = character(1)
  )

  out_vec[needs_fixing] <- fixed
  out_vec[!needs_fixing] <- x[!needs_fixing]

  return(out_vec)

}