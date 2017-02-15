#' Clean up xml or html markup tags and formatting
#'
#' Useful when dealing with '< >' enclosed parts of strings in a vector
#'
#' @importFrom xml2 xml_text read_html read_xml
#'
#' @param x A character vector of length 1. The input you wish to unescape
#' @param what_ml One of \code{xml, html} to denote if content should be handled as such. Defaults to \code{xml}
#' @param toASCII A logical vector of length 1. Should the input be encoded as ASCII via \code{iconv}?
#' @param ... Optional. Additional args to \code{iconv} and used when \emph{toASCII} is \code{TRUE}
#'
#' @return A character vector of length 1, with \code{<x>}
#'
#' @description
#' This is a minor modification of
#' http://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r,
#' and all credit is due.
#'
#' It generally makes no difference whether \emph{what_ml} is specified, but when html vs. xml is
#' important, this function will call either \code{\link[xml2]{read_xml}} or \code{\link[xml2]{read_html}},
#' depending on the value passed to the argument.
#'
#' @note
#' There are probably more efficient ways to do this, but this function seems to work, and is useful
#' enough to add to a misc package. No error checking yet, beyond what the comprising function
#' implement
#'
#' @export
#'
#' @examples
#' str <- "<i>in-situ</i> electron microscopy"
#' unescape_markup(str)
unescape_markup <- function(x, what_ml = c("xml", "html"), toASCII = FALSE, ...){

  fun <- switch(match.arg(what_ml),
                xml = read_xml,
                html = read_html
  )

  if(toASCII) {
    x <- iconv(x, to = "ASCII", ...)
  }

  xml_text(fun(paste0("<x>", x, "</x>")))
}
