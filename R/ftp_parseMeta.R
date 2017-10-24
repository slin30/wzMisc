#' Parse ftp return from getURL call with no options
#'
#' @importFrom stringr str_match
#' @import data.table
#' @family ftp_functions
#'
#' @param ret (required) A character vector; the return from a call to
#' \code{getURL} with no options
#'
#' @return
#' A \code{data.table}
#'
#' @export
#'
#' @examples
#' # TBD
ftp_parseMeta <- function(ret) {

  if(!is.character(ret)) {
    stop("ret must be a character vector")
  }

  splt_list<- c(
    permissions = "^([^\\s]+)",
    type_code = "(\\d)",
    owner_group = "(\\d+\\s+\\d+)",
    size = "(\\d+)",
    mod_date = "([A-Z][a-z]{2}\\s+\\d+)",
    mod_time = "(\\d+:\\d+)",
    name = "(.*$)"
  )

  reg_ex <- Reduce(function(a, b) paste0(a, "\\s+", b), splt_list)

  splitted_ret <- as.list(unlist(strsplit(ret, split = "\r\n")))

  parsed <- lapply(splitted_ret, function(f) str_match(f, reg_ex))
  out <- as.data.table(do.call(rbind, parsed))
  setnames(out, c("original", names(splt_list)))

  out[["size"]] <- as.numeric(out[["size"]])
  out[["type_code"]] <- as.integer(out[["type_code"]])
  out

}
