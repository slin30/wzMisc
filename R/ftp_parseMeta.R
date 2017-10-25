#' Parse ftp return from getURL call with no options
#'
#' @importFrom stringr str_match
#' @import data.table
#' @family ftp_functions
#'
#' @param ret (required) A character vector; the return from a call to
#' \code{\link[RCurl]{getURL}} with no options.
#'
#' @return
#' If at least one field in \emph{ret} was parsed, a \code{data.table} containing eight
#' fields, plus the input (so nine total). Fields include:
#'
#' \itemize{
#' \item \code{original} (chr); the original input, for reference
#' \item \code{permissions} (chr); flags denoting various access and modification parameters,
#'       unparsed.
#' \item \code{type_code} (int); the type of asset. Typically, \code{1} denotes a file, while \code{2}
#'       a directory
#' \item \code{owner_group} (chr); a \code{space}-delimited vector denoting the owner and group
#' \item \code{size} (dbl); for a \code{file}, the size in bytes. This may not be meaningful for
#'       directories.
#' \item \code{mod_date} (chr); the date, possibly only the three-letter month, \code{space} date (of month)
#'       if the last modification date is less than 6 months from the current date (of the endpoint).
#' \item \code{mod_time} (chr); the time of the \emph{mod_date}
#' \item \code{name} (chr); the name of the asset; if a file, will include the extension.
#' }
#'
#' If \emph{ret} could not be parsed at all, i.e. if all \code{NA}, then the input is returned
#' as-is, with a warning.
#'
#' @details
#' The expected input assumes that \code{getURL} is called with the bare minimum options,
#' i.e. only \code{url} and, if necessary, \code{userpwd}. Since there is no input checking, this
#' function handles un-parse-able inputs by returning the input as-is.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(RCurl)
#' # get basic metadata from an ftp endpoint
#' my_params <- ftp_makeParams(host = "ftp.123.com", user = "myself", pwd = "my_pwd")
#' my_meta   <- do.call(getURL, my_params)
#' ftp_parseMeta(my_meta)
#' }
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
  if(all(is.na(out))) {
    warning("Failed to parse; returning input as-is")
    return(ret)
  }
  setnames(out, c("original", names(splt_list)))

  out[["size"]] <- as.numeric(out[["size"]])
  out[["type_code"]] <- as.integer(out[["type_code"]])
  out

}
