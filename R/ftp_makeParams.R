#' Construct params to interact with an FTP endpoint
#'
#' Create url strings and, optionally, authentication parameters for FTP endpoint interactions
#'
#' @family ftp_functions
#'
#' @param host (required) The host address string. Should take the form of \code{ftp.your_host.com}
#' @param protocol (optional) The protocol to use; defaults to \code{ftp}
#' @param user (optional) User login name, if host access requires authentication. If provided, \emph{pwd}
#' must also be a valid string
#' @param pwd (optional) User password, if host access requires authentication. If provided, \emph{user}
#' must also be a valid string
#'
#' @return
#' A named list of length 2, containing the \code{url} and \code{userpwd}.
#' If either \emph{user} or \emph{pwd} are missing OR are blank, the second element will be \code{NULL}.
#' @export
#'
#' @details
#' This is a simple helper function that is most useful for constructing argument-lists to pass to e.g.
#' \code{\link[RCurl]{getURL}}; the outputs are named with this use case in mind. See examples.
#'
#' This function normalizes common variants of input formats for \emph{host} and \emph{protocol}. See examples.
#'
#' @examples
#' # with authentication
#' ftp_makeParams(host = "ftp.123.com", user = "myself", pwd = "my_pwd")
#'
#' # NULL or blank for either of user or pwd will result in NULL userpwd
#' ftp_makeParams(host = "ftp.123.com", user = "myself", pwd = " ")
#'
#' # host and protocol input normalization
#' x0 <- ftp_makeParams(host = "ftp.123.com", protocol = "ftp" )
#' x1 <- ftp_makeParams(host = "ftp://ftp.123.com", protocol = "ftp")
#' x2 <- ftp_makeParams(host = "ftp://ftp.123.com", protocol = "ftp://")
#' x3 <- ftp_makeParams(host = "ftp://ftp.123.com")
#'
#' all(sapply(list(x1, x2, x3), function(f) identical(x0, f)))
#'
#' # If you wanted to get a listing of all files:
#' \dontrun{
#' library(RCurl)
#' my_params <- ftp_makeParams(host = "ftp.123.com", user = "myself", pwd = "my_pwd")
#' my_params$ftplistonly <- TRUE
#' do.call(getURL, my_params)
#' }
ftp_makeParams <- function(host, protocol = "ftp", user = NULL, pwd = NULL) {

  protocol_clean <- gsub("[^[:alnum:]]", "", tolower(protocol))

  host <- tolower(host)
  host <- gsub(paste0("^", protocol_clean, "://"), "", host)
  host <- gsub(paste0("^(", protocol_clean, "){2,}"), protocol_clean, host)

  url <- paste0(protocol_clean, "://", host)

  userpwd <- NULL
  if(!any(.null_or_blank(user), .null_or_blank(pwd))) {
    userpwd <- paste(user, pwd, sep = ":")
  }

  list(url = url, userpwd = userpwd)
}

NULL
.null_or_blank <- function(x) {
  if(length(x) > 1L) {
    stop(substitute(x), " must be of a vector of length 1")
  }

  x_is_null <- is.null(x)
  x_is_blank <- nchar(x) == 0L
  x_is_empty <- x == " "

  Reduce("|", list(x_is_null, x_is_blank, x_is_empty))
}
