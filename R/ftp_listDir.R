#' List files and/or directories within an ftp endpoint
#'
#' For a specific ftp endpoint, list all top-level files and/or directories
#'
#' @family ftp_functions
#'
#' @param arglist (reqiured) A list of parameters to pass to \code{\link[RCurl]{getURL}},
#' containing at minimum \code{url}.
#' @param ncharlim (optional) Number of terminal characters after a dot, beyond which a character vector
#' is no longer considered a file. Defaults to \code{3L}.
#'
#' @return
#' A \code{list} of length 2, each element itself \code{list} of directories and files, respectively. If
#' no valid values exist for either top-level return element, then \code{NULL}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(RCurl)
#' my_arglist <- ftp_makeParams(host = "ftp.123.com", user = "myself", pwd = "my_pwd")
#' ftp_listDir(my_arglist)
#' }
ftp_listDir <- function(arglist, ncharlim = 3L) {

  if(class(arglist) != "list") {
    stop("Class of ", substitute(arglist), " must be 'list'")
  }
  reqnms <- c("url")
  if(!all(reqnms %in% names(arglist))) {
    stop("All required names not found in ", substitute(arglist))
  }
  autonms <- c("ftplistonly", "crlf")
  if(any(autonms %in% names(arglist))) {
    curr <- intersect(autonms, names(arglist))
    warning(paste(curr, collapse = ","),
            " found in ", substitute(arglist),
            " replacing with default")
  }

  arglist[["ftplistonly"]] <- TRUE
  arglist[["crlf"]] <- TRUE

  res <- do.call(getURL, arglist)

  server_paths <- unlist(strsplit(res, split = "\r\n"))

  is_file <- server_paths[.has_file_ext(server_paths, ncharlim = ncharlim)]
  is_dir  <- server_paths[!.has_file_ext(server_paths, ncharlim = ncharlim)]

  if(!is.null(is_dir)) {
    is_dir <- paste0(arglist[["url"]], "/", is_dir, "/")
  }

  list(is_dir = as.list(is_dir),
       is_file = as.list(is_file)
  )
}


NULL
.has_file_ext <- function(x, ncharlim = 3L) {

  reg_ex <- paste0("\\..{1,", ncharlim, "}$")
  out <- grepl(reg_ex, x)
  out[!out == ""]
}
