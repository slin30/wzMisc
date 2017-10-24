#' List files and/or directories within an ftp endpoint
#'
#' For a specific ftp endpoint, list all top-level files and/or directories
#'
#' @importFrom RCurl getURL
#'
#' @family ftp_functions
#'
#' @param arglist (reqiured) A list of parameters to pass to \code{\link[RCurl]{getURL}},
#' containing at minimum \code{url}.
#' @param ncharlim (optional) Number of terminal characters after a dot, beyond which a character vector
#' is no longer considered a file. Defaults to \code{3L}.
#'
#' @return
#' A \code{list} of length 3, the first two elements each a \code{list} of directories and files, respectively. If
#' no valid values exist for either top-level return element, then \code{NULL}. The last element contains the
#' root (host) url.
#'
#' If present, files will have the parent directory appended as a name. Files that exist at the root level will
#' be named with the input url.
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

  is_file <- as.list(server_paths[.has_file_ext(server_paths, ncharlim = ncharlim)])
  is_dir  <- as.list(server_paths[!.has_file_ext(server_paths, ncharlim = ncharlim)])

  # if no dir, then files inherit basename of parent as upper level
  # if dir and file, then files inherit basename of parent as upper level
  # if dir and no file, then do nothing

  # use this later for naming files
  url_parsed <- unlist(strsplit(arglist[["url"]], split = "/{1,}"))
  if(length(is_file) > 0L) {
    is_file <- setNames(is_file, rep(tail(url_parsed, 1), length(is_file)))
  }

  out <- list(dirs = is_dir,
              files = is_file,
              root_url = url_parsed[[1]]
  )

  lapply(out, .zeroLenToNull)

}


NULL
.has_file_ext <- function(x, ncharlim = 3L) {

  reg_ex <- paste0("\\..{1,", ncharlim, "}$")
  out <- grepl(reg_ex, x)
  out[!out == ""]
}

.zeroLenToNull <- function(x) {
  if(length(x) > 0L) {
    return(x)
  }
  list(NULL)
}
