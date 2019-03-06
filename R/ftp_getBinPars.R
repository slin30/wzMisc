#' ftp_getBinPars
#'
#' Get parameters needed to download one or more remote binary file(s) to a local directory
#'
#' @import data.table
#' @family ftp_functions
#'
#' @param host Remote host URL
#' @param user User name
#' @param pwd  Password
#' @param hdir Host directory in which to find file(s) to download
#' @param ldir Local directory in which to download remote files
#'
#' @details
#' This function performs three main tasks. First, it verifies if the server can even be queried
#' with the supplied parameters. Second, it checks whether the required information is available;
#' this means that \code{\link{ftp_listDir}} must be able to create a table of files, with
#' successfully parsed-out fields for \code{type_code} and \code{name}. Additionally, at least one
#' \code{type_code} must have a value of \code{1}, indicating a file, and not a directory. Finally,
#' if the required criteria are all satisfied, it outputs a \code{data.table} containing the two
#' required sets of values needed to download the files, \code{remote_url} and \code{local_dest}.
#'
#' @seealso \code{\link{curlProgress}} for the function that a successful output is expected
#' to be used for.
#'
#' @return
#' If no errors, a \code{data.table} with a minimum of two fields:
#'
#' \itemize{
#' \item \code{remote_url}: The full server path, with embedded credentials required to download
#' \item \code{local_dest}: The local destination path corresponding to each \emph{remote_url} asset
#' }
#'
#' In most cases, additional metadata for each remote asset will also be populated, including:
#'
#' \itemize{
#' \item \code{url_sizeMB} The \code{numeric} size in megabytes
#' \item \code{url_mod_year} The \code{character} last modification year
#' \item \code{url_mod_date} The \code{character} last modification date, usually month and day
#' \item \code{url_mod_time} The \code{character} last modification time, usually in 24h-format
#' }
#'
#' Additionally (if no errors), each output table will have two attributes:
#' \itemize{
#' \item \code{local_basedir} The base directory to download to
#' \item \code{local_basedir_exists} Does the local base directory exist?
#' }
#'
#' ... these are a convenient way to determine if the local target directory exists, and if
#' not, then the attribute \code{local_basedir} can be used to create it via e.g. \code{dir.create}.
#'
#' If a server error, then the error message printed to console, and:
#' \itemize{
#' \item \code{call_params} A \code{list} of the parameters used to query the server
#' }
#'
#' If no server error, but all required return data is not detected, then an error message, and:
#' \itemize{
#' \item \code{call_params} A \code{list} of the parameters used to query the server
#' \item \code{call_ret} A nested \code{list} of the return from a call to the server using
#'       the aforementioned params via \code{\link{ftp_listDir}}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## ftp parameter list
#' ftp_params <- list(
#'   host = ftp.xyz.com,
#'   user = "me",
#'   pwd = "pwd",
#'   dir = "/dir1/" # leading and trailing slashes will be normalized in-function
#' )
#'
#' ftp_getBinPars(
#'   host = ftp_params$host,
#'   user = ftp_params$user,
#'   pwd = ftp_params$pwd,
#'   hdir = ftp_params$dir,
#'   ldir = "./local_dir_1/"
#' )
#' }
ftp_getBinPars <- function(host = NULL, user = NULL, pwd = NULL, hdir = NULL, ldir = NULL) {

  is_valid_input <- vapply(mget(names(formals(ftp_getBinPars))),
                           function(f) length(f) == 1L & is.character(f),
                           logical(1))
  invalid_input <- names(formals(ftp_getBinPars))[!is_valid_input]

  if(length(invalid_input) > 0L) {
    stop(
      paste(
        "All inputs must be length 1 characters; the following (is/are) not:\n\t",
        paste0("[", paste(invalid_input, collapse = ", "), "]")
      )
    )
  }

  # normalize slashes for host and local dirs here to avoid having to redo a bunch later
  dirlist <- Map(
    norm_trailing_slash,
    list(
      host_dir = hdir,
      local_dir = ldir
    ),
    how = list("replace"),
    replace_with = list("forward"),
    strip_leading = list(TRUE)
  )

  # these are also returned during early termination
  call_params <- ftp_makeParams(
    host = paste0(
      norm_trailing_slash(host, how = "replace", replace_with = "forward"),
      dirlist[["host_dir"]]
    ),
    user = user,
    pwd = pwd
  )

  call_ret <- tryCatch(
    {
      ftp_listDir(call_params)
    },
    error = function(e) {
      message("Error in call; ", appendLF = FALSE)
      message("here is the error message:")
      message(" ", e)
      return(NULL)
    },
    warning = function(w) {
      message("Warning raised; ", appendLF = FALSE)
      message("here is the warning message:")
      message(" ", w)
    },
    finally = NULL
  )

  # early termination if error
  if(is.null(call_ret)) {
    message("...returning the call parameters that raised the error")
    return(list(call_params = call_params))
  }
  # early termination if we do not have file-level info either
  if(!is.data.frame(call_ret[["files"]]) || nrow(call_ret[["files"]]) == 0L) {
    message("Terminating: call to server succeeded, but required information (files) not available")
    return(
      list(call_params = call_params,
           call_ret = call_ret)
    )
  }

  file_dt <- call_ret[["files"]]
  req_nms <- c("type_code", "name")

  # early termination if we do not have type_code and name
  if(!all(req_nms %in% names(file_dt))) {
    message("Terminating: file information returned, but not all required fields present")
    return(
      list(call_params = call_params,
           call_ret = call_ret)
    )
  }

  # now subset file_dt by type_code, and extract just name field for downstream
  file_dt <- file_dt[get("type_code") == 1L]
  files <- file_dt[["name"]]

  # early termination if we do not have any type code 1, indicating no files/only dir listing
  if(length(files) == 0L) {
    message("Terminating: no files of type_code '1' found; is this the right level of directory?")
    return(
      list(call_params = call_params,
           call_ret = call_ret)
    )
  }

  ##### if we get to this point, then we have the required info to return params for curlProgress

  # this is non-essential stuff, so should not cause failure
  if("size" %in% names(file_dt)) { # provide size info if possible, but do not let this cause failures
    sizes <- file_dt[get("type_code") == 1L & get("name") %in% files][["size"]]
  } else {
    sizes <- NA_real_
  }
  if(all(c("mod_time", "mod_date", "mod_year") %in% names(file_dt))) { # and mod datetime if possible, as-is
    last_mod_subDT <- file_dt[get("type_code") == 1L & get("name") %in% files,
                              c("mod_year", "mod_date", "mod_time"), with = FALSE]
    setnames(last_mod_subDT, paste0("url_", names(last_mod_subDT)))
  } else {
    last_mod_subDT <- NULL
  }

  # these are your target remote files
  remote_paths <- paste0(
    norm_trailing_slash(
      call_ret[["root_url"]],
      how = "replace",
      replace_with = "forward"
    ),
    norm_trailing_slash(
      file_dt[["parent_dir"]],
      how = "replace",
      replace_with = "forward",
      strip_leading = TRUE
    ),
    files
  )
  # generate remote urls
  remote_url <- paste0(
    "ftp://",
    user, ":",
    pwd, "@",
    remote_paths
  )

  local_dest <- paste0(
    dirlist[["local_dir"]], files
  )

  out_dt <- data.table(
    remote_url = remote_url,
    local_dest =  local_dest,
    url_sizeMB = sizes/1E6
  )
  if(!is.null(last_mod_subDT)) {
    out_dt <- cbind(out_dt, last_mod_subDT)
  }

  # useful to check if the local base dir even exists
  setattr(out_dt, "local_basedir", dirlist[["local_dir"]])
  setattr(out_dt, "local_basedir_exists", dir.exists(dirlist[["local_dir"]]))

  out_dt
}
