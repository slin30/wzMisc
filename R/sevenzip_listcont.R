#' List 7-zip file contents
#'
#' List the contents of one or more valid file 7-zip extensions in a directory
#'
#' @family sevenzip functions
#'
#' @param archive_path A path containing valid file extensions. See details for allowed extensions.
#' @param extras Additional arguments to pass to \code{\link{sevenzip_makecmd}}
#' @param recursive Should the call to \code{list.files} be performed recursively? Defaults to \code{FALSE}
#' @param parse Parse the raw return via \code{\link{sevenzip_parseinfo}}? Defaults to \code{TRUE}
#'
#' @details
#' Valid file extensions include \code{7z, zip, rar, gzip, rar, gzip, tar, bzip2, gz, tar.gz,
#' tgz, bz, wim}. File (directory) listing is performed with a (non-recursive by default) call to
#' \code{\link[base]{list.files}}. An error is raised if no files including one of these
#' extensions are found in \emph{archive_path}.
#'
#' Running this with \code{recursive=TRUE} can be a useful way to get a listing of the contents of all valid
#' archive files in a directory. Note, though, that this can take some time, depending on the
#' scope and/or level of nesting of the target directory.
#'
#' @note
#' There is no guarantee that this will work for a non-Windows OS.
#'
#' \emph{recursive} refers to the files in a directory, and not the contents of each archive.
#' In other words, this function will not look beyond the first listing level in an archive.
#'
#' @return
#' By default, a \code{list} of length 2:
#'
#' \itemize{
#' \item \code{success} A \code{list}; if no valid archive contents could be accessed, then an
#'       empty \code{list} of length 1, otherwise, of length 2:
#'     \itemize{
#'         \item  \code{is_parseable}: A \code{data.frame} of successfully parsed items, unless no items
#'                were parseable, in which case an empty list, i.e. \code{list()}
#'         \item \code{not_parseable}: A \code{list} of unparseable items, containing the raw text output;
#'                if none, then an empty list, i.e. \code{list()}
#'              }
#' \item \code{failure} A \code{list} of archives where 7-zip could not access any files within.
#' }
#'
#' If \code{parse=FALSE}, then a \code{list} of raw output
#'
#' @export
#'
#' @examples
#' # TBD
sevenzip_listcont <- function(archive_path, extras = NULL, recursive = FALSE, parse = TRUE) {

  # check archive_path
  possible_ext <- paste(paste0(.valid_extensions, "$"), collapse = "|")
  file_list <- list.files(archive_path, pattern = possible_ext,
                          recursive = recursive, full.names = TRUE,
                          ignore.case = TRUE)

  if(length(file_list) == 0L) {
    stop("No compatible archive formats found; valid extensions are:\n\t(",
         paste(.valid_extensions, collapse = ","), ")")
  }

  # need to enclose file_list in quotes to handle spaces
  # dQuote() does not seem to work
  file_list <- paste0('\"', file_list, '\"')

  cmd_list <- lapply(file_list, function(f)
    sevenzip_makecmd(action = "list", target = f, extras = extras)
  )
  rawinfo <- lapply(cmd_list, function(f)
    do.call(system2, c(f, list(stdout = TRUE)))
  )

  if(!parse) {
    return(rawinfo)
  }

  # rawinfo can have status 2 if archive could not be read
  success <- Filter(function(x) is.vector(x), rawinfo)
  failure <- Filter(function(x) !is.vector(x), rawinfo)

  if(length(success) == 0L) {
    warning("One or more archives found, but unable to access files within any")
    return(
      list(success = list(),
           failure = failure)
    )
  }

  # try to parse
  success_parsed <- vector("list", length(success))
  for(i in seq_along(success_parsed)) {
    success_parsed[[i]] <- tryCatch(
      {
        sevenzip_parseinfo(success[[i]])
      },
      error = function(e) {
        invisible(e)
        success[[i]]
      },
      warning = function(w) {
        invisible(w)
        success[[i]]
      },
      finally = NULL
    )
  }

  # split parseable
  is_parseable <- Filter(function(x) is.data.frame(x), success_parsed)
  not_parsable <- Filter(function(x) !is.data.frame(x), success_parsed)

  if(length(is_parseable) == 0L) {
    warning("Failed to parse any archive listings, returning unparsed values")
    return(
      list(
        success = list(
          is_parseable = list(),
          not_parseable = not_parseable
        ),
        failure = failure
      )
    )
  }

  list(
    success = list(
      is_parseable = do.call(rbind, is_parseable),
      not_parsable = not_parsable
    ),
    failure = failure
  )
}

.valid_extensions <- c(
  "7z",
  "zip",
  "rar",
  "gzip",
  "tar",
  "bzip2",
  "gz",
  "tar.gz",
  "tgz",
  "bz",
  "wim"
)
