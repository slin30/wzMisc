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
#' Valid file extensions include \code{7z, zip, rar, gzip, rar, gzip, tar, bzip2, gz, tar.gz}.
#' File (directory) listing is performed with a (non-recursive by default) call to
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
#' By default, a \code{data.frame} with the full path, archive name, and
#' size of the file in KB, for each file within the first-level hierarchy of
#' each archive found within \code{archive_path}. If parsing failed, of if
#' no parsing is requested, a \code{list} containing the raw text output.
#'
#' @export
#'
#' @examples
#' # TBD
sevenzip_listcont <- function(archive_path, extras = NULL, recursive = FALSE, parse = TRUE) {

  # check archive_path
  possible_ext <- paste(paste0(.valid_extensions, "$"), collapse = "|")
  file_list <- list.files(archive_path, pattern = possible_ext,
                          recursive = recursive, full.names = TRUE)

  if(length(file_list) == 0L) {
    stop("No compatible archive formats found; valid extensions are:\n\t(",
         paste(.valid_extensions, collapse = ","), ")")
  }

  cmd_list <- lapply(file_list, function(f)
    sevenzip_makecmd(action = "list", target = f, extras = extras)
  )
  rawinfo <- lapply(cmd_list, function(f)
    do.call(system2, c(f, list(stdout = TRUE)))
  )

  if(!parse) {
    return(rawinfo)
  }

  tryCatch(
    {
      parsed <- lapply(rawinfo, sevenzip_parseinfo)
    },
    error = function(e) {
      invisible(e)
      return(rawinfo)
    },
    finally = NULL
  )

  do.call(rbind, parsed)

}

.valid_extensions <- c(
  "7z",
  "zip",
  "rar",
  "gzip",
  "tar",
  "bzip2",
  "gz",
  "tar.gz"
)
