#' Create commands to perform system calls with 7-zip
#'
#' Helper to construct argument lists to use with system2, to invoke 7-zip commands
#'
#' @family sevenzip functions
#'
#' @param action (required) One of \code{a,d,e,l,t,u,x}, or the literal names,
#'         \code{archive,delete,extract,list,test,update,extract with full paths}.
#'          Intentionally not all-encompassing
#' @param target (required) The (input) path upon which to ultimately perform the action
#' @param output The (output) path; required for \emph{actions} \code{a,e,u,x}.
#' @param extras (optional) Other commands to pass to 7zip. If provided, will be appended
#'        last
#' @param verbose Output the full command string to console? Defaults to \code{TRUE}
#'
#' @details
#' This is simply a call-construction helper that also checks for the presence of 7-zip in
#' your PATH on Windows systems. It does not invoke any system calls beyond said
#' check (and no check is performed on a non-Windows OS. There is also minimal validation,
#' meaning that it is possible to construct invalid calls.
#'
#' The inputs to \emph{target, output} are automatically reversed for archive
#' actions, i.e. \code{a, u}. This is done to ensure that the semantic intent of the
#' arguments is consistent with the actual command ordering. For any actions other than
#' \code{archive, update}, the values passed to \emph{target, output} are passed through
#' as-is.
#'
#' @note
#' There is no guarantee that this will generate valid commands for a non-Windows OS.
#'
#' The main purpose of this function is to make it easier to construct syntactically valid
#' commands, and the main audience comprises those who are not already fluent in invoking
#' 7-zip from the command line, but need to use some of its capabilities programatically.
#' If you feel something is missing, it probably is, but then you are also likely not the
#' target end-user.
#'
#' @return
#' A \code{list} of length 2, containing the \code{command} and \code{args}; the former will
#' always be \code{7z}. Specifically designed to be compatible with \code{\link[base]{system2}} via
#' \code{\link[base]{do.call}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # make a temp dir and populate with a simple file
#' my_tempdir <- tempdir()
#' my_tempfile <- tempfile(tmpdir = my_tempdir, fileext = ".txt")
#' sink(my_tempfile)
#' cat(1:10)
#' sink()
#' list.files(my_tempdir)
#'
#' # make a zip command
#' my_cmd_zip <- sevenzip_makecmd("a",
#'                                target = paste(my_tempfile),
#'                                output = paste(my_tempdir, "test.7z", sep = "/")
#' )
#' do.call(system2, my_cmd_zip)
#' list.files(my_tempdir)
#' # make an unzip command, into a new dir called 'exracted'
#' my_cmd_unzip <- sevenzip_makecmd("e",
#'                                  target = paste(my_tempdir, "test.7z", sep = "/"),
#'                                  output = paste(my_tempdir, "extracted", sep = "/")
#' )
#' do.call(system2, my_cmd_unzip)
#' file.info(paste(my_tempdir, "extracted", sep = "/"))
#' # if you try to extract a file and it exists, you will get an error unless you pass
#' #  required flags, e.g:
#' my_cmd_unzip_overwrite <- sevenzip_makecmd("e",
#'                                            target = paste(my_tempdir, "test.7z", sep = "/"),
#'                                            output = paste(my_tempdir, "extracted", sep = "/"),
#'                                            extras = "-aoa"
#' )
#' do.call(system2, my_cmd_unzip_overwrite)
#' file.info(paste(my_tempdir, "extracted", sep = "/"))
#' Map(file.remove, list.files(my_tempdir, recursive = TRUE, full.names = TRUE))
#' unlink(paste(my_tempdir, "extracted", sep = "/"), recursive = TRUE)
#' }
sevenzip_makecmd <- function(action = NULL, target = NULL, output = NULL, extras = NULL, verbose = TRUE) {

  sys_call <- "7z"
  extras <- paste(extras, collapse = " ")

  # only check PATH on Windows
  is_windows <- .Platform$OS.type == "windows"

  if(is_windows) {
    if(!sevenzip_checkpath(sys_call)) {
      stop("7z not found in PATH variable.
         Please add this environment variable to 'path' and re-try\n",
           "Usually this will be 'C:\\\\Program Files\\\\7-zip'")
    }
  }

  if(is.null(target)) {
    stop("A value for 'target' must be provided")
  }

  valid_actions <- c("a", "d", "e", "l", "t", "u", "x")

  if(is.null(action)) {
    action = "l"
  } else if(any(action %in% names(.zip_7z_ref))) {
    action = .zip_7z_ref[action]
  } else if(any(action %in% valid_actions)) {
    action = action
  }

  req_output <- c("a", "e", "u", "x")

  if(action %in% req_output && is.null(output)) {
    stop("A value for 'output' is required for archive and extract actions")
  }

  outflag <- NULL
  if(action == "e") {
    outflag <- "-o"
  }

  # if archiving files, then switch output and target
  if(action %in% c("a", "u")) {
    tmp.target <- target
    tmp.output <- output

    output <- tmp.target
    target <- tmp.output
  }

  out.args <- paste(
    action,
    target,
    paste0(outflag,
           paste(output,
                 collapse = " ")
    ),
    extras,
    collapse = " "
  )

  out.args <- trimws(out.args, "both")
  out.args <- gsub("\\s{2,}", " ", out.args)

  if(verbose) {
    message(paste("command is:", "'",
                  sys_call, out.args,
                  "'")
    )
  }

  list(command = sys_call, args = out.args)
}

NULL
# helper lookup list
.zip_7z_ref <- c(
  archive = "a",
  extract = "e",
  list = "l",
  update = "u",
  test = "t"
)
