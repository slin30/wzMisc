#' WINDOWS ONLY - Check if 7zip is on your system path
#'
#' Check for presence of 7zip on your system path
#'
#' @family sevenzip functions
#'
#' @param cmd Defaults to \code{7z}
#' @param print.to.console Be verbose and print 7-zip options and other information?
#'        Defaults to \code{FALSE}
#'
#' @details
#' This is primarily a helper function for other 7zip functions, but can be a helpful
#' shortcut to check if 7-zip is in your PATH environment variable, and/or to review
#' the available commands/flags/options for 7-zip, if called with
#' \code{print.to.console=TRUE}.
#'
#' @note
#' If \code{FALSE}, typically you should:
#'
#' \enumerate{
#' \item Navigate to your environment variables, e.g. go to \code{START}
#' \item Type \code{Edit environment variables}
#' \item click on \code{PATH} or create a variable called \code{PATH} if none exists
#' \item \code{Edit}
#' \item Add the following: \code{C:\\Program Files\\7-zip}, making sure that if
#'       you have other variables, you separate each with a semicolon (and no spaces
#'       in-between)
#' }
#'
#'
#' @return
#' \code{TRUE} if 7-zip found on your system PATH, \code{FALSE} otherwise
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sevenzip_checkpath() # Returns only TRUE of FALSE
#' sevenzip_checkpath(print.to.console = TRUE) # Also view options and other info
#' }
sevenzip_checkpath <- function(cmd = "7z", print.to.console = FALSE) {

  call <- invisible(
    system(cmd, show.output.on.console = print.to.console)
  )

  call == 0L
}
