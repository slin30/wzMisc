#' Handle trailing (and optionally, leading) slashes
#'
#' Remove, or remove and insert a single trailing forward or back slash
#'
#' @param x A character vector
#' @param how What kind of normalization should be performed? One of \code{remove,replace}. Defaults to \code{remove}
#' @param replace_with If \code{how = replace}, should we replace with a forward or backslash? One of \code{forward,back}.
#' Defaults to \code{forward}
#' @param strip_leading Also strip the first leading slash, if present? Defaults to \code{FALSE}
#' @param view_res Output the structure the first six rows of the input and output? Defaults to \code{FALSE}
#'
#' @details
#' \emph{Normalization} is defined as taking the input and stripping a single trailing slash, if one is present. This
#' is performed whether removing only, or replacing; hence, this function will remove a single trailing slash and stop,
#' or also append a single trailing slash (and if so, the type of slash as specified by \emph{replace_with}).There is
#' nothing fancy about this function; it is a useful helper for e.g. creating directory strings
#'
#' @note
#' If \code{view_res = TRUE}, the first six rows of the input and output are printed to the console via \code{dput()}
#' (with default args). This may be useful to verify that the literal output, sans escapes, are as expected by e.g.
#' calling \code{View()} on the deparsed output.
#'
#' @return A character vector of length \emph{x}
#' @export
#'
#' @examples
#' norm_trailing_slash(x = c("/dir1\\", "/dir1/"), how = "remove", view_res = TRUE)
#' norm_trailing_slash(x = c("/dir1\\", "/dir1/"), how = "replace",
#'   replace_with = "forward", view_res = TRUE)
norm_trailing_slash <- function(x, how = c("remove", "replace"), replace_with = c("forward", "back"),
                                strip_leading = FALSE, view_res = FALSE) {

  stopifnot(is.character(x))

  if(missing(how)) {
    message("Using default of 'remove'")
    how = "remove"
  }

  if(how == "remove" && !missing("replace_with")) {
    warning("Requested 'how = remove'. Removing trailing slashes and ignoring 'replace_with' arg")
  }

  if(how == "replace" && missing(replace_with)) {
    message("Arg not supplied to 'replace_with'. Defaulting to forward slash")
    addslash = "forward"
  }


  # first we strip trailing slash
  x_clean <- sub("\\\\$|/$", "", x)
  if(strip_leading) {
    x_clean <- sub("^\\\\|^/", "", x_clean)
  }

  to_add <- switch(
    match.arg(replace_with),
    forward = "/",
    back = "\\"
  )

  out = switch(
    match.arg(how),
    remove = x_clean,
    replace = paste0(x_clean, to_add)
  )

  if(view_res){
    dput(
      head(
        data.frame(
          input = x,
          output = out,
          stringsAsFactors = FALSE
        )
      )
    )
  }

  invisible(out)
}
