#' Remove all columns that match one or more critieria
#'
#' For a data.frame, drop all columns that satisfy a condition or set of conditions
#'
#' @import data.table
#'
#' @param df A data.frame (or data.table)
#' @param ifallwhat A vector of value(s) to test for. \code{NA}, if included, will be dropped.
#' If not specified, defaults to a blank zero-length character, i.e. \code{""}.
#' @param factor_as_chr If \emph{ifallwhat} is of class \code{character}, should columns of
#' class \code{factor} be treated as character (and therefore subject to deletion as well)?
#' @param drop_all_NA Should columns that are all NA be dropped? Defaults to \code{TRUE}
#'
#' @return A \code{data.frame}, or a \code{data.table} if this was passed to \emph{df}, with
#' all columns that match the requirements within \emph{ifallwhat} removed. If no columns
#' in \emph{df} met the criteria requested, then the input is returned without modification.
#'
#' @note
#' This function will coerce the input data.frame into a data.table via
#' \code{\link[data.table]{as.data.table}} as an intermediate step. If a data.frame is detected
#' as input, \code{\link[data.table]{setDF}} is used for output coercion.
#'
#' @export
#'
#' @examples
#' my_df <- data.frame(
#' A = 1:3L,
#' B = rep(numeric(3), 3),
#' C = rep("", 3),
#' D = rep(NA_real_, 3),
#' stringsAsFactors = FALSE
#' )
#'
#' my_df_1 <- remove_ifall_cols(my_df, numeric(3))
#' my_df_2 <- remove_ifall_cols(my_df, numeric(3), drop_all_NA = FALSE)
#' remove_ifall_cols(my_df, c(""), drop_all_NA = FALSE)
#'
#' remove_ifall_cols(my_df, c(1:3))
#' remove_ifall_cols(my_df, c(1:3), drop_all_NA = FALSE)
#' remove_ifall_cols(my_df, as.numeric(1:3), drop_all_NA = FALSE) # does nothing
remove_ifall_cols <- function(df, ifallwhat = NULL, factor_as_chr = TRUE, drop_all_NA = TRUE) {
  if(is.null(ifallwhat)) {
    ifallwhat <- ""
  }

  as_df <- FALSE
  if(is.data.frame(df) && !is.data.table(df)) {
    as_df <- TRUE
    df <- as.data.table(df)
  }

  class.ifallwhat <- class(ifallwhat)

  if(class.ifallwhat == "character") {
    if(factor_as_chr) {
      class.ifallwhat <- c("factor", "character")
    }
  }

  class_fun   <- cols_class(class.ifallwhat)
  targ_cols   <- class_fun(df)

  # handle NA passed to ifallwhat
  ifallwhat <- ifallwhat[!is.na(ifallwhat)]

  out.what <- unlist(
    df[, lapply(.SD, function(f) all(f %in% ifallwhat)), .SDcols = targ_cols]
  )
  out.NA <- unlist(
    df[, lapply(.SD, function(f) all(is.na(f)))]
    )

  found_what <- names(out.what[out.what])
  found_NA   <- names(out.NA[out.NA])

  if(drop_all_NA) {
    targs_found <- unique(c(found_what, found_NA))
  } else {
    targs_found <- found_what
  }

  targs_found <- targs_found[!is.na(targs_found)]

  if(length(targs_found) == 0L || is.null(targs_found)) {
    message("No columns detected; doing nothing")
    return(df)
  }

  df.out <- df[, -c(targs_found), with = FALSE]
  message("removed ", paste(targs_found, collapse = ", "))
  if(as_df) {
    return(setDF(df.out))
  } else {
    return(df.out)
  }
}
