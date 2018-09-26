#' High-level column statistics for data.frames
#'
#' Calculate pre-determind per-column stats
#'
#' @param tbl A data.frame or data.table.
#' @param pivot Output in cross-tabular format if \code{TRUE} (default)
#'
#' @details
#' Useful to check high-level column statistics prior to e.g. creating a table schema.
#'
#' @note
#' \code{factor} columns are treated as \code{character} via \code{as.character()}
#'
#' @return
#' By default, a data.table with the following fields:
#'
#' \itemize{
#' \item field_name: \code{factor}; input field names
#' \item CLASS: \code{chr}; the class of the field
#' \item MAYBE_NUMBER: \code{logi}; does the field contain only numbers, such that even
#'       upon coercion of non-numeric fields, no \code{NA} values would result?
#'       Always \code{TRUE} for \code{numeric} (or \code{integer}) fields
#' \item FRAC_COMPLETE: \code{numeric}; the fraction of rows that are not \code{NA}
#' \item NCHAR_MAX_LEN: \code{integer}; the maximum character length of the field, after
#'       coercing to \code{character}
#' \item UNIQUEN: \code{integer}; the distinct count of values, excluding \code{NA}
#' \item INTEGRAL_DUPE_FCTR; \code{integer}; the fraction of duplicate values, only if
#'       the result of dividing the distinct count of non-\code{NA} values by the
#'       row count is an integral value. \code{NA} if this is not true, i.e. if the
#'       modulo of the calculation \code{!= 0}.
#' }
#'
#' @export
#'
#' @examples
#' set.seed(10)
#' int_sample <- sample(1:10L, 100, replace = TRUE)
#' test_df <- data.frame(
#'   num_col = rnorm(100),
#'   chr_col = sample(LETTERS, 100, replace = TRUE),
#'   int_col = int_sample,
#'   int_as_factor = as.factor(int_sample),
#'   int_as_chr = as.character(int_sample),
#'   stringsAsFactors = FALSE
#' )
#'
#' profile_table(test_df, pivot = TRUE)
profile_table <- function(tbl, pivot = TRUE) {

  stopifnot(is.data.frame(tbl))
  if(!is.data.table(tbl)) {
    tbl <- copy(tbl)
    setDT(tbl)
  }

  ord <- factor(names(tbl), ordered = TRUE, levels = names(tbl))

  cls <- tbl[, lapply(.SD, class)]
  un_dupe <- lapply(tbl, .dupe_factor_int) # different here to avoid dual uniqueN calc
  non_na <- tbl[, lapply(.SD, .length_complete)]
  nchar_max <- tbl[, lapply(.SD, .max_nchar)]
  possibly_numeric <- tbl[, lapply(.SD, .maybe_numeric)]

  out_lst <- list(
    CLASS = cls,
    MAYBE_NUMBER = possibly_numeric,
    FRAC_COMPLETE = non_na,
    NCHAR_MAX_LEN = nchar_max,
    UNIQUEN = vapply(un_dupe, function(f) f[["un"]], integer(1)), # extract uniqueN part
    INTEGRAL_DUPE_FCTR = vapply(un_dupe, function(f) f[["dupe_fctr"]], integer(1)) # extract dupe_fctr part
  )
  var_fctr <- factor(names(out_lst), ordered = TRUE, levels = names(out_lst))

  out_tbl <- lapply(out_lst, function(f) data.table(field_name = names(f), value = unlist(f)))
  out <- rbindlist(out_tbl, use.names = TRUE, idcol = "variable")
  out[["variable"]] <- factor(out[["variable"]], levels = levels(var_fctr))
  out[["field_name"]] <- factor(out[["field_name"]], levels = levels(ord))

  if(pivot) {
    out <- dcast(out, ... ~ variable, value.var = "value")
  }
  out
}
# helpers
.max_nchar <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }
  # handle factors as chr
  if(is.factor(x)) {
    x <- as.character(x)
  }

  max(nchar(x[!is.na(x)]))
}
.maybe_numeric <- function(x) {
  if(all(is.na(x))) {
    return(NA)
  }

  if(is.numeric(x)) {
    return(TRUE)
  }

  if(!any(grepl("^[0-9]", trimws(x, which = "left")))) {
    return(FALSE)
  }

  if(!anyNA(suppressWarnings(as.numeric(x[!is.na(x)])))) {
    return(TRUE)
  }

  FALSE

}
.valid_uN <- function(x) {
  if(all(is.na(x))) {
    return(NA_integer_)
  }

  uniqueN(x, na.rm = TRUE)
}
.dupe_factor_int <- function(x) {
  if(all(is.na(x))) {
    return(NA_integer_)
  }
  un <- .valid_uN(x)
  len <- length(x)

  fctr_num <- len/un
  fctr_modu <- len %% un

  fctr_num[fctr_modu != 0] <- NA_real_
  list(un = un, dupe_fctr = as.integer(fctr_num))
}
.length_complete <- function(x) {
  if(all(is.na(x))) {
    return(NA_character_)
  }

  tot_len <- length(x)
  non_na_len <- length(x[!is.na(x)])

  out <- non_na_len/tot_len
  out
}

