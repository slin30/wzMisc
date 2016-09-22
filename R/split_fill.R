#' Split and fill a chr vector
#'
#' Split a chr vector based on sep, return melted DT by ID
#' @import data.table
#' @param dat a data.table
#' @param targ chr; vector of length 1 that contains data to be split
#' @param split_on chr; what pattern should be used to perform the split?
#' @param IDcol chr; vector of length 1 denoting the column in \emph{dat} containing the ID to be used for melting
#' @param rebind logical; should the original columns be appended back to the output? Defaults to \code{FALSE}
#' @param keep.targ logical; only used if \emph{rebind} = \code{TRUE}; drop the column that was split on?
#' @details
#' This is a convenience-convenience wrapper around \code{data.table::tstrsplit}, taking advantage
#' of the performance of Ctranspose, and adding faculties to melt and rejoin selectively.
#'
#' @note
#' \emph{targ} currently is limited to a vector of length 1, as is \emph{IDcol}. This is likely to change in the future, to
#' make this function more flexible and consistent with the capabilities of \code{melt.data.table}. At the moment, still
#' trying to understand \code{data.table} scoping rules.
#' @return
#' A melted data.table using \emph{IDcol} as \code{id.var} for \link[data.table]{melt.data.table},
#' with \emph{targ} splitted by split_on.
#'
#' If \emph{rebind} == \code{TRUE}, will also return the original columns, with a single \emph{IDcol} as denoted
#' in input. This is performed via a \code{data.table} ad-hoc join, using \emph{IDcol} in \code{j}. The input
#' \emph{targ} column will be returned as well, if \emph{keep.targ} is \code{TRUE}.
#' @export
#' @examples
#' set.seed(1)
#' dt <- data.table(
#'     ID = 1:10,
#'     targ = sapply(1:10, function(f)
#'         paste(sample(LETTERS, sample(1:5)), collapse = "|"))
#' )
#' head(split_fill(dat = dt, targ = "targ", split_on = "\\|", IDcol = "ID"))
#'
#' #Demonstrating rebind
#' dt[, targ_additional := targ]
#' head(split_fill(dat = dt, targ = "targ", split_on = "\\|", IDcol = "ID", rebind = TRUE))
#'
split_fill <- function(dat, targ, split_on, IDcol, rebind = FALSE, keep.targ = FALSE) {

  splts_fill <- data.table::tstrsplit(dat[[targ]], split_on, fixed = TRUE) %>%
    as.data.table %>%
    .[, c(IDcol) := dat[[IDcol]]]
  out <- melt.data.table(splts_fill, measure.vars = patterns("V[0-9]"),
                         id.vars = c(IDcol),
                         na.rm = TRUE,
                         variable.factor = FALSE)

  if(rebind == FALSE){
    return(out)
  } else {

    dat_orig <- copy(dat)

    if(keep.targ == FALSE) {
      dat_orig[[targ]] <- NULL
    }

    diffcols    <- setdiff(names(dat_orig), names(out))
    appendCols  <- c(IDcol, diffcols)

    if(length(diffcols) == 0) {

      appendCols <- IDcol

      names.orig   <- names(dat_orig)
      names.split  <- names(out)
      names.unique <- c(names.orig, names.split) %>% make.unique

      new.orig  <- names.unique[1:length(names.orig)]
      new.split <- names.unique[(length(names.orig)+1):length(names.unique)]

      names(new.split) <- names.split
      new.split[names(new.split) == IDcol] <- IDcol

      setnames(dat_orig, new.orig)
      setnames(out, new.split)

      warning(stringr::str_wrap(paste("Overlapping names detected in original and split output.",
                                      "Names in split output will be made unique via make.unique",
                                      "and this will be reflected in output unless violating columns",
                                      "are == 'targ' argument, which should only happen if you run",
                                      "this function with rebind = TRUE with a two-column DT",
                                      "in which case you can safely ignore this warning", collapse = "")),
              width = 80)

    }

    out[dat_orig[, c(appendCols), with = FALSE], on = IDcol]


  }
}

