% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_xs_ys.R
\name{match_xs_ys}
\alias{match_xs_ys}
\title{Collapse multiple possible matches between two data.tables}
\usage{
match_xs_ys(
  dt_x,
  dt_y,
  xs,
  ys,
  reverse = FALSE,
  lower = TRUE,
  incomparables = NA,
  use_fastmatch = FALSE,
  ...
)
}
\arguments{
\item{dt_x}{A data.table where you wish to find corresponding index values in \emph{dt_y}}

\item{dt_y}{A data.table where matching values in \emph{dt_x} should be found,
and indices from \emph{dt_y} returned}

\item{xs}{A chr vector of names in \emph{dt_x} to match upon. Intrinsic order is potentially
important, and must correspond to order and length of \emph{ys}. Non-character columns
will be coerced to character via \code{as.character}, with a warning.}

\item{ys}{A chr vector of names in \emph{dt_y} that correspond to \emph{xs} in scope and
length. Non-character columns will be coerced to character via \code{as.character},
with a warning.}

\item{reverse}{Logi. Do you wish to perform the match in reverse, that is find indices
in \emph{dt_x} where corresponding values in \emph{dt_y} match? Defaults to \code{FALSE}}

\item{lower}{Should values within \emph{xs} and \emph{ys} be lowercased? Defaults to \code{TRUE}.
If possible and requested to use \code{fmatch}, keeping this at default will not set any hash
indices, but will also not result in any benefits upon repeat runs.}

\item{incomparables}{For \code{match}; set to \code{NA} by default to prevent matches on \code{NA}}

\item{use_fastmatch}{Use \code{fmatch()} from the \code{fastmatch} package if installed? Defaults
to FALSE. See details, and also note interaction with the \emph{lower} parameter (above).}

\item{...}{Additional arguments to pass to \code{Reduce()}}
}
\value{
An \code{integer} vector of length \emph{xs} or length \emph{ys} (since it is required that
\code{length(xs) == length(ys)}) containing matching indices, else \code{NA}. The indices by
default denote the positions of values in \emph{dt_y} that match \emph{dt_x}, unless
\code{reverse = TRUE}, in which case the reverse.

Additionally, a console message containing match statistics. If called with \code{accumulate=TRUE},
statistics are printed out for each step in cumulative fashion, displaying the (any) additional
coverage provided by each additional pair of match elements.
}
\description{
Cascading character matching for sets of columns across two data.tables
}
\details{
Addresses a common merge use case between two tables where a single, robust key is not available,
and one must rely on one or more fields between the two tables to make a best-attempt merge.
In this scenario, the order of values in \emph{xs}, and the corresponding order in \emph{ys} is
critical, and should correspond to one's best-guess (expectation) of specificity, since this
function calls \code{Reduce} to collapse the list of match results into a single vector.

If the \code{fastmatch} package is loaded, and \emph{use_fastmatch} is \code{TRUE},
will use \code{fastmatch::fmatch} for performance, else will use \code{base::match}. This
is intentional due to the fact that \code{fmatch} will technically modify \code{dt_y} in-place by
appending a hash index to fields flagged within \emph{ys}. To ensure this function is
truly side-effect-free by defalt, you must set this option explicitly. Also, there is no repeat-
run benefit (but also no side-effect) with the default setting of \code{lower=TRUE}.
}
\note{
By default, this function coerces both \emph{xs} and \emph{ys} to lowercase via \code{tolower}.
\code{match} is called with defaults, i.e. \code{match(x, y, nomatch = NA_integer_, incomparables = NULL)},
and this is enforced, with no plans to make optional.

The \emph{length} requirement for formals \emph{xs} and \emph{ys} apply to the arguments themselves,
and not to the length of variables represented by the values within each. In other words, a standard
call to \code{match} does not require equal-length inputs, nor does this function. What is required,
though, is that if e.g. \emph{xs} is a vector of length \code{2}, representing two fields within
\emph{dt_x}, then the length of \emph{ys} must also be \code{2}, even if you wish to match a single
field within \emph{dt_y} to each field within \emph{dt_x} (no recycling is performed on argument
length for \emph{xs} or \emph{ys}).
}
\examples{
library(data.table)
set.seed(10)
# dt_x is the table you want to append to
dt_x <- data.table(
  key_a = sample(LETTERS[1:15], replace = FALSE)
)
# dt_y has one or more target fields you wish to pull, using the keys
# from dt_x.
dt_y <- data.table(
  col_a = unlist(Map(c, LETTERS[seq(1, 10, by = 2)], list(NA_character_))),
  col_b = unlist(Map(c, list(NA_character_), LETTERS[seq(2, 11, by = 2)])),
  targ = sample(1:100L, 10, replace = FALSE)
)
# this is used for indexing results out
mvec <- match_xs_ys(dt_x, dt_y, c("key_a", "key_a"), c("col_a", "col_b"))
# pull over results
dt_x[, targ := dt_y$targ[mvec]]

# also useful for quick tests:
match_xs_ys(dt_x, dt_y, c("key_a", "key_a"), c("col_a", "col_b"),
            accumulate = TRUE)
}
