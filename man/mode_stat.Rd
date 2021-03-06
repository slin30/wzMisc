% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mode_stat.R
\name{mode_stat}
\alias{mode_stat}
\title{Mode (as in statistical 'mode') function}
\usage{
mode_stat(x)
}
\arguments{
\item{x}{input vector}
}
\value{
The most frequent value in \emph{x}, or see details for exception cases.
Will throw a warning if \code{length(unique(x) == length(x))}.
}
\description{
Calculates the mode of x.
}
\details{
In case of ties, or if \code{length(x) == 1}, will return the first value.
The standard caveats apply to floats; see examples.
}
\note{
Credit: http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
}
\examples{
mode_stat(sample(1:10, 100, replace = TRUE))
mode_stat(sample(LETTERS[1:3], 20, replace = TRUE))

\dontrun{
set.seed(1)
x <- rnorm(100)
x[1] == mode_stat(x)
x <- signif(x, 2)
x[1] == mode_stat(x) #better, but note about ties applies
}
}
