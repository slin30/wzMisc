% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ftp_listDir.R
\name{ftp_listDir}
\alias{ftp_listDir}
\title{List files and/or directories within an ftp endpoint}
\usage{
ftp_listDir(arglist, include_fileInfo = TRUE, ncharlim = 3L)
}
\arguments{
\item{arglist}{(reqiured) A list of parameters to pass to \code{\link[RCurl]{getURL}},
containing at minimum \code{url}.}

\item{include_fileInfo}{(optional) Try to get file information, if files are found? Defaults to \code{TRUE}}

\item{ncharlim}{(optional) Number of terminal characters after a dot, beyond which a character vector
is no longer considered a file. Defaults to \code{3L}.}
}
\value{
A \code{list} of length 3, the first two elements each a \code{list} of directories and files, respectively. If
no valid values exist for either top-level return element, then \code{NULL}. The last element contains the
root (host) url.

If present, files will have the parent directory appended as a name. Files that exist at the root level will
be named with the input url.
}
\description{
For a specific ftp endpoint, list all top-level files and/or directories
}
\examples{
\dontrun{
library(RCurl)
my_arglist <- ftp_makeParams(host = "ftp.123.com", user = "myself", pwd = "my_pwd")
ftp_listDir(my_arglist)
}
}
\seealso{
Other ftp_functions: 
\code{\link{ftp_getBinPars}()},
\code{\link{ftp_makeParams}()},
\code{\link{ftp_parseMeta}()}
}
\concept{ftp_functions}
