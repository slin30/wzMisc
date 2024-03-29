% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sevenzip_listcont.R
\name{sevenzip_listcont}
\alias{sevenzip_listcont}
\title{List 7-zip file contents}
\usage{
sevenzip_listcont(archive_path, extras = NULL, recursive = FALSE, parse = TRUE)
}
\arguments{
\item{archive_path}{A path containing valid file extensions. See details for allowed extensions.}

\item{extras}{Additional arguments to pass to \code{\link{sevenzip_makecmd}}}

\item{recursive}{Should the call to \code{list.files} be performed recursively? Defaults to \code{FALSE}}

\item{parse}{Parse the raw return via \code{\link{sevenzip_parseinfo}}? Defaults to \code{TRUE}}
}
\value{
By default, a \code{list} of length 2:

\itemize{
\item \code{success} A \code{list}; if no valid archive contents could be accessed, then an
      empty \code{list} of length 1, otherwise, of length 2:
    \itemize{
        \item  \code{is_parseable}: A \code{data.frame} of successfully parsed items, unless no items
               were parseable, in which case an empty list, i.e. \code{list()}
        \item \code{not_parseable}: A \code{list} of unparseable items, containing the raw text output;
               if none, then an empty list, i.e. \code{list()}
             }
\item \code{failure} A \code{list} of archives where 7-zip could not access any files within.
}

If \code{parse=FALSE}, then a \code{list} of raw output
}
\description{
List the contents of one or more valid file 7-zip extensions in a directory
}
\details{
Valid file extensions include \code{7z, zip, rar, gzip, rar, gzip, tar, bzip2, gz, tar.gz,
tgz, bz, wim}. File (directory) listing is performed with a (non-recursive by default) call to
\code{\link[base]{list.files}}. An error is raised if no files including one of these
extensions are found in \emph{archive_path}.

Running this with \code{recursive=TRUE} can be a useful way to get a listing of the contents of all valid
archive files in a directory. Note, though, that this can take some time, depending on the
scope and/or level of nesting of the target directory.
}
\note{
There is no guarantee that this will work for a non-Windows OS.

\emph{recursive} refers to the files in a directory, and not the contents of each archive.
In other words, this function will not look beyond the first listing level in an archive.
}
\examples{
# TBD
}
\seealso{
Other sevenzip functions: 
\code{\link{sevenzip_checkpath}()},
\code{\link{sevenzip_makecmd}()},
\code{\link{sevenzip_parseinfo}()}
}
\concept{sevenzip functions}
