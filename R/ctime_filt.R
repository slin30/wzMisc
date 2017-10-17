#' Filter files by c/m/atime
#'
#' Identify earliest or latest file(s) by creation/modified/access time
#' @param x Input file path
#' @param pattern Optional regex passed to \code{list.files} pattern arg; defaults to \code{.*} (any).
#' @param type One of "min" or "max" (quotes explicit) to denote if function should find
#' earliest or latest file by file.info file time, respectively. Defaults to "max" (most recent file(s));
#' see \emph{what_finfo}.
#' @param what_finfo What file.info field type should be used for filtering? Defaults to \code{ctime}.
#' @param only_last Should only the last file found be returned? Defaults to \code{TRUE}
#' @param fmt \code{character} date format for ctime transformation. Defaults to \%Y-\%m-\%d
#' @param print_limit Optional integer. How many found results should be printed?
#' Defaults to 6. This ONLY affects display results, NOT returned results!
#' @param ... Other arguments to pass to list.files, aside from \code{pattern}
#' @details
#' This is most useful for filtering out multiple files with similar or identical file names within a single parent
#' dir, regardless of nesting, by one of \code{ctime,mtime,atime} as denoted by file.info. By default, the timestamp
#' used is \code{ctime}.
#'
#' Beware if using directly for reading-in, as this function will list all files found according to input criteria,
#' although it will also return only the last file found, by default. Nonetheless, you should
#' apply additional constraints for non-interactive use, e.g. specifying a more specific path and/or
#' pattern.
#' @return
#' A vector of files (full path names) that meet the type criteria (earliest or latest) and any other constraints,
#' within the scope of the specified dir.
#' @export
#' @examples
#' x <- list.files(R.home("doc"), full.names = TRUE)
#' ctime_filt(x) #use defaults
#' ctime_filt(x, pattern = ".html|.png", type = "max")
ctime_filt <- function(x, pattern, type = c("max", "min"),
                       what_finfo = c("ctime", "mtime", "atime"),
                       only_last = TRUE, fmt = "%Y-%m-%d",
                       print_limit = 6L, ...) {

  if(missing(type)) {
    type <- "max"
  }

  if(missing(pattern)) {
    pattern = ".*"
  }

  if(missing(what_finfo)) {
    what_finfo <- "ctime"
  }

  files <- list.files(x, full.names = TRUE, pattern = pattern, ...)
  finfo <- file.info(files)[[what_finfo]]
  targ  <- match.fun(type)(as.Date(format(finfo, fmt)))

  note <- switch(type,
                 max = "Latest",
                 min = "Earliest")

  out <- files[grepl(targ, finfo)]

  if(length(out) == 0L) {
    stop("No files found")
  }
  # the default option is also an early return
  if(only_last) {
    ret <- out[[length(out)]]
    message(paste(note, "file timestamp is", targ,
                  "corresponds to this file:\n",
                  ret,
                  "\n",
                  "(file 1 of", length(out)), "found)")
    return(invisible(ret))
  }

  # if you want all, though...
  message(paste(note, "file timestamp is", targ,
                "and file count is:", length(out)
                )
          )

  print_limit <- min(c(length(out), print_limit))

  if(length(out) >= print_limit) {
    message(paste0("Here are the first ", print_limit, " files:",
                  "\n",
                  paste(head(out, n = print_limit), collapse = "\n"),
                  "\n...", length(out) - print_limit, " more not shown"))
  } else {
    message(paste(head(out), collapse = "\n"))
  }

    return(invisible(out))
}
