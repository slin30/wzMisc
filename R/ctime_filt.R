#' Filter files by ctime
#'
#' Identify earliest or latest file(s) by creation time
#' @param x Input file path
#' @param pattern Optional regex passed to \code{list.files} pattern arg; defaults to \code{.*} (any).
#' @param type One of "min" or "max" (quotes explicit) to denote if function should find
#' earliest or latest file by creation time, respectively. Defaults to "max" (most recent file(s))
#' @param fmt \code{character} date format for ctime transformation. Defaults to \%Y-\%m-\%d
#' @param print_limit Optional integer. How many found results should be printed?
#' Defaults to 6. This ONLY affects display results, NOT returned results!
#' @param ... Other arguments to pass to list.files, aside from \code{pattern}
#' @details
#' This is most useful for filtering out multiple files with similar or identical file names within a single parent
#' dir, regardless of nesting, by ctime as denoted by file.info.
#'
#' Beware if using directly for reading-in, as this function will list all files found according to input criteria;
#' you should apply additional constraints for non-interactive use, e.g. specifying a more specific path and/or
#' pattern.
#' @return
#' A vector of files (full path names) that meet the type criteria (earliest or latest) and any other constraints,
#' within the scope of the specified dir.
#' @export
#' @examples
#' x <- list.files(R.home("doc"), full.names = TRUE)
#' ctime_filt(x) #use defaults
#' ctime_filt(x, pattern = ".html|.png", type = "max")
ctime_filt <- function(x, pattern, type = c("max", "min"), fmt = "%Y-%m-%d", print_limit = 6L, ...) {

  if(missing(type)) {
    message("Type arg missing; defaulting to max")
    type <- "max"
  }

  if(missing(pattern)) {
    pattern = ".*"
  }

  files <- list.files(x, full.names = TRUE, pattern = pattern, ...)
  finfo <- file.info(files)[["ctime"]]
  targ  <- match.fun(type)(as.Date(format(finfo, fmt)))

  note <- switch(type,
                 max = "Latest",
                 min = "Earliest")

  out <- files[grepl(targ, finfo)]

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
