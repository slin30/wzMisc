#' Parse key metadata from common 7-zip archives
#'
#' Extract the name and size of each file from a 7-zip listing
#'
#' @importFrom stringr str_locate str_sub str_extract
#' @family sevenzip functions
#'
#' @param x A valid return from a (captured) call to 7-zip with an
#'          action of \code{l} (\code{list})
#'
#' @details
#' This is mainly used as a helper for \code{\link{sevenzip_listcont}}. The expected
#' input is a single listing from a 7-zip return, i.e. a call to 7-zip with an action of
#' \code{l}.
#'
#' @return
#' A \code{data.frame}
#'
#' @export
#'
#' @examples
#' #TBD
sevenzip_parseinfo <- function(x) {

  if(!is.character(x)) {
    stop("character input required")
  }

  # get orig path metadata
  orig_path <- .extract_7z.Piece(x, targ = "Path =")

  # subset the parts you want
  x_new <- .extract_7z.FileMeta(x)

  whats = c("Attr", "Size", "Compressed", "Name")
  locs <- vapply(whats, function(f) str_locate(x_new[[1]][1], f), integer(2))
  rownames(locs) <- c("start", "end")

  # further subset for output
  extract_pos <- vapply(x_new, function(f) which(grepl("-{3,}", f)) + 1L, integer(1))
  x_subidx <- Map(`:`, extract_pos, vapply(x_new, length, integer(1)))
  x_sub <- Map("[", x_new, x_subidx)

  x_size <- lapply(x_sub, function(f) str_sub(f, start = locs["end", "Attr"]+1L, end = locs["start", "Compressed"]-1L))
  x_size <- lapply(x_size, function(f) gsub(" ", "", f))
  x_size <- lapply(x_size, as.numeric)
  x_size <- do.call(cbind, x_size)

  KB_conv <- 1000L
  if(.Platform$OS.type == "windows") {
    KB_conv  <- 1024L
  }

  x_fname <- lapply(x_sub, function(f) str_sub(f, start = locs["start", "Name"]))
  x_fname <- lapply(x_fname, trimws)
  x_fname <- do.call(cbind, x_fname)

  data.frame(orig_path = orig_path,
             fname = x_fname,
             size_KB = round(as.numeric(x_size/KB_conv), 2),
             stringsAsFactors = FALSE
  )

}

NULL
# Get number of folders, files, total bytes
.extract_7z_toplevel <- function(x) {

  start_line <- "Scanning the drive for archives:"
  start_idx  <- which(grepl(start_line, x))

  if(length(start_idx) == 0L) {
    return(NULL)
  }

  res <- x[start_idx:start_idx+1]

  n_folders <- str_extract(res, "(\\d+)(?=\\sfolder[s]?)")
  n_files <- str_extract(res, "(\\d+)(?=\\sfile[s]?)")
  tot_size <- str_extract(res, "(\\d+)(?=\\sbytes)")

  list(n_folders = as.numeric(n_folders),
       n_files = as.numeric(n_files),
       tot_bytes = as.numeric(tot_size)
  )
}

# helper
.extract_7z.Piece <- function(x, targ = "Path =") {

  where <- which(grepl(targ, x))
  out <- x[where]

  clean <- sub(targ, "", out)
  trimws(clean, "both")

}

# helper to extract out key metadata from 7zip list files
.extract_7z.FileMeta <- function(x) {

  top_level <- .extract_7z_toplevel(x)

  if(all(is.na(unlist(top_level)))) {
    stop("No valid info detected")
  }

  x_start <- which(grepl("Date.*Time", x))
  x_end <-   which(grepl("\\d+\\s+\\d+\\s+files", x)) -2L

  if(length(x_start) == 0L || length(x_end) == 0L) {
    stop("Valid start and end indicies not found")
  }

  x_end <- head(x_end, top_level[["n_files"]])

  x_idx <- Map(`:`, x_start, x_end)

  lapply(x_idx, function(f) x[f])
}
