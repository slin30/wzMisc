#' Create n chunks from a vector
#'
#' Generate start and end (from, to) points along a positive vector of length n
#'
#' @param n A positive (nonzero) integer representing the total length to break into chunks
#' @param chunk_size A positive (nonzero) integer denoting the size of each chunk
#' @param start Optional. A positive (nonzero) integer denoting where to start; defaults to \code{1L}
#' @param limit Optional. A positive (nonzero) integer denoting the maxinum chunk size; no limit by default
#' @param n.fx Optional. A positive (nonzero) numeric denoting the factor to increase n and chunk_size beyond the input limit.
#' Defaults to 1.00 (identity). Should be very rarely needed.
#'
#' @details
#' This function creates equally-spaced, or as equal as possible, start (\emph{from}) and end (\emph{to}) points.
#' The core functionality can be recreated simply by using
#' \code{seq.int(from, to, by)} along with \code{seq.int(from, to, by)-(by-1)}. This function provides quite a bit more
#' flexibility and error-checking.
#'
#' This function handles the common scenario where the upper threshold denoted by \emph{n} is important,
#' i.e. for batched API calls. As such, the terminal chunk may very well be of a different size
#' than previous ones. Additionally, if \emph{n} \code{<} \emph{chunk_size}, the latter will automatically
#' be truncated to \emph{n}, or if \emph{n.fx} \code{!= 1.00} (default), towards \emph{n}\code{*} \emph{n.fx}.
#'
#' The \emph{start} argument optionally enables setting a start point that is not the default, 1. This is
#' useful if you wish to e.g make an API call starting from a specific index.
#'
#' The limit argument optionally enables setting a \emph{chunk_size} threshold to e.g. ensure that a
#' \emph{chunk_size} > \emph{limit} is not possible. This should be set if using \emph{n.fx}!
#'
#' @note
#' Inputs other than \emph{n.fx} should be integers. They need not be multiples of one another. Aside from
#' \emph{n.fx}, numerics will be coerced to integer via \code{as.integer}, and this may create unexpected,
#' although still correct (from an integer coercion perspective) results.
#'
#' In typical use, \emph{n.fx} will not be used, even if it is explicitly provided; the anticipated use case for
#' this function is to take a large \emph{n} and chunk it up into pieces of size \emph{chunk_size}. If and only if
#' \emph{n} is less than \emph{chunk_size} will \emph{n.fx} be used; this scenario is outside the scope of the most
#' common use case. It can, however, arise when dealing with a range of potential \emph{n}, where some \emph{n} values
#' might be \code{<} \emph{chunk_size}. In these situations, the function will automatically adjust both \emph{n} AND
#' \emph{chunk_size} by \emph{n.fx}. If you do not provide an explicit \emph{n.fx}, there will be no additional adjustment.
#' If you provide an \emph{n.fx} value other than \code{1.00} (default), there will be (upward) adjustment as requested.
#'
#' Note that upward adjustment is mainly relevant if you are not sure if the value of (some of) your \emph{n} values are
#' actual upper limits, and do not wish to make any safety adjustments in your input data, in which case this function
#' has facilities to meet this (admittedly fringe) requirement.
#'
#' If you need upward adjustment, you should set the \emph{limit} argument if an upper limit is important in your
#' application, as the function will not know what the upper limit is in such cases.
#'
#'
#' @return
#' A data.frame containing three variables: \emph{from}, \emph{to}, and \emph{size}. By default, in ascending order
#' according to \emph{to}.
#' @export
#' @examples
#' make_chunks(1000L, 200L)
#' make_chunks(1000.999, 200) # same
#' make_chunks(1000.99, 200.99) # also same
#' make_chunks(100, 23) # note final chunk size
#'
#' make_chunks(2E5, 5E4, limit = 5E4) # common Site Catalyst use case
#' make_chunks(2E5, 5E4) # same without limit, since limit is optional
#' make_chunks(2E5, 5E5, limit = 5E4) # same; limit auto-corrects
#' make_chunks(2E5, 5E4, limit = 5E4, start = 50001) # skip the first chunk
#'
#' make_chunks(1E4, 5E4) # n < chunk_size; will auto-set chunk_size to n
#' make_chunks(1E4, 5E4, n.fx = 1.01) # using n.fx to raise input limit for safety
#' make_chunks(1E4, 5E4, n.fx = 1.01, limit = 1E3) # now n.fx not used since limit < n
#'
#' \dontrun{
#' make_chunks(100, 1000, start = 101) # error; start < n
#' make_chunks(100, 1000, start = 101, n.fx = 2) # still error; cannot circumvent with n.fx!
#' make_chunks(10000, 1000, start = 101, n.fx = 2) # this is OK, although n.fx is not used
#' make_chunks(10, 10) # works, but not much point
#' make_chunks(10, 10, limit = 2.1) # also works, but now there is a point
#' }
make_chunks <- function (n, chunk_size, start, limit = Inf, n.fx = 1.00)
{

  stopifnot(n > 0 && chunk_size > 0)
  if (missing(start)) {
    start <- 1L
  }
  else {
    start <- as.integer(start)
  }

  if (!missing(limit)) {
    limit <- as.integer(limit)
    if (chunk_size > limit) {
      chunk_size <- limit
      message(paste("Maximum chunk_size is", limit, "; setting chunk_size to",
                    limit))
    }
  }

  if(start > n){
    stop(paste("n is", n, "which is less than the requested start value of", start,
               "\nThis does not make sense, as the start argument is meant to initiate incrementing",
               "starting from 'start'"))
  }

  if(start == n){
    message("When start == n, this function does not really do anything")
  }

  chunk_size <- as.integer(chunk_size)

  if (chunk_size > n) {
    n.fx <- abs(n.fx)
    if(n.fx < 1){
      warning(paste("n.fx must be a positive numeric value >= 1, as it is meant",
                    "to increment n and chunk_size if n < chunk_size\n",
                    "Automatically correcting input", n.fx, "to",
                    paste0(1+n.fx, ";"), "if this is not what you intended,",
                    "re-run with a compatible n.fx value"))
      n.fx <- 1+n.fx
    }
    chunk_size <- ceiling(min(c(
      (n*n.fx),
      limit))
    )

    message(paste0("n < chunk_size; since chunk_size cannot exceed ", n,
                   ", setting chunk_size to ", chunk_size))
    n <- ceiling(n*n.fx)

  }

  nchunks <- ceiling(as.integer((n-start+1))/chunk_size)
  print(paste("Total number of chunks is", nchunks))
  from <- seq(start, n, by = chunk_size)
  to <- from + (chunk_size - 1)
  to[to >= n] <- n
  to <- as.integer(to)
  per <- to - from + 1
  out <- data.frame(from = from, to = to, size = per)
  stopifnot(sum(out[["size"]]) == as.integer(n) - start + 1)
  return(out)
}
