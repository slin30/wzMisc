#' Generate random validly formatted CAS RNs
#'
#' Make random CAS RNs of valid length format, without checksum consideration
#'
#' @importFrom magrittr %>%
#' @param n integer. How many random CAS RNs do you wish to generate? \code{numeric} or \code{character}
#' input will be coerced to integer, if possible. Negative (valid) input will be transformed to
#' positive via \code{abs}
#'
#' @details
#' Generate \emph{n} random CAS RNs with valid CAS digit length (per section and overall). Random applies to
#' all three sections of a CAS RN; for the first section, lengths will also be of random length, between \code{2:7}.
#'
#' This function does not perform any additional validation, which means duplicate CAS are possible.
#' More importantly, there is no last (checksum) digit validation.
#'
#' @note
#' This is mainly useful as a testing tool for \code{\link{cas_checkSum}}. See examples.
#' @export
#'
#' @return
#' A vector of random CAS RNs of length \emph{n}.
#'
#' @examples
#' set.seed(1)
#' random_CASRN <- cas_rand(1E4L)
#' length(unique(random_CASRN))
#' table(nchar(random_CASRN)-2) #-2 for two dashes
#' #using cas_detect and cas_checkSum
#' table(cas_detect(unique(random_CASRN)))
#' table(cas_checkSum(unique(random_CASRN)))
cas_rand <- function(n) {
  n <- abs(as.integer(n))
  x <- n*10
  samps <- sample(0:9, x, replace = TRUE)
  lmax  <- x-7
  starts <- sample(lmax, n)

  p1 <- lapply(starts, function(f) samps[f:(f + sample(1:6, 1))]) %>%
    lapply(X = ., function(f) paste(f, collapse = ""))
  p2 <- replicate(n, sample(0:9, 2, replace = TRUE), simplify = FALSE) %>%
    lapply(X = ., function(f) paste(f, collapse = ""))
  p3 <- replicate(n, sample(0:9, 1), simplify = FALSE) %>%
    Map(as.character, .)
  out <- list(p1, p2, p3)
  do.call(paste, c(out, sep = "-"))
}
