#' Swap key-value in lookup vector
#'
#' Swap a key-value pair's key with value and vice-versa
#' @param
#' x a named vector, or something coercible to one, with a 1:1 key:value ratio
#' @details
#' This is a convenient way to "flip" the key-value relationship for a lookup vector.
#' The specific use-case is for a simple key-value vector or list, which means a nested
#' hierarchy or a 1:many relationship (key:value) will fail.
#' @return
#' A vector of the same length as \emph{x}, with \emph{x}'s keys as value(s), and \emph{x}'s values as key(s)
#' @export
#' @examples
#' lookup_list <- c(LETTERS[1:5])
#' names(lookup_list) <- c(letters[1:5])
#' reverse_lookup(lookup_list)
#' reverse_lookup(reverse_lookup(lookup_list)) #returns original
#'
#' \dontrun{
#' reverse_lookup(LETTERS[1:5]) #missing names!
#' bad_list <- list(a = 1:2, b = 3, c = letters[1:5])
#' reverse_lookup(bad_list) #error
#' reverse_lookup(unlist(bad_list)) #no error, but probably not what you intended
#' }
reverse_lookup <- function(x) {
  if(is.null(names(x))) {
    stop("Input missing names attribute")
  }
  chk <- all(sapply(x, length) == 1)
  if(chk == "FALSE") {
    stop(paste("values in x are not all length 1",
               "This function will not preserve name integrity under such scenarios")
    )
  }
  x <- unlist(x)
  new_val <- names(x)
  new_nam <- unname(x)
  names(new_val) <- new_nam
  new_val
}
