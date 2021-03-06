% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reverse_lookup.R
\name{reverse_lookup}
\alias{reverse_lookup}
\title{Swap key-value in lookup vector}
\usage{
reverse_lookup(x)
}
\arguments{
\item{x}{a named vector, or something coercible to one, with a 1:1 key:value ratio}
}
\value{
A vector of the same length as \emph{x}, with \emph{x}'s keys as value(s), and \emph{x}'s values as key(s)
}
\description{
Swap a key-value pair's key with value and vice-versa
}
\details{
This is a convenient way to "flip" the key-value relationship for a lookup vector.
The specific use-case is for a simple key-value vector or list, which means a nested
hierarchy or a 1:many relationship (key:value) will fail.
}
\examples{
lookup_list <- c(LETTERS[1:5])
names(lookup_list) <- c(letters[1:5])
reverse_lookup(lookup_list)
reverse_lookup(reverse_lookup(lookup_list)) #returns original

\dontrun{
reverse_lookup(LETTERS[1:5]) #missing names!
bad_list <- list(a = 1:2, b = 3, c = letters[1:5])
reverse_lookup(bad_list) #error
reverse_lookup(unlist(bad_list)) #no error, but probably not what you intended
}
}
