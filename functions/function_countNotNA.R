#function for counting the number of values that are not NAs

count.not.na <- function(vec) {
  n <- length(vec)
  nas <- which(is.na(vec))
  x <- n-length(nas)
  return(x)
}

