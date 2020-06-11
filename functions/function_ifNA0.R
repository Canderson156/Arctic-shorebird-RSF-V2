#function that that replaces NAs with 0s and leaves all other values as is

if.na.0 <- function(x){
  ifelse(is.na(x), 0, x)
}