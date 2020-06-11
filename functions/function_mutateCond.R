## conditional mutation
## works like mutate(), but only performs the operation on a subset of the dataset, returns the whole dataset
## example: prism2 <- prism2 %>% mutate_cond(region == 'Quebec', UTM_Zone = replace_na(UTM_Zone, 15))


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition , ] <- .data[condition, ] %>% mutate(...,  na.rm = TRUE)
  .data
}
