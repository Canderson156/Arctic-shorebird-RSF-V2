mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition , ] <- .data[condition, ] %>% mutate(...,  na.rm = TRUE)
  .data
}




data <- data.frame(col1 = rep(c("A", "B", "C", "D", "E"), each = 10), 
                   col2 = base::sample(c(1,2,3), 50, replace = TRUE))

data2 <- data.frame(col1 = rep(c("A", "B", "C", "D", "E"), each = 10), 
                    col2 = base::sample(c(1,2,3, NA), 50, replace = TRUE))



test1 <- data %>%
  mutate_cond(col1 == 'A', col2 = col2*2)


test2 <- data2 %>%
  mutate_cond(col1 == 'A', col2 = col2*2)
  
