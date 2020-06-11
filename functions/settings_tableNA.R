#change the table function to include NAs by default

table <- function (..., useNA = 'ifany') base::table(..., useNA = useNA)