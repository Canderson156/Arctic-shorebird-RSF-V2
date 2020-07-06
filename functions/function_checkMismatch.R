#compare two character vectors to see where they differ




check_mismatch<- function(...) {
  dots <- list(...)
  if (!length(dots)) {
    out <- list()
  } else {
    nms <- as.character(match.call()[-1])
    out <- lapply(seq_along(dots), function(i) {
      b <- unique(unlist(dots[-i]))
      b[!b %in% dots[[i]]]
    })
    out <- replace(out, sapply(out, is.null), list(dots[[1]][0]))
    names(out) <- nms
  }
  out
}



