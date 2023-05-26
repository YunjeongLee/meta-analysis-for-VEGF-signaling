generate_plabel <- function(pval) {
  if ((pval > 0.01) & (pval <= 0.05)) {
    res <- "p < 0.05"
  }
  else if (pval <= 0.01) {
    res <- "p < 0.01"
  }
  return(res)
}