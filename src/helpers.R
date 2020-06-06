# Various helper functions



#' replace_umlauts
#' Replaces German umlauts ä with ae, ö with oe, ...
#' @param x A character vector
#' @return The modified string with umlauts replaced.
replace_umlauts <- function(x) {
  umlauts <- "äöü"
  UMLAUTS <- "ÄÖÜ"
  x <- gsub(pattern = paste0("([", UMLAUTS, "])"), replacement = "\\1E", x)
  x <- gsub(pattern = paste0("([", umlauts, "])"), replacement = "\\1e", x)
  Replacement <- "AOUaou" 
  x <- chartr(old = paste0(UMLAUTS, umlauts), new = Replacement, x)
  return(x)
}

