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



#' gicon
#' Replacement for the `icon()` function of shiny. Accepts other icon libs.
#' @param name 
#' @param class 
#' @param lib 
#' @return 
#' @source https://stackoverflow.com/questions/55163719/r-shiny-how-to-use-fontawesome-pro-version-with-the-icon-function
gicon <- function (name, class = NULL, lib = "font-awesome") {
  
  prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon", gameicon = "game-icon")
  prefix <- prefixes[[lib]]
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ", 
         paste0("\"", names(prefixes), "\"", collapse = ", "))
  }
  # set class name to get the icon
  iconClass <- ""
  if (!is.null(name)) {
    prefix_class <- prefix
    if (prefix_class == "fa" && name %in% font_awesome_brands) {
      prefix_class <- "fab"
    }
    iconClass <- paste0(prefix_class, " ", prefix, "-", name)
  }
  if (!is.null(class)) 
    iconClass <- paste(iconClass, class)

  iconTag <- tags$i(class = iconClass)
  if (lib == "font-awesome") {
    htmlDependencies(iconTag) <- htmlDependency("font-awesome", "5.3.1", 
                                                "www/shared/fontawesome", package = "shiny", 
                                                stylesheet = c("css/all.min.css", "css/v4-shims.min.css"))  
  }
  htmltools::browsable(iconTag)
}