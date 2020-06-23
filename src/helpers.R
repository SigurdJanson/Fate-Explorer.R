# Various helper functions


# MODEL -------------

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


# VIEW -----------

#' gicon
#' Replacement for the `icon()` function of shiny. Accepts other icon libs.
#' @param name Name of icon according to icon lib (i.e. without prefixes like the
#' "fa-" and "glyphicon-" prefixes).
#' @param class Additional classes to customize the style of the icon.
#' @param lib Icon library to use ("font-awesome", "glyphicon", or "gameicon)
#' @return An icon element (which is a browsable object).
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



RenderRollConfirmation <- function( Success, Value = NA, i18n = NULL ) {
  Message <- switch(Success,
               Fumble   = "Still a Fumble",
               Critical = "Critical confirmed",
               Success  = "Critical lost",
               Fail     = "Fumble avoided",
               "")
  if (isTruthy(i18n)) Message <- i18n$t(Message)
  if (isTruthy(Value)) {
    Message <- paste0(Message, " (", Value, ")")
  }
  
  return(Message)
}



RenderRollKeyResult <- function(RollValue, RollResult) {
  value.style  <- "font-size: 440%"
  result.style <- "font-size: 140%"
  
  if (grepl("Fumble", RollResult))
    SuccessIcon  <- "game-icon game-icon-crowned-skull col-fumble ico-success"
  else if (grepl("Critical", RollResult)) 
    SuccessIcon  <- "game-icon game-icon-laurel-crown col-critical ico-success"
  else if (grepl("Success", RollResult))
    SuccessIcon  <- "game-icon game-icon-laurels col-success ico-success"
  else if (grepl("Fail", RollResult))
    SuccessIcon  <- "game-icon game-icon-spectre col-fail ico-success"
  else SuccessIcon  <- ""
  
  if (RollValue < 0)  {
    RollValue <- "·"
    SuccessIcon  <- "game-icon game-icon-dice-eight-faces-eight"
  }
  
  Result <- div(tags$p( tags$i(class = SuccessIcon), 
                        RollValue, style = value.style ), 
                tags$p(i18n$t(RollResult), style = result.style),
                class = "roll-keyval")
}
