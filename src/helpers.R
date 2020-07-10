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
#' Renders an icon from an icon font.
#' @param name Name of icon according to icon lib (i.e. without prefixes like the
#' "fa-" and "glyphicon-" prefixes).
#' @param class Additional classes to customize the style of the icon.
#' @param lib Icon library to use ("font-awesome", "glyphicon", or "gameicon)
#' @return An icon element (which is a browsable object).
#' @note Replacement for the `icon()` function of shiny. Accepts other icon libs.
#' @source https://stackoverflow.com/questions/55163719/r-shiny-how-to-use-fontawesome-pro-version-with-the-icon-function
gicon <- function (name, class = NULL, lib = "fe") {

  prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon", gameicon = "game-icon", fe = "icon-fe")
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



#' RenderRollConfirmation
#' The outout of this function provides the confirmation message of a fumble/critical
#' in a format that can directly be used in `renderText`.
#' @param RollResult String indicating critical, succes, fail or fumble.
#' @param RollValue Value of the confirmation roll (numeric, optional).
#' @param i18n A `shiny.i18n` object.
#' @return Character string
RenderRollConfirmation <- function( RollResult, RollValue = NA, i18n = NULL ) {
  Message <- switch(RollResult,
               Fumble   = "Still a Fumble",
               Critical = "Critical confirmed",
               Success  = "Critical lost",
               Fail     = "Fumble avoided",
               "")
  if (isTruthy(i18n)) Message <- i18n$t(Message)
  if (isTruthy(RollValue)) {
    Message <- paste0(Message, " (", RollValue, ")")
  }
  
  return(Message)
}



#' RenderRollKeyResult
#' The outout of this function provides the html representation
#' to display a roll result in a format that can directly be used in `renderText`.
#' @param KeyResult String indicating critical, succes, fail or fumble.
#' @param keyValue Value of the confirmation roll (numeric).
#' @param FurtherValue An additional number or string that will be appended to `KeyResult`
#' in brackets.
#' @return The result from these functions is a tag object, which can be 
#' converted using `as.character()`.
RenderRollKeyResult <- function(KeyResult, KeyValue, FurtherValue = NULL, 
                                KeyUnit = c("dr", "ql", "hp")) {
  if (grepl("Fumble", KeyResult))
    SuccessIcon  <- "icon icon-fe-crowned-skull col-fumble ico-success"
  else if (grepl("Critical", KeyResult)) 
    SuccessIcon  <- "icon-fe icon-fe-laurel-crown col-critical ico-success"
  else if (grepl("Success", KeyResult))
    SuccessIcon  <- "icon-fe icon-fe-laurels col-success ico-success"
  else if (grepl("Fail", KeyResult))
    SuccessIcon  <- "icon-fe icon-fe-spectre col-fail ico-success"
  else SuccessIcon  <- ""

  if (isTruthy(FurtherValue))
    KeyResult <- paste0(i18n$t(KeyResult), " (", FurtherValue, ")")
  else 
    KeyResult <- i18n$t(KeyResult)
  
  if (!missing(KeyUnit))
    KeyUnit <- match.arg(KeyUnit)
  else
    KeyUnit <- "dr"
  ParClass <- "keyval"
  
  Result <- div(tags$p( tags$i(class = SuccessIcon, .noWS = c("after")), 
                        span(format(KeyValue, width = 2, justify = "right"), class = KeyUnit), 
                        class = ParClass, .noWS = c("before") ), 
                tags$p(KeyResult, class = "keyresult"),
                class = "roll-keyval")
}
