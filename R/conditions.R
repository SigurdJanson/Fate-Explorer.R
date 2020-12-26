# R6 classes to handle character conditions like fear, 
# For details and underlying rationale see "Analysis of States & Conditions" 
# in the documentation folder

require(R6)
require(jsonlite)


ConditionManager <-  R6Class("CharacterSkills", 
  private = list(
    Conditions = list()
    
  ),#private
  
  public = list(
    #' Constructor
    #' @param Filename Path and file to a json file that contains condition definitions
    #' @return `self`
    initialize = function(Filename = "./R/data/conditions_de.json") {
      if (!file.exists(Filename)) stop("Conditions not found")
      
      data <- read_json(Filename, simplifyVector = TRUE)
      for(cond in data) {
        Conditions <- c(Conditions, ConditionBase$new(cond))
      }
      
      invisible(self)
    },
    
    #' ChangeConditionLevel
    #' Set the level of the condition
    #' @param condId A vector of condition id strings
    #' @param to a new level [0..4]
    #' @param by an increment/decrement (-4 to +4)
    #' @note Use either `to` or `by` but not both
    #' @return `invisible(self)`
    #' @export
    ChangeConditionLevel = function(condId, to, by = NULL) {
      for (cond in Conditions) {
        if (cond$GetId() %in% condId) cond$ChangeLevel(to, by)
      }
      invisible(self)
    }
    
  )#public
)



# ConditionBase class -------------

#' Base class for character conditions
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
ConditionBase <- R6Class("ConditionBase", 
  private = list(
    Name = "",
    Id = "",
    Url = "",
    Level = 0L,
    Modifiers = NULL,
    Layovers = NULL
  ), # private
  
  
  public = list(
    #' Constructor
    #' @param Data A data structure as imported from the conditions json file
    #' @return `self`
    initialize = function(Data) {
      private$Name <- Data[["name"]]
      private$Id <- Data[["attrID"]]
      private$Url <- Data[["url"]]
      
      if (is.null(Data[["modifiers"]]) ||
          is.na(Data[["modifiers"]]) ||
          length(Data[["modifiers"]]) == 0)
        private$Modifiers <- NULL # make sure that it's NULL if empty
      else
        private$Modifiers <- Data[["modifiers"]]

      if (is.null(Data[["layovers"]]) ||
          is.na(Data[["layovers"]]) ||
          length(Data[["layovers"]]) == 0)
        private$Layovers <- NULL # make sure that it's NULL if empty
      else
        private$Layovers <- Data[["layovers"]]
      
      invisible(self)
    },
    
    GetName = function() {
      return(private$Name)
    },

    GetId = function() {
      return(private$Id)
    },

    GetUrl = function() {
      return(private$Url)
    },
    
    GetLevel = function() {
      return(private$Level)
    },

    GetLayoverIds = function() {
      if (is.null(private$Layovers)) return(NULL)
      
      return(private$Layovers$conditionID)
    },
    
    #' ConditionBase::ChangeLevel
    #' Set the level of the condition
    #' @param to a new level [0..4]
    #' @param by an increment/decrement (-4 to +4)
    #' @param Others Other conditions that may be required for layover effects
    #' @note Use either `to` or `by` but not both
    #' @return `invisible(self)`
    ChangeLevel = function(to = NULL, by = NULL, Others = NULL) {
      if (is.null(to) && is.null(by))
        stop("To change the condition at least one argument must be present (but both are NULL)")
      if (!is.null(to) && !is.null(by))
        stop("Cannot use two arguments to and by at the same time")
      if (is.null(to) && (by < -4L || by > +4L))
        stop("Argument 'by' outside allowed range")
      if (is.null(by) && (to < 0L || to > +4L))
        stop("Argument 'by' outside allowed range")
      if (!is.null(Others) && length(Others) == 0L) Others <- NULL
      
      # Save old level and determine the new one
      OldLevel <- private$Level

      if (!is.null(to))
        NewLevel <- to
      else
        NewLevel <- private$Level + by
      
      if (NewLevel < 0L) NewLevel = 0L
      if (NewLevel > 4L) NewLevel = 4L
      
      if (OldLevel != NewLevel) {
        if (!is.null(private$Layovers)) {
          if (is.null(Others)) stop("Cannot handle layovers because 'Others' conditions are missing")

          for (lo in 1:nrow(private$Layovers)) {
            OldValue <- ifelse(OldLevel == 0, 0L, private$Layovers[lo, paste0("level", OldLevel)])
            NewValue <- ifelse(NewLevel == 0, 0L, private$Layovers[lo, paste0("level", NewLevel)]) 
            # New layover - old layover
            Delta <- NewValue - OldValue
            if (Delta != 0L) {
              # Find layover condition in this loop
              Found <- sapply(Others, function(x) x$GetId() == private$Layovers[lo, "conditionID"])
              Found <- which(Found) # get index
              if (length(Found) == 0L) stop("Layover condition can not be found")
              Found <- Others[[Found]] # get object
              # change layover: by = delta
              Found$ChangeLevel(by = Delta)
            }
          }
        }
      }
      
      private$Level <- NewLevel
      invisible(self)
    },
    
    CanDo = function(Action, ...) {
      if (GetModifier(Action, ...) == -99L) return(FALSE)
      return(TRUE)
    },

    
  #' GetModifier
  #' @param Action an ID identifies the action(s) for which modifiers are requested
  #' @param ... 
  #' @return An integer representing a check modifier
  #' @export
  #' @note Not vectorized
    GetModifier = function(Action, ...) {
      if (private$Level == 0L) return(0L)
      
      # Find first modifier that fits `Action`
      for (Row in 1:length(private$Modifiers$actionID)) {
        if (startsWith(Action, private$Modifiers$actionID[Row])) {
          Col <- paste0("level", private$Level)
          return(private$Modifiers[[Row, Col]])
        }
      }
      
      # No instructions for `Action` found
      return(0L)
    }

  )#public
)