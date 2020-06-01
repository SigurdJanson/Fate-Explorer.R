#' GetSkillSourceRadioButtonNames
#' Helper function to create radio button names
GetSkillSourceRadioButtonNames <- function( IsCharacterLoaded = FALSE ) {
  ChoiceNames <- paste0("<i class='fa fa-", 
                        c("question", "sliders-h", "bars"),
                        "'>  ", 
                        i18n$t(c("Unchecked", "Manual", "Character Skills")), 
                        "</i>")
  ChoiceValues = c("NoSkill", "ManualSkill", "CharSkill")
  if(!IsCharacterLoaded) {
    ChoiceNames <- ChoiceNames[-3]
    ChoiceValues <- ChoiceValues[-3]
  }
  
  return(list(Names = ChoiceNames, Values = ChoiceValues))
}



#' CreateSkillSourceRadioButton
#' @param IsCharacterLoaded 
#' @return Creates a 'shiny widgets' radio button group to select the source
#' of skill values
CreateSkillSourceRadioButton <- function( IsCharacterLoaded = FALSE ) {
  Choices <- GetSkillSourceRadioButtonNames(IsCharacterLoaded)
  radioGroupButtons(inputId = "rdbSkillSource",
                    choiceNames = Choices$Names, choiceValues = Choices$Values, 
                    justified = TRUE
  )
}



#' UpdateSkillSourceRadioButton
#' @param IsCharacterLoaded 
#' @return Creates a 'shiny widgets' radio button group to select the source
#' of skill values
UpdateSkillSourceRadioButton <- function(session, IsCharacterLoaded = FALSE ) {
  Choices <- GetSkillSourceRadioButtonNames(IsCharacterLoaded)
  updateRadioGroupButtons(session, inputId = "rdbSkillSource",
                    choiceNames = Choices$Names, choiceValues = Choices$Values)
}