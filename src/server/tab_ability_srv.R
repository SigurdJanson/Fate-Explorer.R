# ABILITY TAB

LastAbilityConfirmationRoll <- reactiveVal()


# Panel Ability Source: Selection buttons for ability source ----
output$ShowAbilitySoureSelection <- reactive({
  return( !is.null(Character$Attr) )
})
outputOptions(output, 'ShowAbilitySoureSelection', suspendWhenHidden = FALSE)


# Panel: Character Abilities: Selection buttons for ability source ----
output$ShowCharacterAbilities <- reactive({
  return( !is.null(Character$Attr) & input$rdbAbilitySource ==  "CharAbility" )
})
outputOptions(output, 'ShowCharacterAbilities', suspendWhenHidden = FALSE)


observeEvent(input$rdbCharacterAbility, {
  Abilities <- Character$Attr
  ActiveAbility <- Abilities[1, names(Abilities) == input$rdbCharacterAbility ]
  
  updateSliderInput(session, "inpAbility", value = ActiveAbility)
  # TODO: can I trigger a roll instantly?
}, ignoreNULL = TRUE, ignoreInit = TRUE)


# Value slider
observeEvent(input$inpAbility, {
  isolate(
    updateRadioGroupButtons(session = session, inputId = "rdbCharacterAbility", selected = NA)
  )
  LastAbilityConfirmationRoll(NULL) # is invalid after change of ability
}, ignoreNULL = TRUE, ignoreInit = TRUE)



# Main Panel ----
observeEvent(input$doAbilityConfirmationRoll, {
  LastAbilityConfirmationRoll( AbilityRoll() )
})

# Initiate ability roll ans store die result
LastAbilityRoll <- eventReactive(input$doAbilityRoll, {
  isolate(LastAbilityConfirmationRoll(NULL)) # is invalid after re-roll
  return(AbilityRoll())
}, ignoreInit = TRUE)


# Display result of skill roll
output$AbilityRoll <- renderText({
  Value <- LastAbilityRoll()
  Confirmation <- LastAbilityConfirmationRoll()
  
  SuccessStr <- VerifyAbilityRoll(Value, input$inpAbility, input$inpAbilityMod)
  # Critical or Fumble waiting for confirmation
  if (SuccessStr == "Critical" || SuccessStr == "Fumble") {
    if (!is.null(Confirmation)) {
      ConfirmationResult <- VerifyAbilityRoll(Confirmation, input$inpAbility, input$inpAbilityMod)
      SuccessStr <- VerifyConfirmation( SuccessStr, ConfirmationResult )
      ConfirmationStr <- i18n$t(switch(SuccessStr,
                                       Fumble   = "Still a Fumble",
                                       Critical = "Critical confirmed",
                                       Success  = "Critical was lost",
                                       Fail     = "Fumble avoided",
                                       ""))
      ConfirmationStr <- paste0(i18n$t(ConfirmationStr), " (", Confirmation, ")")
    } else {
      Label <- i18n$t(ifelse(SuccessStr == "Critical", "Confirm!", "Avert!"))
      ConfirmationStr <- actionLink("doAbilityConfirmationRoll", Label)
    }
  } else {
    ConfirmationStr <- NULL
  }
  
  Result <- RenderRollKeyResult(Value, SuccessStr)
  if (!is.null(ConfirmationStr)) # add confirmation <div/>
    Result <- div(Result, div( ConfirmationStr ),
                  class = "shiny-html-output shiny-bound-output roll")
  
  return(paste((Result), collapse=""))
})

