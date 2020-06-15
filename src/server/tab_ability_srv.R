# ABILITY TAB

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
}, ignoreNULL = TRUE, ignoreInit = TRUE)



# Main Panel ----
# Initiate skill roll
LastAbilityRoll <- eventReactive(input$doAbilityRoll, {
  return(AbilityRoll())
})

# Display result of skill roll
output$AbilityRoll <- renderText({
  Value <- LastAbilityRoll()
  if (is.numeric(Value)) {
    SuccessStr <- VerifyAbilityRoll(Value, input$inpAbility, input$inpAbilityMod)
    Result     <- RenderRollKeyResult(Value, SuccessStr)
  }
  return(paste((Result), collapse=""))
})
