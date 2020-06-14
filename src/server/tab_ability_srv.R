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
    base.style   <- "margin:0 40% 0 40%;border: 1px solid lightgray;border-radius:3px;height:18rem"
    value.style  <- "text-align: center;font-size: 440%;padding-top:5%"
    result.style <- "text-align: center;font-size: 140%"

    SuccessStr   <- VerifyAbilityRoll(Value, input$inpAbility, input$inpAbilityMod)
    if (SuccessStr == "Fumble") 
      SuccessIcon  <- "game-icon game-icon-crowned-skull col-fumble"#"fa fa-skull fa-fw col-fumble"
    else if (SuccessStr == "Critical") 
      SuccessIcon  <- "game-icon game-icon-laurel-crown col-critical"
    else if (SuccessStr == "Success") 
      SuccessIcon  <- "game-icon game-icon-trophy-cup col-success" # "fa fa-trophy fa-fw"
    else # Fail
      SuccessIcon  <- "game-icon game-icon-spectre col-fail"#evil-eyes ghost imp-laugh spectre
    
    Result <- div(tags$p(tags$i(class = SuccessIcon), Value, style = value.style), 
                  #tags$p(),#, style = result.style),
                  tags$p(i18n$t(SuccessStr), style = result.style),
                  class = "roll-ab-div")
  }
  return(paste((Result), collapse=""))
})
