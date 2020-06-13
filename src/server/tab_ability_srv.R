# ABILITY TAB

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
      SuccessIcon  <- "fa fa-skull fa-fw col-fumble"
    else if (SuccessStr == "Critical") 
      SuccessIcon  <- "fa fa-trophy fa-fw col-critical"
    else if (SuccessStr == "Success") 
      SuccessIcon  <- "fa fa-trophy fa-fw col-success"
    else # Fail
      SuccessIcon  <- "fa fa-times fa-fw col-fail"
    
    Result <- div(tags$p(Value, style = value.style), 
                  tags$p(tags$i(class = SuccessIcon), style = result.style),
                  tags$p(i18n$t(SuccessStr), style = result.style),
                  class = "roll-ab-div")#style = base.style)
  }
  return(paste((Result), collapse=""))
})
