# COMBAT TAB


FightVal <- reactiveValues(Action = "", Roll = NA, 
                           Success = "", Damage = 0,
                           ConfirmRoll = 0, Confirmation = NA)


# VALUES -------------------------------
observeEvent(input$CombatSelectWeapon, {
  Weapon <- as.character(input$CombatSelectWeapon)
  # Update values
  updateNumericInput(session, "ATValue", value = Character$Weapons["AT", Weapon])
  updateNumericInput(session, "PAValue", value = Character$Weapons["PA", Weapon])
  updateNumericInput(session, "DamageDieCount", value = Character$Weapons["DamageDice", Weapon])
  updateNumericInput(session, "Damage", value = Character$Weapons["DamageMod", Weapon])
  #input$PAValue <- Character$Weapons[3, Weapon] #"PA"
  #input$DamageDieCount <-  Character$Weapons[4, Weapon] #"DamageDice"
  #input$Damage <-  Character$Weapons[5, Weapon] #"DamageMod"
})
  
observeEvent(input$doAttackThrow, {
  FightVal$Action  <- "Attack"
  FightVal$Roll    <- CombatRoll()
  FightVal$Success <- VerifyCombatRoll(FightVal$Roll, input$ATValue)
  # Level of success
  if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
    FightVal$ConfirmRoll <- CombatRoll()
    Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, input$ATValue)
    FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
    if (!FightVal$Confirmation)
      FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")
  } else {
    FightVal$ConfirmRoll <- NA
  }
  FightVal$EffectOfFumble <- NA
  # Damage
  if (FightVal$Success == "Critical")
    FightVal$Damage <- 2 * DamageRoll(input$DamageDieCount, input$Damage)
  else if  (FightVal$Success == "Success")
    FightVal$Damage <- DamageRoll(input$DamageDieCount, input$Damage)
  else 
    FightVal$Damage <- NA
})


# ACTIONS -------------------------------
observeEvent(input$doParryThrow, {
  FightVal$Action  <- "Parry"
  FightVal$Roll    <- CombatRoll()
  FightVal$Success <- VerifyCombatRoll(FightVal$Roll, input$PAValue)
  FightVal$Damage  <- NA
  # Level of success
  if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
    FightVal$ConfirmRoll <- CombatRoll()
    Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, input$ATValue)
    FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
    if (!FightVal$Confirmation)
      FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")
  } else {
    FightVal$ConfirmRoll <- NA
  }
  FightVal$EffectOfFumble <- NA
})


observeEvent(input$doDodge, {
  FightVal$Action  <- "Dodge"
  FightVal$Roll    <- CombatRoll()
  FightVal$Success <- VerifyCombatRoll(FightVal$Roll, input$DodgeValue)
  FightVal$Damage  <- NA
  # Level of success
  if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
    FightVal$ConfirmRoll <- CombatRoll()
    Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, input$ATValue)
    FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
    if (!FightVal$Confirmation)
      FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")
  } else {
    FightVal$ConfirmRoll <- NA
  }
  FightVal$EffectOfFumble <- NA
})


output$CombatAction <- renderPrint({
  Result <- paste0(FightVal$Action, ": ", FightVal$Roll, " - ", FightVal$Success)
  cat(Result)
})

# Confirmation Panel
output$ShowCombatConfirm <- reactive({
  return(!is.na(FightVal$ConfirmRoll))
})
outputOptions(output, 'ShowCombatConfirm', suspendWhenHidden = FALSE)

output$CombatConfirm <- renderPrint({
  if (FightVal$Success == "Fumble") {
    Result <- "Fumble!" #icon("frown-open")
  } else if (FightVal$Success == "Critical") {
    Result <- "Critical confirmed!"
  } else if (FightVal$Success == "Success") {
    Result <- "Critical was lost :-("
  } else if (FightVal$Success == "Fail") {
    Result <- "Fumble avoided :-)"
  } else Result <- ""
  
  cat(Result)
})

# Damage Panel
output$ShowCombatDamage <- reactive({
  return( !is.na(FightVal$Damage) )
})
outputOptions(output, 'ShowCombatDamage', suspendWhenHidden = FALSE)

output$CombatDamage <- renderPrint({
  cat("Hit points: ", FightVal$Damage)
})


# Fumble Panel
output$ShowCombatFumble <- reactive({
  return( FightVal$Success == "Fumble" )
})
outputOptions(output, 'ShowCombatFumble', suspendWhenHidden = FALSE)

output$CombatFumble <- renderPrint({
  if(!is.na(FightVal$EffectOfFumble)) {
    Result <- GetCombatFumbleEffect(FightVal$EffectOfFumble)
  } else Result <- ""
  
  cat(Result)
})

observeEvent(input$doCombatFumble, {
  FightVal$EffectOfFumble <- CombatFumbleRoll()
})
