# COMBAT TAB


FightVal <- reactiveValues(Action = "", Roll = NA, 
                           Success = "", Damage = 0,
                           ConfirmRoll = 0, Confirmation = NA)


# VALUES -------------------------------
# Source of Weapons Panel
output$ShowPredefinedWeapons <- reactive({
  return( !is.null(Character$Weapons) && input$PredefinedWeapon )
})
outputOptions(output, 'ShowPredefinedWeapons', suspendWhenHidden = FALSE)

observeEvent(input$PredefinedWeapon,{
  updateSelectizeInput(session, "CombatSelectWeapon", selected = NA)
})

observeEvent(input$CombatSelectWeapon, {
  if (input$PredefinedWeapon) {
    Weapon <- as.character(input$CombatSelectWeapon)
    # Update values
    updateNumericInput(session, "ATValue", value = Character$Weapons["AT", Weapon])
    updateNumericInput(session, "PAValue", value = Character$Weapons["PA", Weapon])
    updateNumericInput(session, "DamageDieCount", value = Character$Weapons["DamageDice", Weapon])
    updateNumericInput(session, "Damage", value = Character$Weapons["DamageMod", Weapon])
  } else {
    updateNumericInput(session, "ATValue", value = 6)
    updateNumericInput(session, "PAValue", value = 3)
    updateNumericInput(session, "DamageDieCount", value = 1)
    updateNumericInput(session, "Damage", value = 0)
  }
}, ignoreNULL = FALSE)

  
# ACTIONS -------------------------------
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
  Result <- paste0(i18n$t(FightVal$Action), ": ", 
                   FightVal$Roll, " - ", 
                   i18n$t(FightVal$Success))
  cat(Result)
})

# Confirmation Panel
output$ShowCombatConfirm <- reactive({
  return(!is.na(FightVal$ConfirmRoll))
})
outputOptions(output, 'ShowCombatConfirm', suspendWhenHidden = FALSE)

output$CombatConfirm <- renderPrint({
  if (FightVal$Success == "Fumble") {
    Result <- "Fumble" #icon("frown-open")
  } else if (FightVal$Success == "Critical") {
    Result <- "Critical confirmed"
  } else if (FightVal$Success == "Success") {
    Result <- "Critical was lost"
  } else if (FightVal$Success == "Fail") {
    Result <- "Fumble avoided"
  } else Result <- ""
  
  cat(i18n$t(Result))
})

# Damage Panel
output$ShowCombatDamage <- reactive({
  return( !is.na(FightVal$Damage) )
})
outputOptions(output, 'ShowCombatDamage', suspendWhenHidden = FALSE)

output$CombatDamage <- renderPrint({
  cat(i18n$t("Hit points"), ": ", FightVal$Damage, sep = "")
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
  
  cat(i18n$t(Result))
})

observeEvent(input$doCombatFumble, {
  FightVal$EffectOfFumble <- CombatFumbleRoll()
})
