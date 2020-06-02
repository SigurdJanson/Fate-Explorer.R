# COMBAT TAB


FightVal <- reactiveValues(Action = "", Roll = NA, 
                           Success = "", Damage = NA,
                           ConfirmRoll = NA, Confirmation = FALSE)
ShowConfirmation <- FALSE

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
    updateNumericInput(session, "ATValue", value = 6L)
    updateNumericInput(session, "PAValue", value = 3L)
    updateNumericInput(session, "DamageDieCount", value = 1L)
    updateNumericInput(session, "Damage", value = 0L)
  }
}, ignoreNULL = FALSE)

  
# ACTIONS -------------------------------
doCombatRollBase <- function(Action) {
  ShowConfirmation <- FALSE
  FightVal$Action  <- Action
  FightVal$Roll    <- CombatRoll()

  Penalty <- as.numeric(input$CombatPenalty)
  if (Action == "Attack") {
    Check <- input$ATValue
  } else if (Action == "Parry") {
    Check <- input$PAValue
  } else if (Action == "Dodge") {
    Check <- input$DodgeValue
  }
  FightVal$Success <- VerifyCombatRoll(FightVal$Roll, Check, Penalty)
  if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
    FightVal$ConfirmRoll <- "Required"
  } else {
    FightVal$ConfirmRoll <- NA
  }
  FightVal$Confirmation <- FALSE
  FightVal$EffectOfFumble <- NA
  FightVal$Damage <- NA
}

observeEvent(input$doAttackThrow, {
  doCombatRollBase("Attack")
  # FightVal$Action  <- "Attack"
  # FightVal$Roll    <- CombatRoll()
  # FightVal$Success <- VerifyCombatRoll(FightVal$Roll, 
  #                                      input$ATValue, 
  #                                      as.numeric(input$CombatPenalty))
  # Level of success
  # if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
    # FightVal$ConfirmRoll <- CombatRoll()
    # Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, 
    #                                  input$ATValue, 
    #                                  as.numeric(input$CombatPenalty))
    # FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
  #   if (!FightVal$Confirmation)
  #     FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")
  # } else {
  #   FightVal$ConfirmRoll <- NA
  # }
  # FightVal$EffectOfFumble <- NA
  # # Damage
  # if (FightVal$Success == "Critical")
  #   FightVal$Damage <- 2 * DamageRoll(input$DamageDieCount, input$Damage)
  # else if  (FightVal$Success == "Success")
  #   FightVal$Damage <- DamageRoll(input$DamageDieCount, input$Damage)
  # else 
  #   FightVal$Damage <- NA
})


# Parry Roll
observeEvent(input$doParryThrow, {
  doCombatRollBase("Parry")
  # FightVal$Action  <- "Parry"
  # FightVal$Roll    <- CombatRoll()
  # FightVal$Success <- VerifyCombatRoll(FightVal$Roll, 
  #                                      input$PAValue, 
  #                                      as.numeric(input$CombatPenalty))
  # FightVal$Damage  <- NA
  # # Level of success
  # if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
  #   FightVal$ConfirmRoll <- CombatRoll()
  #   Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, 
  #                                    input$PAValue, 
  #                                    as.numeric(input$CombatPenalty))
  #   FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
  #   if (!FightVal$Confirmation)
  #     FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")
  # } else {
  #   FightVal$ConfirmRoll <- NA
  # }
  # FightVal$EffectOfFumble <- NA
})


# Dodge Roll
observeEvent(input$doDodge, {
  doCombatRollBase("Dodge")
  # FightVal$Action  <- "Dodge"
  # FightVal$Roll    <- CombatRoll()
  # FightVal$Success <- VerifyCombatRoll(FightVal$Roll, 
  #                                      input$DodgeValue, 
  #                                      as.numeric(input$CombatPenalty))
  # FightVal$Damage  <- NA
  # # Level of success
  # if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
  #   FightVal$ConfirmRoll <- CombatRoll()
  #   Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, 
  #                                    input$DodgeValue, 
  #                                    as.numeric(input$CombatPenalty))
  #   FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
  #   if (!FightVal$Confirmation)
  #     FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")
  # } else {
  #   FightVal$ConfirmRoll <- NA
  # }
  # FightVal$EffectOfFumble <- NA
})


# OUTPUT -------------------------------
# Roll
output$CombatAction <- renderPrint({ 
  if (!is.na(FightVal$Roll)) {
    Result <- paste0(i18n$t(FightVal$Action), ": ", 
                     FightVal$Roll, " - ", 
                     i18n$t(FightVal$Success))
    cat(Result)
  }
})



# Confirmation Panel
observeEvent(input$doCombatConfirm, {
  Penalty <- as.numeric(input$CombatPenalty)
  if (FightVal$Action == "Attack") {
    Check <- input$ATValue
  } else if (FightVal$Action == "Parry") {
    Check <- input$PAValue
  } else if (FightVal$Action == "Dodge") {
    Check <- input$DodgeValue
  }
  
  FightVal$ConfirmRoll <- CombatRoll()
  Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, Check, Penalty)
  FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
  if (!FightVal$Confirmation)
    FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")

  # Damage
  if(FightVal$Action == "Attack") {
    if (FightVal$Success == "Critical")
      FightVal$Damage <- 2 * DamageRoll(input$DamageDieCount, input$Damage)
    else if  (FightVal$Success == "Success")
      FightVal$Damage <- DamageRoll(input$DamageDieCount, input$Damage)
  } else {
    FightVal$Damage <- NA
  }
  updateActionButton(session, "doCombatConfirm", )
})

output$ShowCombatConfirm <- reactive({    
  ConfirmLabel <- ifelse(FightVal$Success =="Critical", i18n$t("Confirm!"), i18n$t("Avert!")) #######TODO####
  updateActionButton(session, "doCombatConfirm", label = ConfirmLabel)
  return(!is.na(FightVal$ConfirmRoll))
})
outputOptions(output, 'ShowCombatConfirm', suspendWhenHidden = FALSE)


output$CombatConfirm <- renderPrint({
  if (FightVal$ConfirmRoll != "Required") {
    if (FightVal$Success == "Fumble") {
      Result <- "Still a Fumble"
    } else if (FightVal$Success == "Critical") {
      Result <- "Critical confirmed"
    } else if (FightVal$Success == "Success") {
      Result <- "Critical was lost"
    } else if (FightVal$Success == "Fail") {
      Result <- "Fumble avoided"
    }
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
  return( FightVal$Success == "Fumble" & FightVal$Confirmation )
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
