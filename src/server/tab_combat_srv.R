# COMBAT TAB

# Values of last combat roll
FightVal <- reactiveValues(Action = "", Roll = NA, 
                           Success = "", Damage = NA,
                           ConfirmRoll = NA, Confirmation = FALSE)
# Indicates if confirmation roll of last critical/botch is being displayed
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
  if(FightVal$Action == "Attack") {
    if  (FightVal$Success == "Success")
      FightVal$Damage <- DamageRoll(input$DamageDieCount, input$Damage)
  }
}

observeEvent(input$doAttackThrow, {
  doCombatRollBase("Attack")
})


# Parry Roll
observeEvent(input$doParryThrow, {
  doCombatRollBase("Parry")
})


# Dodge Roll
observeEvent(input$doDodge, {
  doCombatRollBase("Dodge")
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
  if (FightVal$Success == "Critical")
    FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
  else 
    FightVal$Confirmation <- Confirmation == "Fail" | Confirmation == "Fumble"
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
  updateActionButton(session, "doCombatConfirm", label = "")
})


output$ShowCombatConfirm <- reactive({
  if (FightVal$Success =="Critical")
    ConfirmLabel <- i18n$t("Confirm!")
  else if (FightVal$Success =="Fumble")
    ConfirmLabel <- i18n$t("Avert!")
  else ConfirmLabel <- ""
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
  
  if(Result != "")
    return(cat(i18n$t(Result), " (", FightVal$ConfirmRoll, ")", sep = ""))
  else 
    return(cat(""))
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


#
# Weapon details Panel
# (harvest Ulisses Wiki)
output$ShowWeaponDetails <- reactive({
  return( !is.null(Character$Weapons) && input$PredefinedWeapon )
})
outputOptions(output, 'ShowWeaponDetails', suspendWhenHidden = FALSE)

output$WeaponDetails <- renderText({
  Result <- ""
  if (input$PredefinedWeapon) {
    # Make name URL comform
    Weapon <- tolower(as.character(input$CombatSelectWeapon))
    Weapon <- gsub("[ ,]+", "-", Weapon)
    Weapon <- replace_umlauts(Weapon)
    
    # harvest HTML page
    if( nchar(Weapon) > 0) {
      URL <- paste0("https://ulisses-regelwiki.de/index.php/", 
                  Weapon, ".html")
      Sssn <- try(html_session(URL))
      if (httr::status_code(Sssn) == 200) {
        Nodes <- html_nodes(Sssn, "div.ce_text.last.block")
        Nodes <- html_children(Nodes)
        Result <- as.character(Nodes)
      } else {
        Result  <- paste0("<p>", i18n$t("Not available"), "</p>") ##TODO: TRANSLATE
      }
    }
    # Render result to HTML
    Title <- "<p><strong>Weapon Details</strong></p>"
    Result <- paste0(Result, collapse="") # Collapse several strings into one
    Result <- paste0("<div style='color:gray;padding:0.5em;border: 1px solid lightgray'>", 
                     Title, Result, 
                     "</div")
  }
  
  return(Result)
})

