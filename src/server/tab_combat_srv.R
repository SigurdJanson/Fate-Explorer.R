# COMBAT TAB

# Values of last combat roll
CombatRoll <- reactiveValues(Action = "", Roll = NA)
CombatConfirmRoll <- reactiveValues(ConfirmRoll = NA, Confirmation = FALSE)
CombatRollEffect <- reactiveValues(Success = "", Damage = NA)
CombatFumble <- reactiveValues(EffectOfFumble = NA)

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
    if(nchar(Weapon) > 0) {
      updateNumericInput(session, "AttackValue", value = Character$Weapons["AT", Weapon])
      updateNumericInput(session, "ParryValue", value = Character$Weapons["PA", Weapon])
      updateNumericInput(session, "DamageDieCount", value = Character$Weapons["DamageDice", Weapon])
      updateNumericInput(session, "Damage", value = Character$Weapons["DamageMod", Weapon])
    }
  } else {
    updateNumericInput(session, "AttackValue", value = 6L)
    updateNumericInput(session, "ParryValue", value = 3L)
    updateNumericInput(session, "DamageDieCount", value = 1L)
    updateNumericInput(session, "Damage", value = 0L)
  }
}, ignoreNULL = FALSE)


  
# ACTIONS -------------------------------
doCombatRollBase <- function(Action) {
  ShowConfirmation <- FALSE
  CombatRoll$Action  <- Action
  CombatRoll$Roll    <- CombatRoll()
  CombatConfirmRoll$Confirmation <- FALSE
  CombatFumble$EffectOfFumble <- NA
}

observeEvent(input$doAttackThrow, { # Attack Roll
  doCombatRollBase("Attack")
})

observeEvent(input$doParryThrow, { # Parry Roll
  doCombatRollBase("Parry")
})

observeEvent(input$doDodge, { # Dodge Roll
  doCombatRollBase("Dodge")
})


# OUTPUT -------------------------------
# Roll
output$CombatAction <- renderPrint({
  req(CombatRoll$Action)
  # Compute
  Penalty <- as.numeric(input$CombatPenalty)
  SkillValue <- input[[ paste0(CombatRoll$Action, "Value") ]]
  CombatRollEffect$Success <- VerifyCombatRoll(CombatRoll$Roll, SkillValue, Penalty)
  
  isolate({
    if (CombatRollEffect$Success == "Critical" || CombatRollEffect$Success == "Fumble") {
      CombatConfirmRoll$ConfirmRoll <- "Required"
    } else {
      CombatConfirmRoll$ConfirmRoll <- NA
    }
    CombatRollEffect$Damage <- NA
    if(CombatRoll$Action == "Attack") {
      if  (CombatRollEffect$Success == "Success")
        CombatRollEffect$Damage <- DamageRoll(input$DamageDieCount, input$Damage)
    }
  })
  # Print
  if (!is.na(CombatRoll$Roll)) {
    Result <- paste0(i18n$t(CombatRoll$Action), ": ", 
                     CombatRoll$Roll, " - ", 
                     i18n$t(CombatRollEffect$Success))
    cat(Result)
  }
})



# Confirmation Panel: Confirm Critical/Botch
observeEvent(input$doCombatConfirm, {
  Check <- input[[ paste0(CombatRoll$Action, "Value") ]]
  Penalty <- as.numeric(input$CombatPenalty)
  CombatConfirmRoll$ConfirmRoll <- CombatRoll()
  Confirmation <- VerifyCombatRoll(CombatConfirmRoll$ConfirmRoll, Check, Penalty)
  
  #FightVal$Confirmation <- VerifyConfirmation(FightVal$Success, Confirmation)
  if (CombatRollEffect$Success == "Critical")
    CombatConfirmRoll$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
  else
    CombatConfirmRoll$Confirmation <- Confirmation == "Fail" | Confirmation == "Fumble"
  if (!CombatConfirmRoll$Confirmation)
    CombatRollEffect$Success <- ifelse(CombatRollEffect$Success == "Critical", "Success", "Fail")

  # Damage
  if(CombatRoll$Action == "Attack") {
    if (CombatRollEffect$Success == "Critical")
      CombatRollEffect$Damage <- 2 * DamageRoll(input$DamageDieCount, input$Damage)
    else if  (CombatRollEffect$Success == "Success")
      CombatRollEffect$Damage <- DamageRoll(input$DamageDieCount, input$Damage)
  } else {
    CombatRollEffect$Damage <- NA
  }
  updateActionButton(session, "doCombatConfirm", label = "")
})


output$ShowCombatConfirm <- reactive({
  if (CombatRollEffect$Success == "Critical")
    ConfirmLabel <- i18n$t("Confirm!")
  else if (CombatRollEffect$Success == "Fumble")
    ConfirmLabel <- i18n$t("Avert!")
  else ConfirmLabel <- ""
  updateActionButton(session, "doCombatConfirm", label = ConfirmLabel)
  
  return(!is.na(CombatConfirmRoll$ConfirmRoll))
})
outputOptions(output, 'ShowCombatConfirm', suspendWhenHidden = FALSE)


output$CombatConfirm <- renderPrint({
  if (!is.na(CombatConfirmRoll$ConfirmRoll) && CombatConfirmRoll$ConfirmRoll != "Required") {
    Result <- switch(CombatRollEffect$Success,
                     Fumble   = "Still a Fumble",
                     Critical = "Critical confirmed",
                     Success  = "Critical was lost",
                     Fail     = "Fumble avoided",
                     "")
  } else Result <- ""
  
  if(Result != "")
    return(cat(i18n$t(Result), " (", CombatConfirmRoll$ConfirmRoll, ")", sep = ""))
  else 
    return(cat(""))
})


# Damage Panel ----
output$ShowCombatDamage <- reactive({
  return( !is.na(CombatRollEffect$Damage) )
})
outputOptions(output, 'ShowCombatDamage', suspendWhenHidden = FALSE)

output$CombatDamage <- renderPrint({
  cat(i18n$t("Hit points"), ": ", CombatRollEffect$Damage, sep = "")
})


# Fumble Panel ----
output$ShowCombatFumble <- reactive({
  return( CombatRollEffect$Success == "Fumble" & CombatConfirmRoll$Confirmation )
})
outputOptions(output, 'ShowCombatFumble', suspendWhenHidden = FALSE)

output$CombatFumble <- renderPrint({
  if(!is.na(CombatFumble$EffectOfFumble)) {
    Result <- GetCombatFumbleEffect(CombatFumble$EffectOfFumble)
  } else Result <- ""
  
  cat(i18n$t(Result))
})

observeEvent(input$doCombatFumble, {
  CombatFumble$EffectOfFumble <- CombatFumbleRoll()
})


#
# Weapon details Panel ------
# (harvest Ulisses Wiki)
output$ShowWeaponDetails <- reactive({
  return( !is.null(Character$Weapons) && input$PredefinedWeapon )
})
outputOptions(output, 'ShowWeaponDetails', suspendWhenHidden = FALSE)

output$WeaponDetails <- renderText({
  Result <- ""
  if (input$PredefinedWeapon) {
    Weapon <- as.character(input$CombatSelectWeapon)
    
    WeaponData <- GetWeapons(Weapon)
    URL <- WeaponData[["url"]]

    # harvest HTML page
    if( length(URL) > 0 && nchar(URL) > 0) {
      URL <- paste0("https://ulisses-regelwiki.de/", URL)
      Sssn <- try(html_session(URL))
      if (httr::status_code(Sssn) == 200) {
        Nodes <- html_nodes(Sssn, "div.ce_text.last.block")
        Nodes <- html_children(Nodes)
        Result <- as.character(Nodes)
      } else {
        Result  <- paste0("<p>", i18n$t("Not available"), "</p>")
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


# Exploration Panel ----
output$ShowExploreFightingChances <- reactive({
  return( input$chbExploreChances )
})
outputOptions(output, 'ShowExploreFightingChances', suspendWhenHidden = FALSE)
