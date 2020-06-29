# COMBAT TAB

# Values of last combat roll
ActiveWeapon <- MeleeWeapon$new(Skill  = list(Attack = 10L, Parry = 10L, Dodge = 10L), 
                                Damage = list(N = 1L, DP = 6L, Bonus = 0L))
UpdateCombatResult <- reactiveVal() # necessary trigger to recognize a new roll - value is unimportant



# VALUES -------------------------------
# Source of Weapons Panel
output$ShowSetupWeapons <- reactive({
  return( !is.null(Character$Weapons) )
})
outputOptions(output, 'ShowSetupWeapons', suspendWhenHidden = FALSE)


output$ShowPredefinedWeapons <- reactive({
  return( !is.null(Character$Weapons) && input$chbPredefinedWeapon )
})
outputOptions(output, 'ShowPredefinedWeapons', suspendWhenHidden = FALSE)

observeEvent(input$chbPredefinedWeapon,{
  updateSelectizeInput(session, "cmbCombatSelectWeapon", selected = NA)
})

observeEvent(input$cmbCombatSelectWeapon, {
  # Select weapon
  if (input$chbPredefinedWeapon) {
    Weapon <- as.character(input$cmbCombatSelectWeapon)
    if(nchar(Weapon) > 0) {
      # Get ID and determine if weapon is for close or ranged combat
      Weapon <- Character$Weapons["templateID", Weapon] # ID
      if ( IsRangedWeapon(Weapon) )
        ActiveWeapon <- RangedWeapon$new(Weapon, Character$Attr, Character$CombatSkills)
      else
        ActiveWeapon <- MeleeWeapon$new(Weapon, Character$Attr, Character$CombatSkills)
    }
  } else {
    ActiveWeapon <- MeleeWeapon$new(Skill  = list(Attack = 9L, Parry = 5L, Dodge = 5L), 
                                    Damage = list(N = 1L, DP = 6L, Bonus = 0L))
  }
  # Update ui controls
  for(Action in names(.CombatActions)) {
    updateNumericInput(session, paste0("inp", Action, "Value"), value = ActiveWeapon$Skill[[Action]])
  }
  updateNumericInput(session, "inpDamageDieCount", value = ActiveWeapon$Damage$N)
  updateNumericInput(session, "inpDamage", value = ActiveWeapon$Damage$Bonus)
  
}, ignoreNULL = FALSE)


# Weapon skill slider: react to changes
observeEvent(input$inpAttackValue, {
  if (isTruthy(input$inpAttackValue))
    ActiveWeapon$Skill[["Attack"]] <- input$inpAttackValue
})
observeEvent(input$inpParryValue, {
  if (isTruthy(input$inpParryValue))
    ActiveWeapon$Skill[["Parry"]] <- input$inpParryValue
})
observeEvent(input$inpDodgeValue, {
  if (isTruthy(input$inpDodgeValue))
    ActiveWeapon$Skill[["Dodge"]] <- input$inpDodgeValue
})
observeEvent(input$inpDamageDieCount, {
  if (isTruthy(input$inpDamageDieCount))
    ActiveWeapon$Damage[["N"]] <- input$inpDamageDieCount
})
observeEvent(input$inpDamage, {
  if (isTruthy(input$inpDamage))
    ActiveWeapon$Damage[["Bonus"]] <- input$inpDamage
})

  
# ACTIONS -------------------------------
doCombatRollBase <- function(Action) {
  UpdateCombatResult( ActiveWeapon$Roll( Action, as.numeric(input$CombatPenalty) ) )
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

observeEvent(input$doCombatConfirm, { # Confirm Critical/Botch
  UpdateCombatResult( ActiveWeapon$Confirm() )
})

observeEvent(input$doCombatFumble, { # Show fumble result
  UpdateCombatResult( ActiveWeapon$FumbleRoll() )
})



# UI COMBAT ROLL -----------------------
output$uiCombatRoll <- renderText({
  req(UpdateCombatResult())

  if (isTruthy(ActiveWeapon$ConfirmRoll))
    ConfirmationStr <- RenderRollConfirmation(names(ActiveWeapon$LastResult),
                                              ActiveWeapon$ConfirmRoll, i18n) 
  else 
    ConfirmationStr <- NULL
  
  if (ActiveWeapon$LastResult == .SuccessLevel["Critical"])
    ConfirmLabel <- i18n$t("Confirm!")
  else if (ActiveWeapon$LastResult == .SuccessLevel["Fumble"])
    ConfirmLabel <- i18n$t("Avert!")

  # Render the result
  KeyResult <- RenderRollKeyResult(names(ActiveWeapon$LastResult), ActiveWeapon$LastRoll)
  Result <- tagList()

  # Confirmation
  # Waiting for confirmation
  if (isTRUE(ActiveWeapon$ConfirmationMissing))
    Result <- tagAppendChild(Result, actionLink("doCombatConfirm", ConfirmLabel, icon = NULL))
  # Show confirmation result (add confirmation <div/>)
  if (!is.null(ConfirmationStr)) {
    Result <- tagAppendChild(Result, p(ConfirmationStr))

    # Fumble effects
    # Waiting for fumble roll
    if (ActiveWeapon$LastResult == .SuccessLevel["Fumble"]) {
      if (!isTruthy(ActiveWeapon$LastFumbleEffect)) {
        Result <- tagAppendChild(Result, p(actionLink("doCombatFumble", i18n$t("See what happens..."))))
        
      } else { # Show fumble effects
        Result <- tagAppendChild(Result, p(i18n$t(ActiveWeapon$LastFumbleEffect)))
      }
    }
  }
  if (length(Result) > 0) # two panels
    return(as.character( div(KeyResult, 
                             div(Result, class = "shiny-html-output shiny-bound-output"), 
                             class = "roll")))
  else # key result panel, only
    return(as.character(KeyResult))
})



# VIEW: Weapon details Panel ----------
# (harvest Ulisses Wiki)
output$ShowWeaponDetails <- reactive({
  return( !is.null(Character$Weapons) && input$chbPredefinedWeapon )
})
outputOptions(output, 'ShowWeaponDetails', suspendWhenHidden = FALSE)


output$WeaponDetails <- renderText({
  Result <- ""
  if (input$chbPredefinedWeapon) {
    Weapon <- as.character(input$cmbCombatSelectWeapon)
    
    WeaponData <- GetWeapons(Weapon)
    URL <- WeaponData[["url"]]

    # harvest HTML page
    if( length(URL) > 0 && nchar(URL) > 0) {
      URL <- paste0("http://ulisses-regelwiki.de/", URL)
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


# View: Exploration Panel ----
output$ShowExploreFightingChances <- reactive({
  return( input$chbExploreChances )
})
outputOptions(output, 'ShowExploreFightingChances', suspendWhenHidden = FALSE)
