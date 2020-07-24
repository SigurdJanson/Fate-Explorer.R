# COMBAT TAB

# Values of last combat roll
ActiveWeapon <- MeleeWeapon$new(Skill  = list(Attack = 10L, Parry = 10L, Dodge = 10L), 
                                Damage = list(N = 1L, DP = 6L, Bonus = 0L))
# necessary trigger to recognize a new roll - value is unimportant
UpdateCombatResult <- reactiveVal() 



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
      # Get weapon from character
      for (w in Character$Weapons) {
        if (w$Name == Weapon) {
          ActiveWeapon <- w; break
        }
      }
    }
  } else {
    ActiveWeapon <- MeleeWeapon$new(Skill  = list(Attack = 9L, Parry = 5L, 
                                                  Dodge = Character$Weapon[[1]]$Skill$Dodge), 
                                    Damage = list(N = 1L, DP = 6L, Bonus = 0L))
  }
  # Update ui controls
  for(Action in names(.CombatAction)) {
    updateNumericInput(session, paste0("inp", Action, "Value"), value = ActiveWeapon$Skill[[Action]])
  }
  updateNumericInput(session, "inpDamageDieCount", value = ActiveWeapon$Damage$N)
  updateNumericInput(session, "inpDamage", value = ActiveWeapon$Damage$Bonus)
  if(ActiveWeapon$CanParry()) {
    shinyjs::enable("doParryThrow")
    shinyjs::enable("inpParryValue")
  }
  else {
    shinyjs::disable("doParryThrow")
    shinyjs::disable("inpParryValue")
  }
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
  UpdateCombatResult( ActiveWeapon$Roll( Action, input$inpCombatMod ) )
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
  UpdateCombatResult( ActiveWeapon$Confirm()+100 )
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

  # Render the result
  if (ActiveWeapon$LastAction == .CombatAction["Attack"])
    KeyResult <- RenderRollKeyResult(names(ActiveWeapon$LastResult), ActiveWeapon$LastDamage, 
                                     ActiveWeapon$LastRoll, KeyUnit = "hp")
  else
    KeyResult <- RenderRollKeyResult(names(ActiveWeapon$LastResult), ActiveWeapon$LastRoll, 
                                     KeyUnit = "dr")
  
  Result <- tagList()

  # Confirmation
  # Waiting for confirmation
  if (isTRUE(ActiveWeapon$ConfirmationMissing))
    Result <- tagAppendChild(Result, RenderConfirmationRequest("doCombatConfirm", ActiveWeapon$LastResult))
  # Show confirmation result (add confirmation <div/>)
  if (!is.null(ConfirmationStr)) {
    Result <- tagAppendChild(Result, p(ConfirmationStr))

    # Fumble effects
    # Waiting for fumble roll
    if (ActiveWeapon$LastResult == .SuccessLevel["Fumble"]) {
      if (!isTruthy(ActiveWeapon$LastFumbleEffect)) {
        Result <- tagAppendChild( Result, RenderFumbleRollRequest("doCombatFumble") )
      } else { # Show fumble effects
        Result <- tagAppendChild( Result, RenderFumbleRollEffect(ActiveWeapon[["LastFumbleEffect"]]) )
      }
    }
  }
  if (length(Result) > 0) # two panels
    return(as.character( div(KeyResult, 
                             div(Result, class = "shiny-html-output shiny-bound-output"), 
                             class = "roll") ))
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
    
    WeaponData <- GetWeapons(Weapon, "Any")
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


# View: Exploration Panel ----
output$ShowExploreFightingChances <- reactive({
  return( input$chbExploreChances )
})
outputOptions(output, 'ShowExploreFightingChances', suspendWhenHidden = FALSE)