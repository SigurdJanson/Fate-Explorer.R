# COMBAT TAB

# Values of last combat roll
ActiveWeapon <- MeleeWeapon$new(Skill  = list(Attack = 9L, Parry = 5L, Dodge = 5L), 
                                Damage = list(N = 1L, DP = 6L, Bonus = 0L))
UpdateCombatResult <- reactiveVal() # necessary trigger to recognize a new roll - the value is unimportant



# VALUES -------------------------------
# Source of Weapons Panel
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
      ActiveWeapon <- MeleeWeapon$new(Weapon, Character$Abilities, Character$CombatSkills)
    }
  } else {
    ActiveWeapon <- MeleeWeapon$new(Skill  = list(Attack = 9L, Parry = 5L, Dodge = 5L), 
                                    Damage = list(N = 1L, DP = 6L, Bonus = 0L))
  }
  # Update ui controls
  for(Action in names(.CombatActions)) {
    updateNumericInput(session, paste0(Action, "Value"), value = ActiveWeapon$Skill[[Action]])
  }
  updateNumericInput(session, "DamageDieCount", value = ActiveWeapon$Damage$N)
  updateNumericInput(session, "Damage", value = ActiveWeapon$Damage$Bonus)
  
}, ignoreNULL = FALSE)


  
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
  Result <- RenderRollKeyResult(names(ActiveWeapon$LastResult), ActiveWeapon$LastRoll)
  
  # Confirmation
  # Waiting for confirmation
  if (isTRUE(ActiveWeapon$ConfirmationMissing))
    Result <- div(Result, div( actionLink("doCombatConfirm", ConfirmLabel, icon = NULL) ),
                  class = "shiny-html-output shiny-bound-output roll")
  # Show confirmation result (add confirmation <div/>)
  if (!is.null(ConfirmationStr)) {
    Result <- div(Result, div( ConfirmationStr ), class = "shiny-html-output shiny-bound-output roll")

    # Fumble effects
    # Waiting for fumble roll
    if (ActiveWeapon$LastResult == .SuccessLevel["Fumble"]) {
      if (!isTruthy(ActiveWeapon$LastFumbleEffect)) {
        Result <- div(Result, actionLink("doCombatFumble", i18n$t("See what happens..."), icon = icon("shield-alt")),
                      class = "shiny-html-output shiny-bound-output roll")
      } else { # Show fumble effects
        Result <- div(Result, div(i18n$t(ActiveWeapon$LastFumbleEffect)),
                      class = "shiny-html-output shiny-bound-output roll")
      }
    }
  }
  
  
  
  return(paste((Result), collapse=""))
})


# OUTPUT -------------------------------

# Confirmation Panel: Confirm Critical/Botch
observeEvent(input$doCombatConfirm, {
  UpdateCombatResult( ActiveWeapon$Confirm() )
})


observeEvent(input$doCombatFumble, {
  UpdateCombatResult( ActiveWeapon$FumbleRoll() )
})


#
# Weapon details Panel ----------
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
