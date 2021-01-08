# COMBAT TAB

#' Combat tab supports combat and initiative rolls
#' But it shall only show either of those two results. That is why
#' any roll must invalidate the other (i.e. set it to `NULL`)


# Values of last combat roll
ActiveWeapon <- MeleeWeapon$new(Skill  = list(Attack = 10L, Parry = 10L, Dodge = 10L), 
                                Damage = list(N = 1L, DP = 6L, Bonus = 0L))
# necessary trigger to recognize a new roll - value is unimportant
UpdateCombatResult <- reactiveVal()
InitiativeRollResult <- reactiveVal(NULL)



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
          ActiveWeapon <<- w; break
        }
      }
    }
  } else {
    # Default
    ActiveWeapon <<- MeleeWeapon$new(Skill = list(Attack = 9L, Parry = 5L, 
                                                  Dodge = Character$Weapon[[1L]]$Skill$Dodge), 
                                     Damage = list(N = 1L, DP = 6L, Bonus = 0L))
  }
  # Update ui controls
  for(Action in names(.CombatAction)) {
    updateNumericInput(session, paste0("inp", Action, "Value"), 
                       value = ActiveWeapon$Skill[[Action]])
  }
  updateNumericInputIcon(session, "inpDamageDieCount", 
                         value = ActiveWeapon$Damage$N, 
                         icon = list(NULL, paste0("W", ActiveWeapon$Damage$DP)))
  updateNumericInput(session, "inpDamage", value = ActiveWeapon$Damage$Bonus)
  
  if(ActiveWeapon$CanParry()) {
    shinyjs::enable("doParryRoll")
    shinyjs::enable("inpParryValue")
  }
  else {
    shinyjs::disable("doParryRoll")
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
  InitiativeRollResult(NULL)
  RollInProgress(paste0("do", Action, "Roll"), TRUE)
  ActiveWeapon$Roll( Action, input$inpCombatMod )
  UpdateCombatResult(format(Sys.time(), "%s %OS4"))
}

observeEvent(input$doAttackRoll, { # Attack Roll
  doCombatRollBase("Attack")
})

observeEvent(input$doParryRoll, { # Parry Roll
  doCombatRollBase("Parry")
})

observeEvent(input$doDodgeRoll, { # Dodge Roll
  doCombatRollBase("Dodge")
})

observeEvent(input$doInitiativeRoll, { # Initiative Roll
  UpdateCombatResult(NULL)
  
  InitiativeRollResult(NULL) # make sure the roll is recognized

  if (isTruthy(Character$Attr)) { #-TODO: this test should not be necessary in the long run
    RollInProgress("doInitiativeRoll", TRUE)
    Roll <- InitiativeRoll(Ability = Character$Attr, Mod = input$inpCombatMod)
    InitiativeRollResult(Roll)
  }
})

observeEvent(input$doCombatConfirm, { # Confirm Critical/Botch
  UpdateCombatResult( ActiveWeapon$Confirm()+100 )
})

observeEvent(input$doCombatFumble, { # Show fumble result
  UpdateCombatResult( ActiveWeapon$FumbleRoll() )
})



# UI COMBAT ROLL -----------------------
output$uiCombatRoll <- renderText({
  req(UpdateCombatResult(), TRUE)

  if (isTruthy(ActiveWeapon$ConfirmRoll))
    ConfirmationStr <- RenderRollConfirmation(names(ActiveWeapon$LastResult),
                                              ActiveWeapon$ConfirmRoll, i18n) 
  else 
    ConfirmationStr <- NULL

  # Render the result
  if (ActiveWeapon$LastAction == .CombatAction["Attack"])
    KeyResult <- RenderRollKeyResult(names(ActiveWeapon$LastResult), 
                                     ActiveWeapon$LastDamage, 
                                     ActiveWeapon$LastRoll, KeyUnit = "hp")
  else
    KeyResult <- RenderRollKeyResult(names(ActiveWeapon$LastResult), 
                                     ActiveWeapon$LastRoll, 
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
  
  # Finally restore the buttons
  Sys.sleep(0.3)
  shinyjs::delay(700, RollInProgress(paste0("do", names(.CombatAction)[ActiveWeapon$LastAction], "Roll"), FALSE))

  # Return the results
  if (length(Result) > 0) # two panels
    return(as.character( div(KeyResult, 
                             div(Result, class = "shiny-html-output shiny-bound-output"), 
                             class = "roll") ))
  else # key result panel, only
    return(as.character(KeyResult))
})



# VIEW: Initiative Roll ---------- ----------
output$uiInitiativeRoll <- renderText({
  req(InitiativeRollResult(), TRUE)

  # Finally restore the buttons
  Sys.sleep(0.3) # artificially delay the result for increased tension
  shinyjs::delay(700, RollInProgress("doInitiativeRoll", FALSE))
  
  # Update button and result display
  Roll <- InitiativeRollResult()
  updateActionButton(session, inputId = "doInitiativeRoll", 
                     label = RollButtonLabel("doInitiativeRoll", i18n$t("Initiative"), Roll))
  
  Result <- RenderRollKeyResult("Initiative", Roll, KeyUnit = "dr")
  return(as.character(Result))
})



# VIEW: Weapon details Panel ---------- ----------
# (harvest Ulisses Wiki)
output$ShowWeaponDetails <- reactive({
  return( input$chbHarvestWeaponDetails && 
            !is.null(Character$Weapons) && 
            input$chbPredefinedWeapon )
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
    Title <- paste0("<p><strong>", i18n$t("Weapon Details"), "</strong></p>")
    Result <- paste0(Result, collapse="") # Collapse several strings into one
    Result <- paste0("<div style='color:gray;padding:0.5em;border: 1px solid lightgray'>", 
                     Title, Result, 
                     "</div")
  }
  
  return(Result)
})



# VIEW: Exploration Panel ---------- ----------
output$ShowExploreFightingChances <- reactive({
  return( input$chbExploreChances )
})
outputOptions(output, 'ShowExploreFightingChances', suspendWhenHidden = FALSE)
