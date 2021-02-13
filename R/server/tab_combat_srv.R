# COMBAT TAB

#' Combat tab supports combat and initiative rolls
#' But it shall only show either of those two results. That is why
#' any roll must invalidate the other (i.e. set it to `NULL`)


# Values of last combat roll
UpdateCombatModsModulePayload <- function(Weapon) {
  if (!missing(Weapon)) {
#-browser()
    CombatModsModulePayload$Name  <<- Weapon$Name
    CombatModsModulePayload$Type  <<- Weapon$Type
    CombatModsModulePayload$Range <<- Weapon$Range
    CombatModsModulePayload$Skill <<- Weapon$Skill
  }
}

ActiveWeapon <- MeleeWeapon$new(Skill  = c(Attack = 10L, Parry = 10L, Dodge = 10L),
                                Damage = c(N = 1L, DP = 6L, Bonus = 0L))
ActiveWeapon$Type <- .WeaponType["Melee"]
ActiveWeapon$RegisterOnValueChange(UpdateCombatModsModulePayload)

# necessary trigger to recognize a new roll - value is unimportant
UpdateCombatResult <- reactiveVal()
InitiativeRollResult <- reactiveVal(NULL)



# Get effective combat values
UpdateEffectiveCombatValues <- function(Actions) {
  req(CombatModifier())
  for (a in Actions) {
    EffectiveValue <- unname(ActiveWeapon$Skill[a] + CombatModifier()[a] + input$inpCombatMod)
    EffectiveValue <- max(EffectiveValue, 0L)
    # Find the right icon
    Comparison <- input[[paste0("inp", a, "Value")]] - EffectiveValue
    Comparison <- sign(Comparison) +2L # map (-1:1) to  (1:3)
    if (isTruthy(Comparison))
      Icon <- switch(letters[Comparison], a = gicon("plus-circle"),
                     b = list("-"), c = gicon("minus-circle"))
    else
      Icon <- list("-")

    isolate(
      updateNumericInputIcon(session, paste0("inp", a, "Mod"),
                             value = EffectiveValue, icon = Icon)
    )
  }
}

observeEvent(CombatModifier(), {UpdateEffectiveCombatValues(names(.CombatAction))})




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
          ActiveWeapon <<- w;
          UpdateCombatModsModulePayload(ActiveWeapon)
          break
        }
      }
    }
  } else {
    # Default
    # - Check first if character is loaded
    Dodge <- ifelse(isTruthy(Character$Weapons[[1L]]$Skill["Dodge"]), Character$Weapons[[1L]]$Skill["Dodge"], 6L)
    ActiveWeapon <<- MeleeWeapon$new(
      Skill = c(Attack = 9L, Parry = 5L, Dodge = Dodge),
      Damage = c(N = 1L, DP = 6L, Bonus = 0L))
    ActiveWeapon$Type <- .WeaponType["Melee"]
    ActiveWeapon$RegisterOnValueChange(UpdateCombatModsModulePayload)
  }
  # Update ui controls
  for(Action in names(.CombatAction)) {
    updateNumericInput(session, paste0("inp", Action, "Value"),
                       value = unname(ActiveWeapon$Skill[Action]))
  }
  updateNumericInputIcon(session, "inpDamageDieCount",
                         value = unname(ActiveWeapon$Damage["N"]),
                         icon = list(NULL, paste0("W", ActiveWeapon$Damage["DP"])))
  updateNumericInput(session, "inpDamage", value = unname(ActiveWeapon$Damage["Bonus"]))

  # Update module payload
  UpdateCombatModsModulePayload()

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
  if (isTruthy(input$inpAttackValue)) {
    ActiveWeapon$Skill["Attack"] <- input$inpAttackValue
    UpdateEffectiveCombatValues("Attack")
    UpdateCombatModsModulePayload()
  }
})
observeEvent(input$inpParryValue, {
  if (isTruthy(input$inpParryValue)) {
    ActiveWeapon$Skill["Parry"] <- input$inpParryValue
    UpdateEffectiveCombatValues("Parry")
    UpdateCombatModsModulePayload()
  }
})
observeEvent(input$inpDodgeValue, {
  if (isTruthy(input$inpDodgeValue)) {
    ActiveWeapon$Skill["Dodge"] <- input$inpDodgeValue
    UpdateEffectiveCombatValues("Dodge")
    UpdateCombatModsModulePayload()
  }
})
observeEvent(input$inpDamageDieCount, {
  if (isTruthy(input$inpDamageDieCount))
    ActiveWeapon$Damage["N"] <- input$inpDamageDieCount
})
observeEvent(input$inpDamage, {
  if (isTruthy(input$inpDamage))
    ActiveWeapon$Damage["Bonus"] <- input$inpDamage
})
observeEvent(input$inpCombatMod, {
  if (isTruthy(input$inpCombatMod))
    UpdateEffectiveCombatValues(names(.CombatAction))
})

# ACTIONS -------------------------------
doCombatRollBase <- function(Action) {
  InitiativeRollResult(NULL)
  RollInProgress(paste0("do", Action, "Roll"), TRUE)
  ActiveWeapon$Roll( Action, input$inpCombatMod +  CombatModifier()[Action] )
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

  RollInProgress("doInitiativeRoll", TRUE)
  Roll <- InitiativeRoll(Ability = Character$Attr, Mod = input$inpCombatMod)
  InitiativeRollResult(Roll)
})

observeEvent(input$doCombatConfirm, { # Confirm Critical/Botch
  UpdateCombatResult( ActiveWeapon$Confirm()+100 )
})

observeEvent(input$doCombatFumble, { # Show fumble result
  UpdateCombatResult( ActiveWeapon$FumbleRoll() )
})


# UI: COMBAT ROLL BUTTONS ---------------
output$uiCombatRollButtons <- renderUI({
  if (isTruthy(Character$Attr)) {
    ColumnInitiative <- column(3L, style="padding-right:1%",
                               actionButton("doInitiativeRoll",
                                            span(i18n$t("Initiative"), id="lbldoInitiativeRoll"),
                                            icon = gicon("initiative"),
                                            width = "98%", style = "font-size: 140%"))
    AvailableWidth <- 12L / 4L * 3L
  } else {
    ColumnInitiative <- NULL
    AvailableWidth <- 12L
  }

  ColumnAttack <- column(AvailableWidth %/% 3,
                         actionButton("doAttackRoll",
                                      span(i18n$t("Attack"), id="lbldoAttackRoll"),
                                      icon = gicon("battle-axe"),
                                      width = "98%",
                                      style = "font-size: 140%"))
  ColumnDefense <- column(2 * AvailableWidth %/% 3, style="padding-right:1%",
                          actionButton("doParryRoll",
                                       span(i18n$t("Parry"), id="lbldoParryRoll"), icon = gicon("shield"),
                                       width = "49%", style = "font-size: 140%"),#
                          actionButton("doDodgeRoll",
                                       span(i18n$t("Dodge"), id="lbldoDodgeRoll"), icon = gicon("dodge"),
                                       width = "49%", style = "font-size: 140%"),)

  Output <- fluidPage(fluidRow(
    ColumnAttack, ColumnDefense, ColumnInitiative
  ))

  return(Output)
})



# OUTPUT: COMBAT ROLL -----------------------
output$uiCombatRoll <- renderText({
  req(UpdateCombatResult(), TRUE)
  # Restore the buttons
  if (!isTruthy(ActiveWeapon$ConfirmRoll))
    shinyjs::delay(500, RollInProgress(paste0("do", names(.CombatAction)[ActiveWeapon$LastAction], "Roll"), FALSE))

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

  # Return the results
  if (length(Result) > 0) # two panels
    return(as.character( div(KeyResult,
                             div(Result, class = "shiny-html-output shiny-bound-output"),
                             class = "roll") ))
  else # key result panel, only
    return(as.character(KeyResult))
})



# OUTPUT: Initiative Roll ---------- ----------
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



# OUTPUT: Weapon details Panel ---------- ----------
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



# OUTPUT: Exploration Panel ---------- ----------
output$ShowExploreFightingChances <- reactive({
  return( input$chbExploreChances )
})
outputOptions(output, 'ShowExploreFightingChances', suspendWhenHidden = FALSE)
