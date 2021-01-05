# SKILL TAB

# Make necessary adjustments when users change the skill source
# 1. determine which panel is open
# 2. change content and status of widgets
observeEvent(input$rdbSkillSource, {
  if (input$rdbSkillSource %in% c("NoSkill", "ManualSkill")) {
    ActiveSkillIdent <<- "ANY"
    ActiveSkillSets  <<- BasicSkillSets
    SkillSource <- ActiveSkillSets$GetSkillSet(ActiveSkillIdent)
    if (input$rdbSkillSource == c("NoSkill"))
      SkillSource$UncheckSkill(ActiveSkillIdent)
    else {
      Abilities <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
      Value <- input$SkillValue
      SkillSource$SetSkill(ActiveSkillIdent, Abilities, Value)
    }
    
  } 
  # if active panel is the "Character" update widget content
  else { 
    ActiveSkillIdent <<- "TAL_1"
    ActiveSkillSets  <<- Character$Skills
    SelectedSkills <- ActiveSkillSets$GetSkillNames()
    
    SkillClasses <- ActiveSkillSets$GetSkillClasses() # for filter drop-down list
    names(SkillClasses) <- SkillClasses
    SkillClasses <- c("", SkillClasses)
    names(SkillClasses)[1] <- i18n$t("All Skills")
    updateSelectInput(session, "lbSkillClasses",
                      choices = SkillClasses) # TODO: translate
    updateSelectInput(session, "lbCharSkills", choices = SelectedSkills,
                      selected = 1)
  }
  LastSkillRoll$Roll <- NA
  LastSkillRoll$Routine <- FALSE
})


# Apply group filter for list of character skills
observeEvent(
  eventExpr = input$lbSkillClasses, 
  handlerExpr = {
    # React to changes of the skill groups: filter the skills
    if (exists("Character") && !is.null(Character$Skills)) {
      Filter <- input$lbSkillClasses
      SelectedItem <- input$lbCharSkills # to restore currently selected item after filter
    
      if (isTruthy(Filter)) {
        SkillSource <- ActiveSkillSets$GetSkillSet(Class = Filter)
        SelectedSkills <- which(SkillSource$Skills[["class"]] == input$lbSkillClasses)
        SelectedSkills <- SkillSource$GetSkillName(SelectedSkills)
      } else {
        # No filter set
        SelectedSkills <- ActiveSkillSets$GetSkillNames()
      }
  
      # if SelectedItem is not part of current selection:
      if (!isTruthy(SelectedItem) || !(SelectedItem %in% SelectedSkills)) 
        Filter <- SelectedSkills[1]
      updateSelectInput(session, "lbCharSkills", choices = SelectedSkills,
                        selected = SelectedItem)
    }
  }#handler
)


# React when users select new skill
observeEvent(
  eventExpr = input$lbCharSkills, 
  handlerExpr = {
    req(input$rdbSkillSource)
    if (input$rdbSkillSource %in% c("ManualSkill", "NoSkill")) {
      ActiveSkillIdent <<- "ANY"
    } else if (input$rdbSkillSource == "CharSkill") {
      req(input$lbCharSkills)
      ActiveSkillIdent <<- input$lbCharSkills
      SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
      # Update LastSkill to redraw the visible roll result
      SkillSource$LastSkill <- SkillSource$GetSkillIndex(ActiveSkillIdent)
      SkillSource$InvalidateRoll()
      LastSkillRoll$Roll <- NA
    }
    UpdateSkillResult(ifelse(isTruthy(UpdateSkillResult()), UpdateSkillResult()+1, 0))
  }#handler
)


# React when users change any of the SkillTrait and SkillValue-Sliders
observeEvent(
  eventExpr = {
    req(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3, 
        input$SkillValue)
  }, handlerExpr = {
    req(ActiveSkillSets)
    SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
    SkillSource$SetSkill(ActiveSkillIdent, 
                         Abilities = c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3), 
                         SkillValue = input$SkillValue)
    SkillSource$VerifyLastRoll()
    UpdateSkillResult(UpdateSkillResult()+1) # Force update of result and routine button
  },#handler
  ignoreInit = TRUE
)


# React when users change the modifier
observeEvent(eventExpr = input$SkillMod, 
  handlerExpr = {
    req(ActiveSkillSets, LastSkillRoll$Roll)
    SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
    SkillSource$UpdateModifier(input$SkillMod)
    SkillSource$VerifyLastRoll()
    UpdateSkillResult(UpdateSkillResult()+1) # Force update of result and routine button
  },#handler
  ignoreInit = TRUE
)


# Decide whether to show the routine check button or not.
output$uiDoSkillRoutine <- renderUI({
  req(input$rdbSkillSource, UpdateSkillResult())

  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  RoutineCheck <- SkillSource$CanRoutineCheck(ActiveSkillIdent, input$SkillMod)
  if (RoutineCheck) {
    actionButton("doSkillRoutine", span(i18n$t("Routine Check"), id="lbldoSkillRoutine"), 
                 icon = gicon("boot-prints"), 
                 width = "49%", style = "font-size: 140%")
  }
})


# Skill role actions (View) -------------------------------------------

# A basic set for manual skill rolls
# This is a dummy for rolls without character sheet
BasicSkillSets <- CharacterSkills$new(SkillSet$new("Mundane"))

# Identifies the currently selected skill
ActiveSkillIdent <- "ANY"

# The selected set of skills
ActiveSkillSets <- BasicSkillSets

LastSkillRoll <- reactiveValues(Roll = NA, Routine = FALSE)
# Trigger to recognize a new roll - value is unimportant
UpdateSkillResult <- reactiveVal(NULL) 


# Initiate skill roll
observeEvent(input$doSkillRoll, {
  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  SkillSource$Roll(ActiveSkillIdent, input$SkillMod, Routine = FALSE)
  LastSkillRoll$Roll <- SkillSource$LastRoll
  LastSkillRoll$Routine <- FALSE
  UpdateSkillResult(ifelse(is.null(UpdateSkillResult()), 0, UpdateSkillResult()+1))
  
  RollInProgress("doSkillRoll", TRUE)
})


# Initiate routine check
observeEvent(input$doSkillRoutine, {
  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  SkillSource$Roll(ActiveSkillIdent, input$SkillMod, Routine = TRUE)
  LastSkillRoll$Roll <- NA
  LastSkillRoll$Routine <- SkillSource$LastRoll
  UpdateSkillResult(sample.int(1000, 1))
  
  RollInProgress("doSkillRoutine", TRUE)
})


observeEvent(input$doSkillConfirm, { # Confirm Critical/Botch
  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  UpdateSkillResult( SkillSource$Confirm()+100 )
})


observeEvent(input$doSkillFumble, { # Show fumble result
  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  UpdateSkillResult( SkillSource$FumbleRoll()+200 )
})



# Display result of skill roll (View) ----
output$SkillRoll <- renderText({
  req(UpdateSkillResult())
  
  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  SkillIndex  <- SkillSource$GetSkillIndex(ActiveSkillIdent)

  EffectiveCharVal <- SkillSource$GetSkillValues(SkillIndex, input$SkillMod)
  Labels  <- SkillSource$GetAbilityLabels(SkillIndex)
  RollVal <- SkillSource$GetLastScore()
  if (anyNA(RollVal)) # routine check 
    RollVal <- rep("-", 4)
  else # die roll
    if (input$rdbSkillSource != "NoSkill")
      RollVal <- c(RollVal, SkillSource$LastRemainder)
  
  # Rendering
  # * roll result in reverse order
  Rows <- list(tags$tr( tags$td(i18n$t("Roll")), lapply(RollVal, tags$td) ))
  if (!is.null(EffectiveCharVal))
    Rows <- list(tags$tr( tags$td(i18n$t("Value")), lapply(EffectiveCharVal, tags$td) ), Rows)
  if (!is.null(Labels)) 
    Rows <- list(tags$th( lapply(Labels, tags$td)), tags$td(), Rows)
  # * final rendering
  if (input$rdbSkillSource == "NoSkill")
    RenderedKeyResult <- ""
  else
    RenderedKeyResult <- RenderRollKeyResult(names(SkillSource$LastResult), 
                                             SkillSource$LastQL, KeyUnit = "ql")
  
  # Confirmation
  ConfirmResult <- tagList()
  if (SkillSource$RollNeedsConfirmation())
    ConfirmResult <- tagAppendChild(ConfirmResult, 
                                    RenderConfirmationRequest("doSkillConfirm", SkillSource$LastResult))
  else {
    if (isTruthy(SkillSource$ConfirmRoll))
      ConfirmResult <- tagAppendChild(ConfirmResult, 
                                      RenderRollConfirmation(names(SkillSource$LastResult),
                                                             SkillSource$ConfirmRoll, 
                                                             i18n))
    if (SkillSource$NeedFumbleRoll()) {# show link button to perform the roll
      ConfirmResult <- tagAppendChild(ConfirmResult, 
                                      RenderFumbleRollRequest("doSkillFumble"))
    } else if (isTruthy(SkillSource$LastFumbleEffect)) { # show result of fumble roll
      ConfirmResult <- tagAppendChild(ConfirmResult, RenderFumbleRollEffect(SkillSource$LastFumbleEffect))
    }
  }
  
  # Put result together
  Result <- div(
    RenderedKeyResult,
    div(tags$table(Rows, class = "table shiny-table table- spacing-s", style = "width:auto"), 
        ConfirmResult), 
    class = "shiny-html-output shiny-bound-output roll")
  
  # Finally restore the buttons
  Sys.sleep(0.3) # artificially delay the result for increased tension
  shinyjs::delay(700, {
    RollInProgress("doSkillRoll", FALSE)
    RollInProgress("doSkillRoutine", FALSE)
  })
  
  return(paste((Result), collapse=""))
})



# Exploration Panel (View) ----
output$ShowExploreSkillChances <- reactive({
  return( input$chbExploreChances & input$rdbSkillSource !=  "NoSkill" )
})
outputOptions(output, 'ShowExploreSkillChances', suspendWhenHidden = FALSE)
