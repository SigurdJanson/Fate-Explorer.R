# SKILL TAB

# Make necessary adjustments when users change the skill source
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
  } else {
    ActiveSkillIdent <<- "TAL_1"
    ActiveSkillSets  <<- Character$Skills
    SelectedSkills <- ActiveSkillSets$GetSkillNames()
    SkillClasses <- ActiveSkillSets$GetSkillClasses() # for dropdown list
    updateSelectInput(session, "lbSkillClasses",
                      choices = c('All Skills' = '', SkillClasses))
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

# React when user selects new skill
observeEvent(
  eventExpr = input$lbCharSkills, 
  handlerExpr = {
    req(input$rdbSkillSource)
    if (input$rdbSkillSource %in% c("ManualSkill", "NoSkill")) {
      ActiveSkillIdent <<- "ANY"
    } else if (input$rdbSkillSource == "CharSkill") {
      req(input$lbCharSkills)
      ActiveSkillIdent <<- input$lbCharSkills
    }
    UpdateSkillResult(UpdateSkillResult()+1)
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
    actionButton("doSkillRoutine", i18n$t("Routine Check"), 
                 icon = gicon("boot-prints"), 
                 width = "49%", style = "font-size: 140%")
  }
})

# Skill role actions (View) -----
BasicSkillSets <- CharacterSkills$new(SkillSet$new("Profane")) # A basic set for manual skill rolls
ActiveSkillIdent <- "ANY"
ActiveSkillSets <- BasicSkillSets
LastSkillRoll <- reactiveValues(Roll = NA, Routine = FALSE)
# Trigger to recognize a new roll - value is unimportant
UpdateSkillResult <- reactiveVal() 


# Initiate skill roll
observeEvent(input$doSkillRoll, {
  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  SkillSource$Roll(ActiveSkillIdent, input$SkillMod, Routine = FALSE)
  LastSkillRoll$Roll <- SkillSource$LastRoll
  LastSkillRoll$Routine <- FALSE
  UpdateSkillResult(0)
})
# Initiate routine check
observeEvent(input$doSkillRoutine, {
  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  SkillSource$Roll(ActiveSkillIdent, input$SkillMod, Routine = TRUE)
  LastSkillRoll$Roll <- NA
  LastSkillRoll$Routine <- SkillSource$LastRoll
  UpdateSkillResult(0)
})


# Display result of skill roll (View) ----
output$SkillRoll <- renderText({
  req(LastSkillRoll$Roll, UpdateSkillResult())
  
  SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
  SkillIndex  <- SkillSource$GetSkillIndex(ActiveSkillIdent)

  RollCheck <- SkillSource$VerifyLastRoll()
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
    RenderedKeyResult <- RenderRollKeyResult(RollCheck$Message, RollCheck$QL, KeyUnit = "ql")
  
  Result <- div(
    RenderedKeyResult,
    div(
      tags$table(Rows, class = "table shiny-table table- spacing-s", style = "width:auto")
    ),
    class = "shiny-html-output shiny-bound-output roll")
  
  return(paste((Result), collapse=""))
})



# Exploration Panel (View) ----
output$ShowExploreSkillChances <- reactive({
  return( input$chbExploreChances & input$rdbSkillSource !=  "NoSkill" )
})
outputOptions(output, 'ShowExploreSkillChances', suspendWhenHidden = FALSE)

