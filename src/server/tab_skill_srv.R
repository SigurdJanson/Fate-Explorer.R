# SKILL TAB


# Apply group filter for list of character skills
observe({
  # React to changes of the skill groups: filter the skills
  if (exists("Character") && !is.null(Character$Skills)) {
    SelectedItem <- input$lbSkillGroups
    if (length(SelectedItem) != 0 && !is.null(SelectedItem) && SelectedItem != "") {
      SelectedSkills <- which(Character$Skills[["class"]] == input$lbSkillGroups)
      SelectedSkills <- Character$Skills[SelectedSkills, "name"]
      if (!(SelectedItem %in% SelectedSkills)) SelectedItem <- SelectedSkills[1]
      updateSelectInput(session, "lbCharSkills", 
                        choices = SelectedSkills, selected = SelectedItem)
    } else {
      updateSelectInput(session, "lbCharSkills", 
                        choices = Character$Skills[, "name"], selected = SelectedItem)
    }
  }
})

output$uiDoSkillRoutine <- renderUI({
  req(input$rdbSkillSource)
  
  if (input$rdbSkillSource != "NoSkill") {
    # fetch ability values to check of routine check is allowed
    if (input$rdbSkillSource == "ManualSkill") {
      Abilities  <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
      SkillValue <- input$SkillValue
      Modifier   <- input$SkillMod
    } else if (input$rdbSkillSource == "CharSkill") {
      req(input$lbCharSkills)
      Skill      <- input$lbCharSkills
      SkillIndex <- which(Character$Skills$name == Skill)
      Labels     <- unlist(Character$Skills[SkillIndex, paste0("ab", 1:3)]) # IDs
      Abilities  <- unlist(Character$Attr[, Labels])
      SkillValue <- Character$Skills[SkillIndex, "value"]
      Modifier   <- input$SkillMod
    } else Abilities <- c(0, 0, 0)
    
    if (CanRoutineSkillCheck(Abilities, SkillValue, Modifier)) {
      actionButton("doSkillRoutine", i18n$t("Routine Check"), 
                   icon = gicon("boot-prints"), 
                   width = "49%", style = "font-size: 140%")
    }
  }
})

LastSkillRoll <- reactiveValues(Roll = NA, Routine = FALSE)
# Initiate skill roll
observeEvent(input$doSkillThrow, {
  LastSkillRoll$Roll <- SkillRoll()
  LastSkillRoll$Routine <- FALSE
})
# Initiate routine check
observeEvent(input$doSkillRoutine, {
  LastSkillRoll$Roll <- NA
  LastSkillRoll$Routine <- TRUE
})

# Display result of skill roll
output$SkillThrow <- renderText({
  # Regular Roll 
  if (!LastSkillRoll$Routine) {
    # User-defined Skill Values
    req(LastSkillRoll$Roll)
    RollVal <- LastSkillRoll$Roll

    if (input$rdbSkillSource == "ManualSkill") {
      Abilities <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
      
      RollCheck <- VerifySkillRoll(RollVal, Abilities, input$SkillValue, input$SkillMod)
      # Content for Rendering
      Labels    <- NULL
      EffectiveCharVal <- c(Abilities + rep(input$SkillMod, 3), input$SkillValue)
      RollVal    <- c(RollVal, RollCheck$Remainder)
      
      # Skill Values from Character Sheet
    } else if (input$rdbSkillSource == "CharSkill") {
      req(input$lbCharSkills)
      Skill      <- input$lbCharSkills
      SkillIndex <- which(Character$Skills$name == Skill)
      
      Labels    <- unlist(Character$Skills[SkillIndex, paste0("ab", 1:3)]) # IDs
      Abilities <- unlist(Character$Attr[, Labels]) 
      
      RollCheck <- VerifySkillRoll(RollVal, Abilities, 
                                   Character$Skills[SkillIndex, "value"], input$SkillMod)
      # Content for Rendering
      Language <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
      NameMapping <- GetAbilities(Language)
      Labels  <- NameMapping[match(Labels, NameMapping[["attrID"]]), "shortname"]
      RollVal <- c(RollVal, RollCheck$Remainder)
      CharVal <- c(Abilities, Character$Skills[SkillIndex, "value"])
      EffectiveCharVal <- CharVal + c(rep(input$SkillMod, 3), 0)
      
    } else {
      Labels    <- NULL
      Abilities <- NULL
      EffectiveCharVal <- NULL
      RollCheck <- list(Message = "", QL = "-", Remainder = NA) # fake `VerifySkillRoll()` results
    }
  } else {   # Routine Check 
    RollVal <- rep("-", 3)
    
    if (input$rdbSkillSource == "ManualSkill") {
      Labels    <- NULL
      Abilities <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
      Skill <- input$SkillValue
      
      RollCheck <- VerifyRoutineSkillCheck(Abilities, Skill, input$SkillMod)
      EffectiveCharVal <- Abilities
    }
    else  if (input$rdbSkillSource == "CharSkill") {
      req(input$lbCharSkills)
      Skill      <- input$lbCharSkills
      SkillIndex <- which(Character$Skills$name == Skill)
      
      Labels    <- unlist(Character$Skills[SkillIndex, paste0("ab", 1:3)]) # IDs
      Abilities <- unlist(Character$Attr[, Labels])
      Skill <- Character$Skills[SkillIndex, "value"]
      EffectiveCharVal <- Abilities
      
      RollCheck <- VerifyRoutineSkillCheck(Abilities, Skill, input$SkillMod)
      NameMapping <- GetAbilities(Language)
      Labels    <- NameMapping[match(Labels, NameMapping[["attrID"]]), "shortname"]
      
    } else {
      Labels    <- NULL
      Abilities <- NULL
      EffectiveCharVal   <- NULL
      RollCheck <- list(Message = "", QL = "-", Remainder = NA) # fake `VerifySkillRoll()` results
    }
  }

  # Rendering
  # * roll result in reverse order
  Rows <- list(tags$tr( tags$td(i18n$t("Roll")),  lapply(RollVal, tags$td) ))
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



# Exploration Panel ----
output$ShowExploreSkillChances <- reactive({
  return( input$chbExploreChances & input$rdbSkillSource !=  "NoSkill" )
})
outputOptions(output, 'ShowExploreSkillChances', suspendWhenHidden = FALSE)

