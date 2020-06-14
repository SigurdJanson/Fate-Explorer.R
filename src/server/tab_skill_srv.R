# SKILL TAB


# Apply group filter for list of character skills
observe({
  # React to changes of the skill groups: filter the skills
  if (exists("Character") && !is.null(Character$Skills)) {
    #SkillClasses <- unique(Character$Skills[["class"]])
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


# Initiate skill roll
LastThrow <- eventReactive(input$doSkillThrow, {
  SkillRoll()
})


# Display result of skill roll
output$SkillThrow <- renderText({
  Values <- LastThrow()
  
  if (input$rdbSkillSource == "ManualSkill") {
    Abilities <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
    
    RollCheck <- VerifySkillRoll(Values, Abilities, input$SkillValue, input$SkillMod)
    # Content for Rendering
    Labels    <- NULL
    Abilities <- c(Abilities, input$SkillValue)
    Values    <- c(Values, RollCheck$Remainder)
    
  } else if (input$rdbSkillSource == "CharSkill") {
    req(input$lbCharSkills)
    Skill      <- input$lbCharSkills
    SkillIndex <- which(Character$Skills$name == Skill)
    
    Labels    <- unlist(Character$Skills[SkillIndex, paste0("ab", 1:3)]) # IDs
    Abilities <- unlist(Character$Attr[, Labels]) 
    
    RollCheck <- VerifySkillRoll(Values, Abilities, 
                                 Character$Skills[SkillIndex, "value"], input$SkillMod)
    # Content for Rendering
    Language <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
    NameMapping <- GetAbilities(Language)
    Labels    <- NameMapping[match(Labels, NameMapping[["attrID"]]), "shortname"]
    Abilities <- c(Abilities, Character$Skills[SkillIndex, "value"])
    Values    <- c(Values, RollCheck$Remainder)
    
  } else {
    Labels    <- NULL
    Abilities <- NULL
    RollCheck <- list(Message = "", QL = -1, Remainder = NA) # fake VerifySkillRoll() results
  }

  # Rendering
  Rows <- list(tags$tr( tags$td(i18n$t("Roll")),  lapply(Values, tags$td) ))
  if (!is.null(Abilities)) 
    Rows <- list(tags$tr( tags$td(i18n$t("Value")), lapply(Abilities-input$SkillMod, tags$td) ), Rows)
  if (!is.null(Labels)) 
    Rows <- list(tags$th( lapply(Labels, tags$td)), tags$td(), Rows)
  Result <- tags$div(
    tags$table(Rows, class = "table shiny-table table- spacing-s", style = "width:auto"),
    p( span(i18n$t(RollCheck$Message)), 
       span(ifelse(RollCheck$QL > 0, paste(i18n$t("with"), RollCheck$QL, i18n$t("QL")), "")) ),
    class = "shiny-html-output shiny-bound-output")
  
  return(paste((Result), collapse=""))
})



# Exploration Panel ----
output$ShowExploreSkillChances <- reactive({
  return( input$chbExploreChances & input$rdbSkillSource !=  "NoSkill" )
})
outputOptions(output, 'ShowExploreSkillChances', suspendWhenHidden = FALSE)

