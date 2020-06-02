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
output$SkillThrow <- renderTable({
  Throw <- LastThrow()
  Result <- matrix(Throw, nrow = 3L)

  if (input$rdbSkillSource == "ManualSkill") {
    TraitVals <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
    Result <- cbind(Result, TraitVals)
    colnames(Result) <- c(i18n$t("Result"), i18n$t("Ability"))
    
    Roll <- VerifySkillRoll(Throw, TraitVals, 
                            input$SkillValue, input$SkillMod)
    Result <- rbind(as.matrix(Result), 
                    c(i18n$t("Skill"), input$SkillValue),
                    c(i18n$t(Roll$Message), ""), 
                    c(i18n$t("QL"), ifelse(Roll$QL > 0, Roll$QL, "-") ))
    
  } else if (input$rdbSkillSource == "CharSkill") {
    req(input$lbCharSkills)
    Skill <- input$lbCharSkills
    SkillIndex <- which(Character$Skills$name == Skill)
    
    TraitVals <- unlist(Character$Skills[SkillIndex, paste0("ab", 1:3)]) # IDs
    TraitVals <- unlist(Character$Attr[, TraitVals]) # Values
    
    Result <- cbind(Result, TraitVals)
    colnames(Result) <- c(i18n$t("Result"), i18n$t("Ability"))
    
    Roll <- VerifySkillRoll(Throw, TraitVals, 
                              Character$Skills[SkillIndex, "value"], 
                              input$SkillMod)
    Result <- rbind(as.matrix(Result), 
                    c(i18n$t("Skill"), Character$Skills[SkillIndex, "value"]),
                    c(i18n$t(Roll$Message), ""), 
                    c(i18n$t("QL"),ifelse(Roll$QL > 0, Roll$QL, "-") ))
    
  } else {
    colnames(Result) <- i18n$t(c("Result"))
  }
  Result
}, spacing = "l")

