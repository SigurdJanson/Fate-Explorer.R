# SKILL TAB



LastThrow <- eventReactive(input$doSkillThrow, {
  SkillRoll()
})


output$SkillThrow <- renderTable({
  Throw <- LastThrow()
  Result <- matrix(Throw, nrow = 3L)

  if (input$rdbSkillSource == "ManualSkill") {
    TraitVals <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
    Result <- cbind(Result, TraitVals)
    colnames(Result) <- c(i18n$t("Result"), i18n$t("Ability"))
    
    Success <- VerifySkillRoll(Throw, TraitVals, 
                               input$SkillValue, input$SkillMod)
    Result <- rbind(as.matrix(Result), c(i18n$t(Success), ""))
    
  } else if (input$rdbSkillSource == "CharSkill") {
    Skill <- input$lbCharSkills
    SkillIndex <- which(Character$Skills$name == Skill)
    
    TraitVals <- unlist(Character$Skills[SkillIndex, paste0("ab", 1:3)]) # IDs
    TraitVals <- unlist(Character$Attr[, TraitVals]) # Values
    
    Result <- cbind(Result, TraitVals)
    colnames(Result) <- c(i18n$t("Result"), i18n$t("Ability"))
    
    Success <- VerifySkillRoll(Throw, TraitVals, 
                              Character$Skills[SkillIndex, "value"], 
                              input$SkillMod)
    Result <- rbind(as.matrix(Result), c(i18n$t(Success), ""))
    
  } else {
    colnames(Result) <- i18n$t(c("Result"))
  }
  Result
}, spacing = "l")

