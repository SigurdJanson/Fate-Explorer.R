# SKILL TAB

#print(length(input))
#print(input$Values) # can only be done from inside a reactive expression or observer.

LastThrow <- eventReactive(input$doSkillThrow, {
  SkillRoll()
})

output$SkillThrow <- renderTable({
  Throw <- LastThrow()
  TraitVals <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
  
  Result <- matrix(Throw, nrow = 3L)
  if(!input$SkillIgnore) {
    Result <- cbind(Result, TraitVals)
    colnames(Result) <- c(i18n$t("Result"), i18n$t("Ability"))
    
    Success <- VerifySkillRoll(Throw, 
                               TraitVals, 
                               input$SkillValue, input$SkillMod)
    Result <- rbind(as.matrix(Result), c(i18n$t(Success), ""))#rbind(Result, Success)
  } else {
    colnames(Result) <- i18n$t(c("Result"))
  }
  Result
}, spacing = "l")

