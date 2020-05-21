# SKILL TAB

LastThrow <- eventReactive(input$doSkillThrow, {
  sample.int(20, 3, TRUE)
})

output$SkillThrow <- renderTable({
  Throw <- LastThrow()
  TraitVals <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
  
  Result <- matrix(Throw, nrow = 3)
  if(!input$SkillIgnore) {
    Result <- cbind(Result, TraitVals)
    colnames(Result) <- c("Result", "Ability")
    
    Success <- VerifySkillRoll(Throw, 
                               TraitVals, 
                               input$SkillValue, input$SkillMod)
    Result <- rbind(as.matrix(Result), c(Success, ""))#rbind(Result, Success)
  } else {
    colnames(Result) <- c("Result")
  }
  Result
}, spacing = "l")

