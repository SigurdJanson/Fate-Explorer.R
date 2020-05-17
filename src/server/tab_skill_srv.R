# SKILL TAB

LastThrow <- eventReactive(input$doSkillThrow, {
  sample.int(20, 3, TRUE)
})

output$SkillThrow <- renderPrint({
  Result <- LastThrow()
  if(!input$SkillIgnore) {
    Success <- VerifySkillRoll(Result, 
                               c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3), 
                               input$SkillValue, input$SkillMod)
    Result <- c(Result, Success)
  }
  cat(Result)
})

