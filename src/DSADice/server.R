#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("dicelogic.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # SkillModifierLabel <- eventReactive(
    #     #impediment
    #     #advantage
    # )
    LastThrow <- eventReactive(input$doSkillThrow, {
        sample.int(20, 3, TRUE)
    })
    output$SkillThrow <- renderPrint({
        Result <- LastThrow()
        if(!input$SkillIgnore) {
            Success <- VerifySkillRoll(Result, 
                                       c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3), 
                                       input$SkillValue, input$SkillMod)
            Result <- c(Result, ifelse(Success, "Success", "Failure"))
        }
        cat(Result)
    })

    
    # FIGHTING TAB --------------------
    FightVal <- reactiveValues(AT = NA, PA = NA)
    
    observeEvent(input$doAttackThrow, {
        FightVal$AT <- sample.int(20, 1)
        FightVal$PA <- NA
    })
    
    observeEvent(input$doParryThrow, {
        FightVal$PA <- sample.int(20, 1)
        FightVal$AT <- NA
    })
    
    output$CombatAction <- renderPrint({
        if (!is.na(FightVal$AT)) {
            Result <- "Attack\n"
            Result <- c(Result, paste(FightVal$AT, ifelse(FightVal$AT <= input$ATValue, "Success", "Failure")))
            Result <- c(Result, "Damage:")
            Result <- c(Result, sample.int(6, 1)+input$Damage)
            Result <- c(Result, "\n")
        } else if (!is.na(FightVal$PA)) {
            cat("Parry\n")
            Result <- paste(FightVal$PA, ifelse(FightVal$PA <= input$PAValue, "Success", "Failure"))
        } else Result = ""
        
        cat(Result)
    })
})
