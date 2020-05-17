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
    # SKILL TAB   -----------------------
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

    
    # FIGHTING TAB --------------------
    FightVal <- reactiveValues(AT = NA, PA = NA, Success = "Fail", Damage = 0)
    
    observeEvent(input$doAttackThrow, {
        FightVal$AT <- sample.int(20, 1)
        FightVal$PA <- NA
        FightVal$Success <- VerifyCombatRoll(FightVal$AT, input$ATValue)
        FightVal$Damage <- sample.int(6, 1) + input$Damage
    })
    
    observeEvent(input$doParryThrow, {
        FightVal$PA <- sample.int(20, 1)
        FightVal$AT <- NA
        FightVal$Success <- VerifyCombatRoll(FightVal$PA, input$PAValue)
        FightVal$Damage <- 0
    })
    
    output$CombatAction <- renderPrint({
        if (!is.na(FightVal$AT)) {
            Result <- paste("Attack:", FightVal$AT, FightVal$Success)
        } else if (!is.na(FightVal$PA)) {
            Result <- "Attack: "
            Result <- paste("Parry:", FightVal$PA, FightVal$Success)
        } else Result = ""

        cat(Result)
    })
    
    output$CombatConfirmation <- reactive({
        return( (FightVal$Success == "Fumble") | 
                    (FightVal$Success == "Critical") )
    })
    outputOptions(output, 'CombatConfirmation', suspendWhenHidden = FALSE)
    
    output$CombatConfirm <- renderPrint({
        Limit <- ifelse(is.na(FightVal$AT), FightVal$PA, FightVal$AT)

        if (FightVal$Success == "Fumble") {
            Fumble <- sample.int(20, 1)
            if (Fumble > Limit) {
                #icon("frown-open")
                Result <- "Fumble!"
            } else {
                Result <- "Fumble avoided"
                #FightVal$Success <- "Fail"
            }
        } else if (FightVal$Success == "Critical") {
            Critical <- sample.int(20, 1)
            if (Critical <= Limit) {
                #icon("smile-open")
                Result <- paste("Critical confirmed - Damage:", FightVal$Damage)
            } else {
                Result <- "Critical was lost"
                #FightVal$Success <- "Success"
            }
        }
        cat(Result)
    })

    # output$CombatCriticals <- renderUI({
    #     Limit <- ifelse(is.na(FightVal$AT), FightVal$PA, FightVal$AT)
    #         
    #     if (FightVal$Success == "Fumble") {
    #         Fumble <- sample.int(20, 1)
    #         if (Fumble > Limit) {
    #             icon("frown-open")
    #             helpText("Fumble confirmed")
    #         } else {
    #             helpText("Fumble avoided")
    #             FightVal$Success <- "Fail"
    #         }
    #     } else if (FightVal$Success == "Critical") {
    #         Critical <- sample.int(20, 1)
    #         if (Critical <= Limit) {
    #             icon("smile-open")
    #             helpText("Critical confirmed")
    #         } else {
    #             helpText("Critical not confirmed")
    #             FightVal$Success <- "Success"
    #         }
    #     }
    #     
    #     helpText(FightVal$Success)
    #     if (!is.na(FightVal$AT)) {
    #         switch (FightVal$Success,
    #                 Critical = {helpText(paste("Damage x2:", FightVal$Damage*2))},
    #                 Success = {helpText(paste("Damage:", FightVal$Damage))},
    #                 Fail = {helpText("-")},
    #                 Fumble = {helpText("-")}
    #         )
    #     }
    #     helpText("HELP!")
    # })
})
