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
shinyServer(function(input, output, session) {
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
    FightVal <- reactiveValues(Action = NA, Roll = NA, 
                               Success = "Fail", Damage = 0,
                               ConfirmRoll = 0, Confirmation = NA)
    
    observeEvent(input$doAttackThrow, {
        FightVal$Action  <- "Attack"
        FightVal$Roll    <- CombatRoll()
        FightVal$Success <- VerifyCombatRoll(FightVal$Roll, input$ATValue)
        # Level of success
        if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
            FightVal$ConfirmRoll <- CombatRoll()
            Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, input$ATValue)
            FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
            if (!FightVal$Confirmation)
                FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")
        } else {
            FightVal$ConfirmRoll <- NA
        }
        FightVal$EffectOfFumble <- NA
        # Damage
        if (FightVal$Success == "Critical")
            FightVal$Damage <- 2 * DamageRoll(input$Damage)
        else if  (FightVal$Success == "Success")
            FightVal$Damage <- DamageRoll(input$Damage)
        else 
            FightVal$Damage <- NA
    })
    
    observeEvent(input$doParryThrow, {
        FightVal$Action  <- "Parry"
        FightVal$Roll    <- CombatRoll()
        FightVal$Success <- VerifyCombatRoll(FightVal$Roll, input$PAValue)
        FightVal$Damage  <- NA
        # Level of success
        if(FightVal$Success == "Critical" || FightVal$Success == "Fumble") {
            FightVal$ConfirmRoll <- CombatRoll()
            Confirmation <- VerifyCombatRoll(FightVal$ConfirmRoll, input$ATValue)
            FightVal$Confirmation <- Confirmation == "Success" | Confirmation == "Critical"
            if (!FightVal$Confirmation)
                FightVal$Success <- ifelse(FightVal$Success == "Critical", "Success", "Fail")
        } else {
            FightVal$ConfirmRoll <- NA
        }
        FightVal$EffectOfFumble <- NA
    })
    
    output$CombatAction <- renderPrint({
        Result <- paste0(FightVal$Action, ": ", FightVal$Roll, " - ", FightVal$Success)
        cat(Result)
    })
    
    # Confirmation Panel
    output$ShowCombatConfirm <- reactive({
        #return( (FightVal$Success == "Fumble") | (FightVal$Success == "Critical") )
        return(!is.na(FightVal$ConfirmRoll))
    })
    outputOptions(output, 'ShowCombatConfirm', suspendWhenHidden = FALSE)
    
    output$CombatConfirm <- renderPrint({
        if (FightVal$Success == "Fumble") {
            Result <- "Fumble!" #icon("frown-open")
        } else if (FightVal$Success == "Critical") {
            Result <- "Critical confirmed!"
        } else if (FightVal$Success == "Success") {
            Result <- "Critical was lost :-("
        } else if (FightVal$Success == "Fail") {
            Result <- "Fumble avoided :-)"
        } else Result <- ""
        
        cat(Result)
    })

    # Damage Panel
    output$ShowCombatDamage <- reactive({
        return( !is.na(FightVal$Damage) )
    })
    outputOptions(output, 'ShowCombatDamage', suspendWhenHidden = FALSE)
    
    output$CombatDamage <- renderPrint({
        cat("Hit points: ", FightVal$Damage)
    })
    
    
    # Fumble Panel
    output$ShowCombatFumble <- reactive({
        return( FightVal$Success == "Fumble" )
    })
    outputOptions(output, 'ShowCombatFumble', suspendWhenHidden = FALSE)
    
    output$CombatFumble <- renderPrint({
        if(!is.na(FightVal$EffectOfFumble)) {
            Result <- GetCombatFumbleEffect(FightVal$EffectOfFumble)
        } else Result <- "Test"

        cat(Result)
    })
    
    observeEvent(input$doCombatFumble, {
        FightVal$EffectOfFumble <- CombatFumbleRoll()
        #session$sendCustomMessage(type = 'testmessage',
        #                          message = 'Thank you for clicking')
    })
})
