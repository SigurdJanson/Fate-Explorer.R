#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # LastThrow <- "NULL"
    # 
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    # output$TraitDie <- renderText(
    #     paste("Here it comes:", LastThrow)
    # )
    # 
    # output$Thrown <- renderText(
    #    LastThrow
    # )
    
    # observeEvent(input$ThrowDice, {
    #     #cat("Showing", input$x, "rows\n")
    #     LastThrow <- sample.int(20, 3)
    #     cat(LastThrow, "\n")
    # })
    LastThrow <- eventReactive(input$ThrowDice, {
        sample.int(20, 3)
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
            cat("Attack\n")
            Result <- paste(FightVal$AT, ifelse(FightVal$AT <= input$ATValue, "Success", "Failure"))
        } else if (!is.na(FightVal$PA)) {
            cat("Parry\n")
            Result <- paste(FightVal$PA, ifelse(FightVal$PA <= input$PAValue, "Success", "Failure"))
        } else Result = ""
        
        cat(Result)
    })
})
