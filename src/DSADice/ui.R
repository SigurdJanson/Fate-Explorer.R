#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(shiny)
# 
# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
# 
#     # Application title
#     #,
# 
#     # Sidebar with a slider input for number of bins
#     
#     ui <- 
        navbarPage("Dark Eye - Fate Explorer", 
                     position = c("static-top"), inverse = TRUE, collapsible = TRUE,
                     tabPanel("Decide Your Fate",
                              sidebarLayout(
                                  sidebarPanel(
                                      radioButtons("RollType", "Type of Roll",
                                                   c("Scatter"="p", "Line"="l")
                                      ),
                                      actionButton("ThrowDice", "Now!")
                                  ),
                                  mainPanel({
                                      renderText(Thrown)
                                  })
                              )),
                   tabPanel("Fight",
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("ATValue", "AT", min = 1, max = 20, value = 11),
                                    sliderInput("PAValue", "PA", min = 1, max = 20, value = 4),
                                    numericInput("Damage", "Damage W6+", value = 1),
                                    actionButton("doAttackThrow", "Attack!", icon = icon("skull"), width = "100%"),
                                    actionButton("doParryThrow", "Parry!", icon = icon("shield-alt"), width = "100%"),
                                ),
                                mainPanel(
                                    h3(textOutput("ATValue")),
                                    helpText("Go for it, buster!"),
                                    #textOutput("AttackThrow"),
                                    textOutput("CombatAction")
                                )
                            )),
                   
                   tabPanel("Explore", {}),
                   
                   tabPanel("Setup", {})
    )
    # sidebarLayout(
    #     sidebarPanel(
    #         
    #     ),

        # Show a plot of the generated distribution
       
    # )
#))
