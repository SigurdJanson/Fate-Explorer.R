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
                                      checkboxInput("SkillIgnore", "Ignore skill, just throw", TRUE),
                                      hr(),
                                      sliderInput("SkillTrait1", "1st Trait", min = 1, max = 20, value = 11),
                                      sliderInput("SkillTrait2", "2nd Trait", min = 1, max = 20, value = 11),
                                      sliderInput("SkillTrait3", "3rd Trait", min = 1, max = 20, value = 11),
                                      hr(),
                                      sliderInput("SkillValue", "Skill", min = 0, max = 20, value = 4),
                                      sliderInput("SkillMod", "Modifier", min = -10, max = 10, value = 0),
                                      conditionalPanel(condition = "input.SkillMod < 0", helpText("Impediment")),
                                      conditionalPanel(condition = "input.SkillMod > 0", helpText("Advantage"))
                                  ),
                                  mainPanel(
                                      actionButton("doSkillThrow", "Now!"),
                                      hr(),
                                      h3(textOutput("SkillThrow"))
                                  )
                              )),
                   tabPanel("Fight",
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("ATValue", "AT", min = 1, max = 20, value = 11),
                                    sliderInput("PAValue", "PA", min = 1, max = 20, value = 4),
                                    numericInput("Damage", "Damage W6+", value = 1),
                                ),
                                mainPanel(
                                    actionButton("doAttackThrow", "Attack!", icon = icon("skull"), width = "49%"),
                                    actionButton("doParryThrow", "Parry!", icon = icon("shield-alt"), width = "49%"),
                                    hr(),
                                    h3(textOutput("CombatAction"))
                                    
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
