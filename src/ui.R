#
#
library(shiny)


shinyUI(fluidPage(
#     # Application title
#     titlePanel(title, windowTitle = title)
     ui <- 
        navbarPage("Dark Eye - Fate Explorer", 
                   position = c("static-top"), inverse = TRUE, collapsible = TRUE,
                   tabPanel("Decide Your Fate",
                            source(file.path("ui", "tab_skill_ui.R"),  local = TRUE)$value
                           ),
                   
                   tabPanel("Fight",
                            source(file.path("ui", "tab_combat_ui.R"),  local = TRUE)$value
                           ),
                   
                   tabPanel("Explore", {}),
                   
                   tabPanel("Setup", {})
    )
))
