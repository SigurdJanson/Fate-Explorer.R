library(shiny)

# UI Skeleton
ui <- shinyUI(fluidPage(
  navbarPage("Dark Eye - Fate Explorer", 
             #titlePanel(title = "titleabc", windowTitle = "title"),
             position = c("static-top"), inverse = TRUE, collapsible = TRUE,
             tabPanel("Decide Your Fate",
                      source(file.path("ui", "tab_skill_ui.R"), local = TRUE)$value
             ),
             
             tabPanel("Fight",
                      source(file.path("ui", "tab_combat_ui.R"), local = TRUE)$value
             ),
             
             tabPanel("Explore", {}),
             
             tabPanel("Setup", 
                      source(file.path("ui", "tab_setup_ui.R"), local = TRUE)$value
             )
  )
))
