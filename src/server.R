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
    source(file.path("server", "tab_skill_srv.R"),  local = TRUE)$value
    source(file.path("server", "tab_combat_srv.R"),  local = TRUE)$value
})
