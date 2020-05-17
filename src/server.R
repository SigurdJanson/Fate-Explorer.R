
library(shiny)
require(rjson)
source("dicelogic.R")
source("readoptjson.R")



server <- shinyServer(function(input, output, session) {
  source(file.path("server", "tab_skill_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_combat_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_setup_srv.R"), local = TRUE)$value
})
