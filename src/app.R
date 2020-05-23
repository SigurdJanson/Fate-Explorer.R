library(shiny)
library(jsonlite)
library(shiny.i18n)

if(file.exists(file.path("src", "data", "lang.json"))) {
  i18n <- Translator$new(translation_json_path = file.path("src", "data", "lang.json"))
} else if(file.exists(file.path("data", "lang.json"))) {
  i18n <- Translator$new(translation_json_path = file.path("data", "lang.json"))
} else stopApp("No translation file found")
i18n$set_translation_language("en")

source("dicelogic.R", local = TRUE)
source("readoptjson.R", local = TRUE)
source("rules.R", local = TRUE)

source("getui.R", local = TRUE)
source("getserver.R", local = TRUE)
shinyApp(ui = ui, server = server)
