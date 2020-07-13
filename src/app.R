library(shiny)
library(jsonlite)
library(shinyjs)
library(shiny.i18n)
library(shinyWidgets)
library(ggplot2)
library(rvest)

# Set language
if (file.exists(file.path("src", "data", "lang.json"))) {
  i18n <- Translator$new(translation_json_path = file.path("src", "data", "lang.json"))
} else if (file.exists(file.path("data", "lang.json"))) {
  i18n <- Translator$new(translation_json_path = file.path("data", "lang.json"))
} else stopApp("No translation file found")
i18n$set_translation_language("de")

# 
source("rules.R", local = TRUE)
source("helpers.R", local = TRUE, encoding = "UTF-8")
source("dicelogic.R", local = TRUE)
source("readoptjson.R", local = TRUE)
.Language <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)

source("explorechances.R", local = TRUE)
source("weapon.R", local = TRUE, encoding = "UTF-8")
source("skillset.R", local = TRUE, encoding = "UTF-8")

# RUN
source("getui.R", local = TRUE)
source("getserver.R", local = TRUE)
shinyApp(ui = ui, server = server)