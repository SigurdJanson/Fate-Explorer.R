# UI Skeleton

source(file.path("ui", "btn_skill_rdbsource.R"), local = TRUE)

ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "fatexplorer.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "rolloutput.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "waiting.css")
  ),
  theme = "bootstrap.css",
  navbarPage(title = div(
               img(src="FateExplorer_Blood_Logo.svg", style="margin-top: -14px;", height = 50)
             ),
             windowTitle = "Fate Explorer", id ="uiTabset",
             position = c("static-top"), inverse = TRUE, collapsible = TRUE,
             
             #header = div(
             #         source(file.path("ui", "header_ui.R"), local = TRUE)$value
             #),
             
             tabPanel(i18n$t("Be"),
                      source(file.path("ui", "tab_ability_ui.R"), local = TRUE)$value
             ),
             
             tabPanel(i18n$t("Act"),
               source(file.path("ui", "tab_skill_ui.R"), local = TRUE)$value
             ),
             
             tabPanel(i18n$t("Fight"),
               source(file.path("ui", "tab_combat_ui.R"), local = TRUE)$value
             ),

             tabPanel(i18n$t("Setup"), 
               source(file.path("ui", "tab_setup_ui.R"), local = TRUE)$value
             ),
             
             tabPanel(i18n$t("About..."), 
               source(file.path("ui", "tab_about_ui.R"), local = TRUE)$value
             )
  )
))
