# UI Skeleton

#jscode <- "shinyjs.refresh = function() { history.go(0); }"


ui <- shinyUI(fluidPage(
  # useShinyjs(),
  # extendShinyjs(text = jscode),
  navbarPage(i18n$t("The Dark Eye - Fate Explorer"), 
             #titlePanel(title = "titleabc", windowTitle = "title"),
             position = c("static-top"), inverse = TRUE, collapsible = TRUE,
             tabPanel(i18n$t("Decide Your Fate"),
                      source(file.path("ui", "tab_skill_ui.R"), local = TRUE)$value
             ),
             
             tabPanel(i18n$t("Fight"),
                      source(file.path("ui", "tab_combat_ui.R"), local = TRUE)$value
             ),
             
             tabPanel(i18n$t("Explore"), {}),
             
             tabPanel(i18n$t("Setup"), 
                      source(file.path("ui", "tab_setup_ui.R"), local = TRUE)$value
             )
  )
))
