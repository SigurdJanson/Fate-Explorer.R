
server <- shinyServer(function(input, output, session) {
  source(file.path("server", "tab_setup_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_ability_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_skill_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_combat_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_explore_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_about_srv.R"), local = TRUE)$value
  
  # Modules
  # - Dialog for combat modifiers
  CombatModifier <- dlgCombatModsModuleServer(
    "btnCombatMods", i18n, 
    reactive(ActiveWeapon$Name), reactive(ActiveWeapon$Type), 
    reactive(ActiveWeapon$Range), reactive(ActiveWeapon$Skill)
  )

  session$onSessionEnded(stopApp)
})
