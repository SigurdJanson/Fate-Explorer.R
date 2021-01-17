
server <- shinyServer(function(input, output, session) {
  source(file.path("server", "tab_setup_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_ability_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_skill_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_combat_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_explore_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_about_srv.R"), local = TRUE)$value
  
  # Modules
  dlgCombatModsModuleServer("btnCombatMods", i18n, ActiveWeapon) # Dialog for combat modifiers

  session$onSessionEnded(stopApp)
})
