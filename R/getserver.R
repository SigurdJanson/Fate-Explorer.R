
server <- shinyServer(function(input, output, session) {
  source(file.path("server", "tab_setup_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_ability_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_skill_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_combat_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_explore_srv.R"), local = TRUE)$value
  source(file.path("server", "tab_about_srv.R"), local = TRUE)$value
  
  # Modules --------
  # - Dialog for combat modifiers
  CombatModsModulePayload <- reactiveValues(Name = ActiveWeapon$Name, Type = ActiveWeapon$Type,
                                            Range = ActiveWeapon$Range, Skill = ActiveWeapon$Skill)
  CombatModifier <- dlgCombatModsModuleServer(
    "btnCombatMods", i18n,
    CombatModsModulePayload$Name,  CombatModsModulePayload$Type,
    CombatModsModulePayload$Range, reactive(CombatModsModulePayload$Skill)
  )

  # ----------------
  session$onSessionEnded(stopApp)
})
