# SETUP


sidebarLayout(
  sidebarPanel(
    fileInput("CharFile", i18n$t("Choose Optolith File (json)"),
              multiple = FALSE,
              accept = c("text/json","text/plain",".json"),
              buttonLabel = i18n$t("Browse..."),
              placeholder = i18n$t("No file selected")),
    hr(),#,
    checkboxInput(inputId = "chbExploreChances", label = i18n$t("Explore roll probabilities")),
    checkboxInput(inputId = "chbShowImprovWeapons", label = i18n$t("Show improvised weapons")),
    conditionalPanel(
      condition = "output.ShowHarvestWeaponDetails",
      checkboxInput(inputId = "chbHarvestWeaponDetails", label = i18n$t("Retrieve weapon details from ulisses wiki"))
    ),
  ),
  mainPanel(
    h3(textOutput("CharacterName")),
    conditionalPanel(
      condition = "output.ShowSetupAttr",
      hr(), h3(i18n$t("Abilities")),
      tableOutput("SetupAttr")
    ),
    conditionalPanel(
      condition = "output.ShowSetupStatesNConds",
      hr(), h3(i18n$t("States"), i18n$t("and"), i18n$t("Conditions")),
      htmlOutput("SetupStatesNConds")
    ),
    conditionalPanel(
      condition = "output.ShowSetupWeapons",
      hr(), h3(i18n$t("Weapons")),
      tableOutput("SetupWeapons")
    ),
    conditionalPanel(
      condition = "output.ShowSetupSkills",
      hr(), h3(i18n$t("Skills")),
      tableOutput("SetupSkills")
    ),
    conditionalPanel(
      condition = "output.ShowSetupSpells",
      hr(), h3(i18n$t("Spells")),
      tableOutput("SetupSpells")
    ),
    conditionalPanel(
      condition = "output.ShowSetupChants",
      hr(), h3(i18n$t("Chants")),
      tableOutput("SetupChants")
    ),
    conditionalPanel(
      condition = "output.ShowSetupJson",
      hr(), h3(i18n$t("Json File")),
      verbatimTextOutput("RawContents")
    )
  )
)

