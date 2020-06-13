
sidebarLayout(
  sidebarPanel(
    sliderInput("inpAbility", i18n$t("Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
    div(style="float:right",
        conditionalPanel(condition = "input.inpAbilityMod < 0", 
                         icon("minus-circle"), i18n$t("Impediment")),
        conditionalPanel(condition = "input.inpAbilityMod > 0", 
                         icon("plus-circle"), i18n$t("Advantage"))
    ),
    sliderInput("inpAbilityMod", i18n$t("Modifier"), min = -10L, max = 10L, value = 0L, step = 1L)
  ),
  mainPanel(
      actionButton("doAbilityRoll", i18n$t("Ability"), width = "100%"),
      # conditionalPanel(condition = "input.rdbSkillSource == 'CharSkill' || input.rdbSkillSource == 'ManualSkill'",
      #                  actionButton("doCourageRoll", i18n$t("COU")),
      #                  actionButton("doSagacityRoll", i18n$t("SAG")),
      #                  actionButton("doIntuitionRoll", i18n$t("INT")),
      #                  actionButton("doCharismaRoll", i18n$t("CHA")),
      #                  actionButton("doDexterityRoll", i18n$t("DEX")),
      #                  actionButton("doAgilityRoll", i18n$t("AGI")),
      #                  actionButton("doConstitutionRoll", i18n$t("CON")),
      #                  actionButton("doStrengthRoll", i18n$t("STR")),
      # ),
    hr(),
    htmlOutput("AbilityRoll")
  )
)
