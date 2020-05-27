
sidebarLayout(
  sidebarPanel(
    #checkboxInput("SkillIgnore", i18n$t("Ignore skill, just throw"), TRUE),
    radioGroupButtons(
      inputId = "rdbSkillSource",
      choices = c(`<i class='fa fa-question'> Unchecked</i>` = "NoSkill", 
                  `<i class='fa fa-sliders-h'> Manual</i>` = "ManualSkill", 
                  `<i class='fa fa-bars'> Character Skill</i>` = "CharSkill"),
      justified = TRUE
    ),
    
    # manually chosen skill values
    conditionalPanel(condition = "input.rdbSkillSource == 'ManualSkill'",
                     #hr(),
                     sliderInput("SkillTrait1", i18n$t("1st Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     sliderInput("SkillTrait2", i18n$t("2nd Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     sliderInput("SkillTrait3", i18n$t("3rd Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     hr(),
                     sliderInput("SkillValue", i18n$t("Skill"), min = 0L, max = 20L, value = 4L, step = 1L),
                     hr(),
                     div(style="float:right",
                         conditionalPanel(condition = "input.SkillMod < 0", 
                                          icon("minus-circle"), i18n$t("Impediment")),
                         conditionalPanel(condition = "input.SkillMod > 0", 
                                          icon("plus-circle"), i18n$t("Advantage"))
                     )),
    
    # Skills loaded from character sheet
    conditionalPanel(condition = "input.rdbSkillSource == 'CharSkill'",
                     #hr(),
                     selectInput("lbCharSkills", NULL, NULL, selectize = FALSE, width = "100%", size = 12)
    ),
    
    
    # Modifier in case we roll against values
    conditionalPanel(condition = "input.rdbSkillSource == 'CharSkill' || input.rdbSkillSource == 'ManualSkill'",
                     hr(),
                     sliderInput("SkillMod", i18n$t("Modifier"), min = -10L, max = 10L, value = 0L, step = 1L)
    ),
  ),
  mainPanel(
    actionButton("doSkillThrow", i18n$t("Now!")),
    hr(),
    div(tableOutput("SkillThrow"), style = "font-size:140%")
  )
)
