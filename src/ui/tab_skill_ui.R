
sidebarLayout(
  sidebarPanel(
    checkboxInput("SkillIgnore", i18n$t("Ignore skill, just throw"), TRUE),
    conditionalPanel(condition = "input.SkillIgnore == 0",
                     hr(),
                     sliderInput("SkillTrait1", i18n$t("1st Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     sliderInput("SkillTrait2", i18n$t("2nd Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     sliderInput("SkillTrait3", i18n$t("3rd Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     hr(),
                     sliderInput("SkillValue", i18n$t("Skill"), min = 0L, max = 20L, value = 4L, step = 1L),
                     div(style="float:right",
                         conditionalPanel(condition = "input.SkillMod < 0", 
                                          icon("minus-circle"), i18n$t("Impediment")),
                         conditionalPanel(condition = "input.SkillMod > 0", 
                                          icon("plus-circle"), i18n$t("Advantage"))
                     ),
                     sliderInput("SkillMod", i18n$t("Modifier"), min = -10L, max = 10L, value = 0L, step = 1L)
    ),
  ),
  mainPanel(
    actionButton("doSkillThrow", i18n$t("Now!")),
    hr(),
    div(tableOutput("SkillThrow"), style = "font-size:140%")
  )
)
