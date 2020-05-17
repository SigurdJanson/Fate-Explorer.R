
sidebarLayout(
  sidebarPanel(
    checkboxInput("SkillIgnore", "Ignore skill, just throw", TRUE),
    conditionalPanel(condition = "input.SkillIgnore == 0",
                     hr(),
                     sliderInput("SkillTrait1", "1st Trait", min = 1, max = 20, value = 11),
                     sliderInput("SkillTrait2", "2nd Trait", min = 1, max = 20, value = 11),
                     sliderInput("SkillTrait3", "3rd Trait", min = 1, max = 20, value = 11),
                     hr(),
                     sliderInput("SkillValue", "Skill", min = 0, max = 20, value = 4),
                     sliderInput("SkillMod", "Modifier", min = -10, max = 10, value = 0),
                     conditionalPanel(condition = "input.SkillMod < 0", helpText("Impediment")),
                     conditionalPanel(condition = "input.SkillMod > 0", helpText("Advantage"))
    ),
  ),
  mainPanel(
    actionButton("doSkillThrow", "Now!"),
    hr(),
    h3(textOutput("SkillThrow"))
  )
)
