
sidebarLayout(
  sidebarPanel(
    CreateSkillSourceRadioButton(),

    # manually chosen skill values
    conditionalPanel(condition = "input.rdbSkillSource == 'ManualSkill'",
                     sliderInput("SkillTrait1", i18n$t("1st Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     sliderInput("SkillTrait2", i18n$t("2nd Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     sliderInput("SkillTrait3", i18n$t("3rd Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
                     hr(),
                     sliderInput("SkillValue", i18n$t("Skill"), min = 0L, max = 20L, value = 4L, step = 1L),
                     hr()),
    
    # Skills loaded from character sheet
    conditionalPanel(condition = "input.rdbSkillSource == 'CharSkill'",
                     selectInput("lbSkillClasses", NULL, NULL, selectize = FALSE, width = "100%"),
                     selectInput("lbCharSkills", NULL, NULL, selectize = FALSE, width = "100%", size = 14)
    ),
    
    
    # Modifier in case we roll against values
    conditionalPanel(condition = "input.rdbSkillSource == 'CharSkill' || input.rdbSkillSource == 'ManualSkill'",
                     hr(),
                     div(style="float:right",
                         conditionalPanel(condition = "input.SkillMod < 0", 
                                          gicon("minus-circle"), i18n$t("Impediment")),
                         conditionalPanel(condition = "input.SkillMod > 0", 
                                          gicon("plus-circle"), i18n$t("Advantage"))
                     ),
                     sliderInput("SkillMod", i18n$t("Modifier"), min = -10L, max = 10L, value = 0L, step = 1L)
    ),
  ),

  mainPanel(
    actionButton("doSkillRoll", span(i18n$t("Now!"), id="lbldoSkillRoll"), icon = gicon("fist"), 
                 width = "49%", style = "font-size: 140%"),
    htmlOutput("uiDoSkillRoutine", inline = TRUE), #Button for routine checks
    hr(),
    div(
        htmlOutput("SkillRoll", style = "font-size:140%"),
        style = "height:18rem"
    ),
    # Exploration Panel for Skill roll probabilities ----
    conditionalPanel(
      condition = "output.ShowExploreSkillChances",
      h3(i18n$t("Skill Roll")),
      plotOutput("imgSkillChances", width = "100%", height = "200px")
    )
  )
)
