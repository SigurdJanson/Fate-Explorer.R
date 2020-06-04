# EXPLORE TAB



# Display result of skill roll
output$imgProbabilities <- renderPlot({
  if (input$rdbSkillSource == "ManualSkill") {
    TraitVals <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
    Chances   <- ChancesOfSkill( Abilities = TraitVals, 
                                 Skill = input$SkillValue, 
                                 Modifier = input$SkillMod )
    PlotTitle <- paste0("[", paste0(TraitVals-input$SkillMod, collapse = ", "), "], ",
                       input$SkillValue)

  } else if (input$rdbSkillSource == "CharSkill") {
    req(input$lbCharSkills)
    Skill <- input$lbCharSkills
    SkillIndex <- which(Character$Skills$name == Skill)
    TraitVals <- unlist(Character$Skills[SkillIndex, paste0("ab", 1:3)]) # IDs
    TraitVals <- unlist(Character$Attr[, TraitVals]) # Values
    
    Chances   <- ChancesOfSkill( Abilities = TraitVals, 
                                 Skill = Character$Skills[SkillIndex, "value"], 
                                 Modifier = input$SkillMod )
    PlotTitle <- Skill
  } else return()
  Chances$Type  <- grepl("QL", Chances[["Names"]])
  Chances[["Names"]] <- c(i18n$t(Chances[["Names"]][1:4]), paste0(i18n$t("QL"), 1:6))
    
  ggplot(data = Chances, aes(x = reorder(Names, 1:10), y = Chance, fill=Type)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("darkgray", "lightgray")) +
    guides(fill = FALSE) +
    xlab(i18n$t("Result")) + ylab(i18n$t("Probability")) +
    ggtitle(paste(i18n$t("Chances of"), PlotTitle))
})

