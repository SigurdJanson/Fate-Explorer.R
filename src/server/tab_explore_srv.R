# EXPLORE TAB



# Display result of skill roll
output$imgProbabilities <- renderPlot({
  if (input$rdbSkillSource == "ManualSkill") {
    TraitVals <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
    Chances   <- ChancesOfSkill( Abilities = TraitVals, 
                                 Skill = input$SkillValue, 
                                 Modifier = input$SkillMod )

  } else if (input$rdbSkillSource == "CharSkill") {
    req(input$lbCharSkills)
    Skill <- input$lbCharSkills
    SkillIndex <- which(Character$Skills$name == Skill)
    TraitVals <- unlist(Character$Skills[SkillIndex, paste0("ab", 1:3)]) # IDs
    TraitVals <- unlist(Character$Attr[, TraitVals]) # Values
    
    Chances   <- ChancesOfSkill( Abilities = TraitVals, 
                                 Skill = Character$Skills[SkillIndex, "value"], 
                                 Modifier = input$SkillMod )
  }
  Chances$Type  <- grepl("QS", Chances[["Names"]])
    
  ggplot(data = Chances, aes(x = reorder(Names, 1:10), y = Chance, fill=Type)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("black", "orange")) +
    guides(fill = FALSE) +
    xlab(i18n$t("Result")) + ylab(i18n$t("Probability")) +
    ggtitle(i18n$t("Chances of a Skill Roll"))
    #geom_col(aes(fill = supp), width = 0.7)
})

