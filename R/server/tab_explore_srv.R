# EXPLORE TAB


# Display result of skill roll
output$imgSkillChances <- renderPlot({

  # Fetch values
  if (input$rdbSkillSource == "ManualSkill") {
    # Fetch the values directly from the input controls
    TraitVals <- c(input$SkillTrait1, input$SkillTrait2, input$SkillTrait3)
    Chances   <- ChancesOfSkill( Abilities = TraitVals,
                                 Skill = input$SkillValue,
                                 Modifier = input$SkillMod )
    PlotTitle <- paste0("[", paste0(TraitVals-input$SkillMod, collapse = ", "), "], ",
                       input$SkillValue)

  } else if (input$rdbSkillSource == "CharSkill") {
    # Fetch the values from the active skill set
    req(input$lbCharSkills)
    Skill <- input$lbCharSkills
    SkillSource <- ActiveSkillSets$GetSkillSet(Ident = ActiveSkillIdent)
    SkillIndex  <- SkillSource$GetSkillIndex(ActiveSkillIdent)
    TraitVals   <- SkillSource$GetAbilities(SkillIndex)
    SkillValue  <- SkillSource$GetSkillValues(SkillIndex, 0) # returns abilities AND skill
    SkillValue  <- SkillValue[length(SkillValue)] # extract skill, omit abilities

    Chances   <- ChancesOfSkill( Abilities = TraitVals,
                                 Skill = SkillValue,
                                 Modifier = input$SkillMod )
    PlotTitle <- Skill
  } else return()

  # Plot labels
  Chances$Type  <- grepl("QL", Chances[["Names"]])
  Chances[["Names"]] <- c(i18n$t(Chances[["Names"]][1:4]), paste0(i18n$t("QL"), 1:6))
  # Plot
  ggplot(data = Chances, aes(x = reorder(Names, 1:10), y = Chance, fill=Type)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("darkgray", "lightgray")) +
    guides(fill = FALSE) +
    xlab(i18n$t("Result")) + ylab(i18n$t("Probability")) +
    ggtitle(paste(i18n$t("Chances of"), PlotTitle))+
    geom_text(aes(label = round(Chance, 2), y = 0.05), vjust=0, color="black", size=3.5)+
    theme_minimal()
})


# Display result of combat rolls
output$imgAttackChances <- renderPlot({
  Value    <- input$inpAttackValue
  Mod      <- input$inpCombatMod
  DmgCount <- input$inpDamageDieCount
  DmgSides <- 6
  DmgMod   <- input$inpDamage

  Chances <- ChancesOfAttack(Value = Value, Modifier = Mod,
                             DmgDieCount = DmgCount,
                             DmgDieSides = DmgSides, DmgMod = input$inpDamage)
  if (input$chbPredefinedWeapon) {
    PlotTitle <- as.character(input$cmbCombatSelectWeapon)
  } else {
    PlotTitle <- paste0("[", Value-Mod, "] Damage ",
                        DmgCount, "d", DmgSides, "+", DmgMod)
  }

  Chances[["HitPoints"]][1:2] <- i18n$t(Chances[["HitPoints"]][1:2])

  # Plot
  ggplot(data = Chances, aes(x = reorder(HitPoints, 1:nrow(Chances)), y = TotalChance)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("gray")) +
    guides(fill = FALSE) +
    xlab(i18n$t("Result")) + ylab(i18n$t("Probability")) +
    ggtitle(paste(i18n$t("Chances of"), PlotTitle))+
    geom_text(aes(label = round(TotalChance, 2), y = 0.05), vjust=0, color="black", size=3.5)+
    theme_minimal()
})
