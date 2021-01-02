# SETUP

Character <- reactiveValues(Name = NULL,
                            Attr = NULL, Skills = NULL, 
                            Weapons = NULL, CombatSkills = NULL)
RawCharacterFile <- reactiveVal(NULL) # raw data container of json content


#' IsLocalhost
IsLocalhost <- function()
{
  return(session$clientData$url_hostname == "127.0.0.1")
}


#' LoadCharacterFile
#' @param PathToFile Path to the file that shall be loaded
LoadCharacterFile <- function(PathToFile)
{
###  # handle dependencies to components that display data of last character
###  Language <- ifelse(length(i18n$get_translation_language()) == 0L, "en", i18n$get_translation_language())
  tryCatch(
    Data <- read_json(path = PathToFile),
    error = function(e) stop(e)
  )

  RawCharacterFile(Data) # update raw data container
  
  Character$Name <- Data$name
  Character$Attr <- GetAbilities_Opt(Data[["attr"]][["values"]])
  Character$CombatSkills <- Data[["ct"]]
  SetupCharacterWeapons(input$chbShowImprovWeapons)
  SetupSkills()
  
  return(invisible(NULL))
}


#' SetupCharacterWeapons
#' Global function to extract the list of weapons from Json
#' @param AddImprov Shall improvised weapons be loaded as well? (`TRUE`/`FALSE`)
#' @return NULL
SetupCharacterWeapons <- function(AddImprov = FALSE) {
  Weapons <- GetWeapons_Opt(RawCharacterFile()[["belongings"]][["items"]], # belongings
                            Character$CombatSkills,  # combat techniques
                            Character$Attr, AddUnarmed = TRUE, AddImprov = AddImprov)
  Character$Weapons <- NULL
  WeaponNames <- NULL
  for (w in Weapons["templateID", ]) {
    if ( IsRangedWeapon(w) )
      ActiveWeapon <- RangedWeapon$new(w, Character$Attr, Character$CombatSkills)
    else
      ActiveWeapon <- MeleeWeapon$new(w, Character$Attr, Character$CombatSkills)

    Character$Weapons <- c(Character$Weapons, ActiveWeapon)
    WeaponNames <- c(WeaponNames, ActiveWeapon$Name)
    #TODO: Correct for encumbrance: TODO (EEC = Effective Encumbrance)
  }
  
  # Update dropdown list on Combat Tab
  updateSelectInput(session, "cmbCombatSelectWeapon", choices = WeaponNames, selected = 1)
  
  invisible(NULL)
}




#' SetupSkills
#' Global function to extract all skills except combat skills from Json and merge it with the 
#' skill values of the character.
#' @return NULL
SetupSkills <- function() {
  Data <- RawCharacterFile()[["talents"]]
  MundaneSkills <- SkillSet$new(1L, Data, Character$Attr)
  
  # Add magic skills if hero has them
  Data <- RawCharacterFile()[["spells"]]
  if (isTruthy(Data) && length(Data) > 0) {
    MagicSkills <- SkillSet$new(2L, Data, Character$Attr)
  } else {
    MagicSkills <- NULL
  }
  
  Data <- RawCharacterFile()[["liturgies"]]
  if (isTruthy(Data) && length(Data) > 0) {
    BlessedSkills <- SkillSet$new(3L, Data, Character$Attr)
  } else {
    BlessedSkills <- NULL
  }
  
  Character$Skills <- CharacterSkills$new(MundaneSkills, MagicSkills, BlessedSkills)
  
  # Update dropdown list on Skills Tab
  UpdateSkillSourceRadioButton(session, IsCharacterLoaded = TRUE )
  
  invisible(NULL)
}



#' Run this on start up to load a default file from the users home directory
observe({
  if (IsLocalhost())
  {
    isolate({
      Files <- list.files(path = Sys.getenv("HOME"), pattern = "\\.json$")
      if (isTruthy(Files))
      {
        FileIndex <- 1L
        Success <- FALSE
        while (FileIndex <= length(Files) && !Success) {
          Success <- TRUE
          tryCatch(
            LoadCharacterFile(file.path(Sys.getenv("HOME"), Files[FileIndex])),
            error = function(e) 
            { 
              Success <<- FALSE
              FileIndex <<- FileIndex+1 
            }
          )
        }
        if (Success)
          showNotification(paste(i18n$t("Character sheet"), Files[FileIndex], 
                                 i18n$t("has been found and loaded")))
      }
    })#isolate
  }
}) 



# SIDEBAR -------------------

# Open new character file (.json)
observeEvent(input$CharFile, {
  if (input$CharFile$type != "application/json") 
    RawCharacterFile(paste(i18n$t("Fate Explorer only understands Json files."), 
                                  input$CharFile$name, i18n$t("is not Json.")))
  LoadCharacterFile(input$CharFile$datapath)
  
  # THIS SECTION IS A BIT OUT OF PLACE HERE
  # Update Combat value with weaponless brawling
  updateSliderInput(session, "inpDodgeValue", value = Character$Weapons[[1]]$Skill$Dodge)
}, ignoreNULL = TRUE, ignoreInit = TRUE) # If new JSON file


# Show/Hide improvised weapons
observeEvent(input$chbShowImprovWeapons, {
  req(RawCharacterFile())
  SetupCharacterWeapons(input$chbShowImprovWeapons)
}, ignoreNULL = TRUE, ignoreInit = TRUE)


#' Checkbox: show the option to harvest weapon details from Ulisses Wiki
output$ShowHarvestWeaponDetails <- reactive({
  return( IsLocalhost() )
})
outputOptions(output, 'ShowHarvestWeaponDetails', suspendWhenHidden = FALSE)



# CHARACTER TITLE --------------------
# Name of hero / character
output$CharacterName <- renderPrint({
  if(isTruthy(Character$Name)) {
    Result <- Character$Name
  } else Result <- i18n$t("No character has been uploaded")
  cat(Result)
})



# Json Panel ------------------------
output$ShowSetupJson <- reactive({
  return( !is.null(RawCharacterFile()) )
})
outputOptions(output, 'ShowSetupJson', suspendWhenHidden = FALSE)

output$RawContents <- renderPrint({
  req(RawCharacterFile())
  print(RawCharacterFile())
})



# Attributes Panel ---------------------
output$ShowSetupAttr <- reactive({
  return( !is.null(Character$Attr) )
})
outputOptions(output, 'ShowSetupAttr', suspendWhenHidden = FALSE)

output$SetupAttr <- renderTable({
  Result <- Character$Attr
  # Show names not codes
  Language <- ifelse(length(i18n$get_translation_language()) == 0L, "en", i18n$get_translation_language())
  NameMapping <- GetAbilities(Language)
  colnames(Result) <- NameMapping[match(names(Result), NameMapping[["attrID"]]), "shortname"]
  
  Result
}, rownames = FALSE, na = "-", digits = 0L)



# Panel "States & Conditions" ---------------------
output$ShowSetupStatesNConds <- reactive({
  return( !is.null(Character$Attr) )
})
outputOptions(output, 'ShowSetupStatesNConds', suspendWhenHidden = FALSE)


output$SetupStatesNConds <- renderUI({
  tags$ul(
    tags$li(i18n$t("No states detected")),
    tags$li(i18n$t("No conditions detected"))
  )
})


# Skills Panels ---------------------
output$ShowSetupSkills <- reactive({
  return( isTruthy(Character$Skills) && 
          Character$Skills$HasTalent(.SkillType["Mundane"]) )
})
outputOptions(output, 'ShowSetupSkills', suspendWhenHidden = FALSE)


output$SetupSkills <- renderTable({
  Result <- Character$Skills$Sets$Mundane$Skills
  Result <- Result[-which(names(Result) == "attrID")]
  Result <- Result[-which(names(Result) == "classID")]
  # Show names not codes
  Language <- ifelse(length(i18n$get_translation_language()) == 0L, "en", i18n$get_translation_language())
  NameMapping <- GetAbilities(Language)
  for (ablty in paste0("ab", 1:3)) {
    Result[[ablty]] <- NameMapping[match(Result[[ablty]], NameMapping[["attrID"]]), "shortname"]
  }
  # Column names
  colnames(Result) <- c(i18n$t("Skill"), i18n$t("Set"), paste(i18n$t("SC"), 1:3), 
                        i18n$t("SR"), paste(i18n$t("Value"), 1:3))
  
  Result
}, rownames = FALSE, na = "-", digits = 0L, hover = TRUE)



output$ShowSetupSpells <- reactive({
  return( isTruthy(Character$Skills) && 
          Character$Skills$HasTalent(.SkillType["Magic"]) )
})
outputOptions(output, 'ShowSetupSpells', suspendWhenHidden = FALSE)


output$SetupSpells <- renderTable({
  Result <- Character$Skills$Sets$Magic$Skills
  Result <- Result[-which(names(Result) %in% c("url", "attrID", "class", "classID"))]

  # Show ability names not codes
  NameMapping <- GetAbilities()
  for (ablty in paste0("ab", 1:3)) {
    Result[[ablty]] <- NameMapping[match(Result[[ablty]], NameMapping[["attrID"]]), "shortname"]
  }
  # Column names
  colnames(Result) <- c(i18n$t("Skill"), i18n$t("Spell"), paste(i18n$t("SC"), 1:3), 
                        i18n$t("Modifier"), i18n$t("Property"), i18n$t("SR"),
                        paste(i18n$t("Value"), 1:3))
  
  return(Result)
}, rownames = FALSE, na = "-", digits = 0L, hover = TRUE)




output$ShowSetupChants <- reactive({
  return( isTruthy(Character$Skills) && 
            Character$Skills$HasTalent(.SkillType["Blessed"]) )
})
outputOptions(output, 'ShowSetupChants', suspendWhenHidden = FALSE)


output$SetupChants <- renderTable({
  Result <- Character$Skills$Sets$Blessed$Skills
  Result <- Result[-which(names(Result) %in% c("url", "attrID", "class", "classID", "tradition"))]
  
  # Show names not codes
  NameMapping <- GetAbilities()#Language
  for (ablty in paste0("ab", 1:3)) {
    Result[[ablty]] <- NameMapping[match(Result[[ablty]], NameMapping[["attrID"]]), "shortname"]
  }
  # Column names
  colnames(Result) <- c(i18n$t("Skill"), paste(i18n$t("SC"), 1:3), i18n$t("Form of Prayer"), 
                        i18n$t("Modifier"), i18n$t("SR"),
                        paste(i18n$t("Value"), 1:3))
  
  return(Result)
}, rownames = FALSE, na = "-", digits = 0L, hover = TRUE)




# Combat Panel ------------------------
output$SetupWeapons <- renderTable({
  req(Character$Weapons)
  WT <- data.frame(a = character(), b = character(), c = integer(), d = integer(), e = character(),
                   stringsAsFactors = FALSE)
  for (W in Character$Weapons) {
    WT <- rbind(WT, list(W$Name, i18n$t(names(W$Type)),
                         as.integer(W$Skill$Attack), as.integer(W$Skill$Parry),
                         sprintf(i18n$t("%dd%d+%d"), W$Damage$N, W$Damage$DP, W$Damage$Bonus)),
                stringsAsFactors = FALSE)
  }
  names(WT) <- i18n$t(c("Name", "Type", "AT", "PA", "Hit points"))
  return(WT)
}, rownames = TRUE, na = "-")

