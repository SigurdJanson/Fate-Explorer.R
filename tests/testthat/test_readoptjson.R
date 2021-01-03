library(testthat)
library(jsonlite)


.testdir <- getwd()
  setwd("../../R")
  .srcdir <- getwd()
  source("./rules.R")
  source("./readoptjson.R")
setwd(.testdir)


GetCharacter <- function(Filename) {
  .oridir <- getwd()
  setwd(.testdir)
  Filename <- file.path("../character", Filename)
  CharacterData <- read_json(Filename) 
  #Wipfelglanz <- read_json("./tests/character/Layariel Wipfelglanz+UniqueWeapons.json", simplifyVector = TRUE) 
    
  setwd(.oridir)
  return(CharacterData)
}


GetBelongings <- function(Filename) {
  CharacterData <- GetCharacter(Filename) 

  # Retrieve the items as required by "GetWeapons_Opt()"
  Things <- CharacterData[["belongings"]][["items"]]
  return(Things)
}


GetUniqueWeapon <- function(Which) {
  Things <- GetBelongings("TestUniqueWeapons_Wipfelglanz_20200102.json")
  
  # Find the requested items
  Index <- which(sapply(Things, function(x) x$name == Which))
  Item   <- Things[[Index]]
  return(Item)
}



# UniqueWeaponFromJson ----
test_that("UniqueWeaponFromJson: Without template", {
  
  # UNIQUE ITEM WITHOUT TEMPLATE: Feuerdolch ==
  Item <- GetUniqueWeapon("Feuerdolch (Unique 1)")
  setwd(.srcdir)
  o <- UniqueWeaponFromCharacter(Item)
  setwd(.testdir)

  expect_identical(o$name, Item$name)
  expect_identical(o$combattech, "Dolche")
  expect_identical(o$primeattrID, "ATTR_6")
  expect_identical(o$primeattr, "GE")
  expect_identical(o$threshold, 16L)
  expect_identical(o$damage, "1W6")
  expect_identical(o$bonus, 6L)
  expect_identical(o$range, "kurz")
  expect_identical(o$clsrng, TRUE)
  expect_identical(o$improvised, FALSE)
  expect_identical(o$templateID, "")
  

  # UNIQUE ITEM WITH TEMPLATE & UNIQUE PRIMARY ABILITY: Schneiderschere ==
  # This item is supposed to be used as improvised weapon (while the next isn't)
  Item <- GetUniqueWeapon("Schneiderschere, scharf")
  setwd(.srcdir)
  o <- UniqueWeaponFromCharacter(Item)
  setwd(.testdir)
  
  expect_identical(o$name, Item$name)
  expect_identical(o$combattech, "Dolche")
  expect_identical(o$primeattrID, ("ATTR_6/ATTR_8"))
  expect_identical(o$primeattr, ("GE/KK"))
  expect_identical(o$threshold, 16L)
  expect_identical(o$damage, "1W6")
  expect_identical(o$bonus, 1L)
  expect_identical(o$clsrng, TRUE)
  expect_identical(o$improvised, TRUE)
  expect_identical(o$templateID, "ITEMTPL_589")

  # Actually, this should not be imported as improvised weapon
  Item <- GetUniqueWeapon("Schneiderschere, stumpf")
  setwd(.srcdir)
  o <- UniqueWeaponFromCharacter(Item)
  setwd(.testdir)
  
  expect_identical(o$name, Item$name)
  expect_identical(o$combattech, "Dolche")
  expect_identical(o$primeattrID, ("ATTR_6/ATTR_8"))
  expect_identical(o$primeattr, ("GE/KK"))
  expect_identical(o$threshold, 16L)
  expect_identical(o$damage, "1W6")
  expect_identical(o$bonus, 1L)
  expect_identical(o$clsrng, TRUE)
  expect_identical(o$improvised, FALSE)
  expect_identical(o$templateID, "ITEMTPL_589")
})



test_that("UniqueWeaponFromJson: With template", {
  
  # UNIQUE ITEM WITH TEMPLATE: Winddolch ==
  Item <- GetUniqueWeapon("Winddolch (Unique 2)")
  setwd(.srcdir)
  o <- UniqueWeaponFromCharacter(Item)
  setwd(.testdir)
  
  expect_identical(o$name, Item$name)
  expect_identical(o$combattech, "Dolche")
  expect_identical(o$primeattrID, "ATTR_6")
  expect_identical(o$primeattr, "GE")
  expect_identical(o$threshold, 16L)
  expect_identical(o$damage, "1W6")
  expect_identical(o$bonus, 3L)
  expect_identical(o$clsrng, TRUE)
  expect_identical(o$templateID, "ITEMTPL_716")
})




test_that("UniqueWeaponFromJson: Ranged", {
  
  # UNIQUE ITEM WITH TEMPLATE: Elfenbogen ==
  Item <- GetUniqueWeapon("Elfenbogen")
  setwd(.srcdir)
  o <- UniqueWeaponFromCharacter(Item)
  setwd(.testdir)
  
  expect_identical(o$name, Item$name)
  expect_identical(o$combattech, "Bögen")
  expect_identical(o$primeattrID, "ATTR_5")
  expect_identical(o$primeattr, "FF")
  expect_identical(o$damage, "1W6")
  expect_identical(o$bonus, 5L)
  expect_identical(o$clsrng, FALSE)
  expect_identical(o$templateID, "ITEMTPL_62")
})



# GetAbilities_Opt() ----

test_that("GetAbilities_Opt", {
  Abilities <- list(list(id = "ATTR_1", value = 11L), list(id = "ATTR_2", value = 10L), 
                    list(id = "ATTR_3", value = 15L), list(id = "ATTR_4", value = 13L), 
                    list(id = "ATTR_5", value = 14L), list(id = "ATTR_6", value = 15L), 
                    list(id = "ATTR_7", value = 11L), list(id = "ATTR_8", value = 11L))
  o <- GetAbilities_Opt(Abilities)
  expect_identical(nrow(o), 1L)
  expect_identical(ncol(o), 8L)
  expect_identical(colnames(o), paste0("ATTR_", 1:8))
})



# GetWeapons_Opt() ----

test_that("GetWeapons_Opt: With unique weapons", {
  # ITEMS WITH and WITHOUT TEMPLATES
  Data <- GetCharacter("TestUniqueWeapons_Wipfelglanz_20200102.json")
  Belongings   <- Data[["belongings"]][["items"]]
  Abilities    <- GetAbilities_Opt(Data[["attr"]][["values"]])
  CombatSkills <- Data[["ct"]]

  setwd(.srcdir)
  Weapons <- GetWeapons_Opt(Belongings, CombatSkills, Abilities, 
                            AddUnarmed = FALSE, AddImprov = FALSE)
  setwd(.testdir)
  
  expect_identical(nrow(Weapons$Melee), 5L)
  expect_identical(sort(rownames(Weapons$Melee)), 
                   sort(c("Dolch", "Elfendolch", 
                          "Feuerdolch (Unique 1)", 
                          "Winddolch (Unique 2)",
                          "Schneiderschere, stumpf")))
  expect_identical(nrow(Weapons$Ranged), 1L)
  expect_identical(sort(rownames(Weapons$Ranged)), 
                   sort(c("Elfenbogen")))
  
  #
  # Including improvised Weapons
  setwd(.srcdir)
  Weapons <- GetWeapons_Opt(Belongings, CombatSkills, Abilities, 
                            AddUnarmed = FALSE, AddImprov = TRUE)
  setwd(.testdir)
  
  #-print(is.data.frame(Weapons))
  expect_identical(nrow(Weapons$Melee), 6L)
  expect_identical(sort(rownames(Weapons$Melee)), 
                   sort(c("Dolch", "Elfendolch", 
                          "Feuerdolch (Unique 1)", 
                          "Winddolch (Unique 2)",
                          "Schneiderschere, scharf", 
                          "Schneiderschere, stumpf")))
  expect_identical(nrow(Weapons$Ranged), 1L)
  expect_identical(sort(rownames(Weapons$Ranged)), 
                   sort(c("Elfenbogen")))
  
  
  #
  # Including unarmed AND improvised Weapons
  setwd(.srcdir)
  Weapons <- GetWeapons_Opt(Belongings, CombatSkills, Abilities, 
                            AddUnarmed = TRUE, AddImprov = TRUE)
  setwd(.testdir)
  
#-print(is.data.frame(Weapons))
  expect_identical(nrow(Weapons$Melee), 7L)
  expect_identical(sort(rownames(Weapons$Melee)), 
                   sort(c("Dolch", "Elfendolch", 
                          "Feuerdolch (Unique 1)", 
                          "Winddolch (Unique 2)",
                          "Schneiderschere, scharf", 
                          "Schneiderschere, stumpf",
                          "Waffenlos")))
  expect_identical(nrow(Weapons$Ranged), 1L)
  expect_identical(sort(rownames(Weapons$Ranged)), 
                   sort(c("Elfenbogen")))
})



test_that("GetWeapons_Opt: NO unique weapons", {
  Data <- GetCharacter("Junis Djelef ibn Yakuban Al Thani_20200517.json")
  Belongings   <- Data[["belongings"]][["items"]]
  Abilities    <- GetAbilities_Opt(Data[["attr"]][["values"]])
  CombatSkills <- Data[["ct"]]
  
  setwd(.srcdir)
  Weapons <- GetWeapons_Opt(Belongings, CombatSkills, Abilities, 
                            AddUnarmed = FALSE, AddImprov = FALSE)
  setwd(.testdir)
  
  ExpectedWeapons <- c("Waqqif", "Speer", "Magierstab, kurz")
  expect_identical(nrow(Weapons$Melee), length(ExpectedWeapons))
  expect_identical(sort(rownames(Weapons$Melee)), 
                   sort(ExpectedWeapons))
  expect_null(Weapons$Ranged)
  
  #
  # Including improvised Weapons
  setwd(.srcdir)
  Weapons <- GetWeapons_Opt(Belongings, CombatSkills, Abilities, 
                            AddUnarmed = FALSE, AddImprov = TRUE)
  setwd(.testdir)

  ExpectedWeapons <- c("Waqqif", "Speer", "Magierstab, kurz", "Federmesser")
  expect_identical(nrow(Weapons$Melee), length(ExpectedWeapons))
  expect_identical(sort(rownames(Weapons$Melee)), 
                   sort(ExpectedWeapons))
  expect_null(Weapons$Ranged)
  
  
  #
  # Including unarmed AND improvised Weapons
  setwd(.srcdir)
  Weapons <- GetWeapons_Opt(Belongings, CombatSkills, Abilities, 
                            AddUnarmed = TRUE, AddImprov = TRUE)
  setwd(.testdir)
  
  ExpectedWeapons <- c("Waffenlos", "Waqqif", "Speer", "Magierstab, kurz", "Federmesser")
  expect_identical(nrow(Weapons$Melee), length(ExpectedWeapons))
  expect_identical(sort(rownames(Weapons$Melee)), 
                   sort(ExpectedWeapons))
  expect_null(Weapons$Ranged)
  
})


