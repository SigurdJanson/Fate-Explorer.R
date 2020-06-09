setwd("..")
source("./src/explorechances.R")
setwd("./test")


test_that("dSumOfDice", {
  # sum(`2d6`) - https://wizardofodds.com/gambling/dice/2/
  e <- c(0.027777777777778, 0.055555555555556, 0.083333333333333, 
         0.111111111111111, 0.138888888888889, 0.166666666666667, 
         0.138888888888889, 0.111111111111111, 0.083333333333333, 
         0.055555555555556, 0.027777777777778)
  Total <- 0
  for (p in 2:12) {
    o <- dSumOfDice(p, 2, 6)
    expect_equal(o, e[p-1])
    Total <- Total + o
  }
  expect_equal(Total, 1)
  
  # sum(`3d6`) - https://wizardofodds.com/gambling/dice/2/
  e <- c(0.004629629629630, 0.013888888888889, 0.027777777777778,
         0.046296296296296, 0.069444444444444, 0.097222222222222,
         0.115740740740741, 0.125000000000000, 0.125000000000000,
         0.115740740740741, 0.097222222222222, 0.069444444444444,
         0.046296296296296, 0.027777777777778, 0.013888888888889,
         0.004629629629630)
  
  Total <- 0
  for (p in 3:18) {
    o <- dSumOfDice(p, 3, 6)
    expect_equal(o, e[p-2])
    Total <- Total + o
  }
  expect_equal(Total, 1)

  # sum(`10d6`) - https://wizardofodds.com/gambling/dice/2/
  e <- c(0.000000016538172, 0.072692805974699, 0.000000016538172)
  for (i in 1:3) {
    p <- c(10, 35, 60)[i]
    o <- dSumOfDice(p, 10, 6)
    expect_equal(o, e[i])
  }

  # sum(`20d6`) - https://en.calc-site.com/probabilities/dice_total
  e <- c(2.7351112277913E-16, 5.4299917045329E-9, 0.051818590196609, 
         5.4299917045329E-9, 2.7351112277913E-16)
  for (i in 1:3) {
    p <- c(20, 30, 70, 110, 120)[i]
    o <- dSumOfDice(p, 20, 6)
    expect_equal(o, e[i])
  }
  
  # Symmetry tests
  for (n in c(2^1:4, 20)) { # number of dice
    for (s in c(3, 6, 20)) { # number of sides
      MinP <- n
      MaxP <- n*s
      expect_equal(dSumOfDice(MinP, n, s), dSumOfDice(MaxP, n, s), info = paste(n, s))
    }
  }
})




test_that("ChancesOfAttack", {
  # Sum test - the sum of all probabilities must be 1
  for (v in 1:20) { # AT values
    for (m in -5:5) { # modifiers
      for (d in 1:3) { # number of dice
        for (s in c(6, 20)) { # sides
          Result <- ChancesOfAttack(v, m, d, s, DmgMod = 0)
          o <- sum(Result$TotalChance)
          e <- 1
          expect_equal(o, e)
        }
      }
    }
  }
  
  Result <- ChancesOfAttack(10, 0, 1, 6, DmgMod = 0)
  o <- as.integer(Result[["HitPoints"]][-(1:2)])
  e <- c(1L:6L, 8L, 10L, 12L)
  expect_identical(o, e)
  
  Result <- ChancesOfAttack(10, 0, 1, 6, DmgMod = 5)
  o <- as.integer(Result[["HitPoints"]][-(1:2)])
  e <- c(6L:11L, 12L, 14L, 16L, 18L, 20L, 22L)
  expect_identical(o, e)
  
})




test_that("ChancesOfDefense", {
  # Symmetry test: if the value is 10, critical == botch 
  for (m in -3:3) { # Modifier
    Result <- ChancesOfDefense(10-m, m)
    o <- Result[which(Result[[1]] == "Fumble"), "TotalChance"]
    e <- Result[which(Result[[1]] == "Critical"), "TotalChance"]
    expect_equal(o, e)
    o <- Result[which(Result[[1]] == "Fail"), "TotalChance"]
    e <- Result[which(Result[[1]] == "Success"), "TotalChance"]
    expect_equal(o, e)
  }
})




test_that("ChancesOfSkill", {
  ChancesOfSkill.bf <- function( Abilities = c(10, 10, 10), Skill = 0, Modifier = 0 ) {
    # setup and precondition checks
    if(any(Abilities + Modifier <= 0)) 
      stop("Cannot roll against effective skill < 0")
    
    MaxD <- 20
    MaxQS <- 6
    Rolls <- 3
    if(length(Abilities) != Rolls) stop("3 Eigenschaftswerte erforderlich")
    
    # Fumbles and criticals can be computed directly
    Fumbles <- (1 + 3 * 19) # 1 if all three dice equal 20; (3*19) if only 2 dice do.
    Criticals <- (1 + 3 * 19) # same logic here
    
    # initialise values
    EffectiveAbilities <- Abilities + Modifier
    Success <- 0
    Fail <- 0
    QS <- rep(0, MaxQS)
    
    # Brute 
    for( d1 in 1:MaxD )
      for( d2 in 1:MaxD )
        for( d3 in 1:MaxD ) {
          Roll <- c(d1, d2, d3)
          Check <- pmax(Roll, EffectiveAbilities)
          
          # if critical (2 or 3 ones) or normal success
          if(sum(Roll == 1) > 1  || sum(Check) <= sum(EffectiveAbilities) + Skill) {
            Success <- Success + 1
            if(sum(Roll == 1) > 1) CurrentQS <- ceiling(Skill / 3)
            else CurrentQS <- ceiling((Skill - sum(Check - EffectiveAbilities)) / 3)
            CurrentQS <- max(CurrentQS, 1) # correct for 0
            QS[CurrentQS] <- QS[CurrentQS] +1
          } else { 
            Fail <- Fail + 1
          }
          
        } #loop
    
    if(Success + Fail != 20^3)
      stop(paste("Algorithm error: ", Success, " -- ", Fail))
    
    Result <- c( Success, Fail, QS, Criticals, Fumbles ) / MaxD^3
    names(Result) <- c("Success", "Fail", paste0("QS", 1:MaxQS), "Critical", "Fumble")
    Result
  }

  
  Names <- c("Fumble", "Fail", "Success", "Critical", paste0("QL", 1:6))
  
  o <- ChancesOfSkill(Abilities = c(15, 15, 12), Skill=5, Modifier=-2)
  e <- c(0.00725, round(1-0.52675, digits = 10)-0.00725, 0.52675-0.00725, 0.00725, 
         0.25925, 0.26750, 0, 0, 0, 0)
  expect_equal(o[["Chance"]], e)
  expect_equal(sum(o[["Chance"]][1:4]), 1)
  expect_equal(sum(o[["Chance"]][c(1:2, 5:10)]), 1)
  expect_equal(o[["Names"]], Names)
  
  o <- ChancesOfSkill(Abilities = c(13, 14, 12), Skill=2, Modifier=1)
  e <- c(0.00725, round(1-0.49475, digits = 10)-0.00725, 0.49475-0.00725, 0.00725, 
         0.49475, 0, 0, 0, 0, 0)
  expect_equal(o[["Chance"]], e)
  expect_equal(sum(o[["Chance"]][1:4]), 1)
  expect_equal(sum(o[["Chance"]][c(1:2, 5:10)]), 1)
  expect_equal(o[["Names"]], Names)
  
  o <- ChancesOfSkill(Abilities = c(9, 2, 12), Skill = 0, Modifier = 0)
  e <- c(0.00725, round(1-0.031625-0.00725, digits = 10), 0.031625-0.00725, 0.00725, 
         0.031625, 0, 0, 0, 0, 0)
  expect_equal(o[["Chance"]], e)
  expect_equal(sum(o[["Chance"]][1:4]), 1)
  expect_equal(sum(o[["Chance"]][c(1:2, 5:10)]), 1)
  expect_equal(o[["Names"]], Names)
  
  o <- ChancesOfSkill(Abilities = c(9, 2, 12), Skill = 15, Modifier = 1)
  e <- c(0.00725, round(1-0.692125-0.00725, digits = 10), 0.692125-0.00725, 0.00725, 
         0.19975, 0.1490, 0.134125, 0.10400, 0.10525, 0)
  expect_equal(o[["Chance"]], e)
  expect_equal(sum(o[["Chance"]][1:4]), 1)
  expect_equal(sum(o[["Chance"]][c(1:2, 5:10)]), 1)
  expect_equal(o[["Names"]], Names)
  
  o <- ChancesOfSkill(Abilities = c(1, 20, 1), Skill = 0, Modifier = 0)
  e <- c(0.00725, round(1-0.00725-0.00725, digits = 10), 0.00725-0.00725, 0.00725, 
         0.00725, 0, 0, 0, 0, 0)
  expect_equal(o[["Chance"]], e)
  expect_equal(sum(o[["Chance"]][1:4]), 1)
  expect_equal(sum(o[["Chance"]][c(1:2, 5:10)]), 1)
  expect_equal(o[["Names"]], Names)
})
