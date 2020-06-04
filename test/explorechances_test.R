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