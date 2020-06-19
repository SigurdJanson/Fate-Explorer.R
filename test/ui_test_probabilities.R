# Verify probabilites

library(shinytest)
library(webdriver)
library(ggplot2)

app <- ShinyDriver$new(path = "./src", seed = 1)

df <- data.frame(roll = 1:20, count = rep(0, 20))
for (i in 1:1E4) {
  app$setInputs(doAbilityRoll = "click")
  output <- app$getValue(name = "AbilityRoll")
  suppressWarnings(
    Numbers <- as.integer(unlist(strsplit(output, "\n")))
  )
  Numbers <- Numbers[!is.na(Numbers)] # remove NAs
  df$count[Numbers] <- df$count[Numbers] +1
  Sys.sleep(0.05)
}

ggplot(data = df, aes(x = roll, y = count)) + 
  geom_bar(stat = "identity")


# Shiny-App stoppen
app$stop()
