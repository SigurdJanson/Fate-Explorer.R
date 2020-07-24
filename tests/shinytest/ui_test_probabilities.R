library(shinytest)
library(webdriver)
library(xml2)
library(rvest)
library(ggplot2)
# Verify probabilites

app <- ShinyDriver$new(path = "../../R") #, seed = 1

df <- data.frame(roll = 1:20, count = rep(0, 20))
totalcount <- 5E3
pvals <- numeric()
chivals <- numeric()

for (i in 1:totalcount) {
  app$setInputs(doAbilityRoll = "click")
  
  app$waitForValue("AbilityRoll", ignore = list(NULL, ""), iotype = "output")
  
  output <- xml2::read_html( app$getValue(name = "AbilityRoll") )
  text <- html_text(html_node(output, ".dr"))
  KeyValue <- as.integer(text)

  df$count[KeyValue] <- df$count[KeyValue] +1
  
  if (i %% 200 == 0) {
    cat(".")
    result <- chisq.test(df$count, rescale.p = TRUE)
    pvals <- c(pvals, result[["p.value"]])
    chivals <- c(chivals, result[["statistic"]][["X-squared"]])
  }
}

ggplot(data = df, aes(x = roll, y = count)) +
  geom_bar(stat = "identity")

ddf <- df
ddf$e <- rep(sum(ddf$count)/20, 20)
chisq.test(ddf[c("count", "e")], rescale.p = TRUE) 
chisq.test(ddf[c("count", "e")], simulate.p.value = TRUE) 

ggplot(data.frame(pvals, samples = seq(200, 200*length(pvals), 200)), aes(x = samples, y = pvals)) +
  geom_line()

# Shiny-App stoppen
app$stop()
