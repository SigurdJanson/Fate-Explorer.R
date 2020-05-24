output$imgAboutShield <- renderImage({
  filename <- normalizePath(file.path("./img", paste("sunnyscimitars.jpg", sep="")))
  list(src = filename, alt = paste("Shield with crossing scimitars holding a sun"),
       width = "100%")
}, deleteFile = FALSE)

output$imgAboutGods <- renderImage({
  filename <- normalizePath(file.path("./img", "twelvegods.jpg", sep=""))
  list(src = filename, alt = paste("Shield with crossing scimitars holding a sun"),
       width = "100%")
}, deleteFile = FALSE)