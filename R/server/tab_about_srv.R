output$imgAboutMagic <- renderImage({
  filename <- normalizePath(file.path("./img", paste("fe_magic.jpg", sep="")))
  list(src = filename, alt = paste("Sorcerer throwing a magic ball"),
       width = "60%")
}, deleteFile = FALSE)

output$imgAboutSkeletons <- renderImage({
  filename <- normalizePath(file.path("./img", "fe_skeletons.jpg", sep=""))
  list(src = filename, alt = paste("Skeletons in awe"),
       width = "60%")
}, deleteFile = FALSE)

output$imgAboutBarbarian <- renderImage({
  filename <- normalizePath(file.path("./img", "fe_barbarian.jpg", sep=""))
  list(src = filename, alt = paste("Barbarian doing the gangsta walk"),
       width = "60%")
}, deleteFile = FALSE)

output$imgAboutBat <- renderImage({
  filename <- normalizePath(file.path("./img", "fe_bat.jpg", sep=""))
  list(src = filename, alt = paste("Flying bat"),
       width = "60%")
}, deleteFile = FALSE)