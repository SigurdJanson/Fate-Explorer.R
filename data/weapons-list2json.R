library(jsonlite)
Text <- scan("./data/weapon-list.jsx", what = "", skip = 1,
             quote = NULL, sep = NULL, encoding = "UTF-8")
NameTags <- grep(".*:$", Text)
Names <- unique(Text[NameTags])
Names <- substr(Names, 1, nchar(Names)-1)
Text[NameTags] <- paste0("\"", substr(Names, 1, length(Names)-1), "\":")

Text[length(Text)] <- "]}"
Text <- c("{\"Weapons\": [", Text)
Text <- paste0(Text, collapse = "")

#json <- parse_json(Text)
write_json(Text, "./data/weapon-list.json")

rm(Text, Names, NameTags)