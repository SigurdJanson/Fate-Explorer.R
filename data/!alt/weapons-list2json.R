library(jsonlite)

# Read jsx file to convert to json
Text <- scan("./data/weapon-list.jsx", what = "", skip = 1,
             quote = NULL, sep = NULL, encoding = "UTF-8")

# JSX is not JSON - tranform plain text to make it fit
NameTags <- grep(".*:$", Text)
Names <- unique(Text[NameTags])
Names <- substr(Names, 1, nchar(Names)-1)
Text[NameTags] <- paste0("\"", Names, "\":")

Text[length(Text)] <- "]}"
Text <- c("{\"Weapons\": [", Text)
Text <- paste0(Text, collapse = "")

# Convert numbers
json <- parse_json(Text, simplifyVector = TRUE) # returns a data frame
for(i in 1:length(json$Weapons)) {
  if (names(json$Weapons[i]) %in% c("schwelle", "bonus", "at", "pa", "bf"))
    json$Weapons[[i]] <- as.integer(json$Weapons[[i]])
  if (names(json$Weapons[i]) %in% c("preis"))
    json$Weapons[[i]] <- as.numeric(json$Weapons[[i]])
}
# Handle ampty values correctly
json$Weapons$leiteigenschaft[which(json$Weapons$leiteigenschaft == "-")] <- NA

# Add column with primary attributes in code
Translate <- function(x) ifelse(!is.na(x), Mapping[match(x, Mapping$shortname), "attrID"], NA)
PrimeAttr <- strsplit(json$Weapons$leiteigenschaft, "/") # get them
Mapping   <- read_json("./data/attributes_de.json", simplifyVector = TRUE)
PrimeAttr <- lapply(PrimeAttr, Translate)
PrimeAttr <- lapply(PrimeAttr, function(x, ...) ifelse(!anyNA(x), paste(x, ...), NA), collapse="/" )
json$Weapons$primeattrID <- unlist(PrimeAttr)
  
# Code for combat techn.
Ct <- read_json("./data/combattechs_de.json", simplifyVector = FALSE, flatten = TRUE)
Ct <- as.data.frame(Ct, stringsAsFactors = FALSE)
json$Weapons$combattechID <- colnames(Ct)[match(json$Weapons$technik, Ct[1, ])]

write_json(json$Weapons, "./data/weapon-list.json") 

# Test!
json <- read_json("./data/weapon-list.json", simplifyVector = TRUE)

rm(Text, Names, NameTags, Ct, json, i, Translate, Mapping, PrimeAttr)



