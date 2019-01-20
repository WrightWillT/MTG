########################################################################################################################
# LOAD PACKAGES
########################################################################################################################
library(jsonlite)
library(dplyr)
library(tidyr)

########################################################################################################################
# READING DATA
########################################################################################################################

mtgCardsRaw <- fromJSON("scryfall-all-cards.json")

########################################################################################################################
# EDA AND DATA CLEANSING
########################################################################################################################

# so what's in this dataset?
vars <- names(mtgCardsRaw)

for(i in 1: length(vars)){
  print(head(mtgCardsRaw[[vars[i]]],2))
}
# 62 lists; 233,514 entries

# how many are in english?
table(mtgCardsRaw[[6]])

# only 43,987 in english so let's subset to that
mtgCardsList <- mtgCardsRaw[which(mtgCardsRaw$lang=="en"),]

# subset to commander-legal only; 41,449 cards
mtgCardsList <- mtgCardsList[which(mtgCardsList$legalities$commander=="legal"),]

# most of these variables aren't needed for our analysis and, ideally, we want to export something manageable so
  # lets get rid of unneeded vars

# df to show data shape
#empty container df
dataStructure <- data.frame(vars, values = NA)

for(i in 1: nrow(dataStructure)){
  print(length(mtgCardsList[[i]][[1]]))
}

gameLengths <- vector()
for(i in 1:length(mtgCardsList$games)){
  gameLengths <- c(gameLengths, length(mtgCardsList$games[i]))
} 
table(gameLengths)


card <- 10000

for(i in 1: nrow(dataStructure)){
  if(length(mtgCardsList[[i]][[1]])==1) {
    dataStructure$values[i] <- mtgCardsList[[i]][[card]]
  }
}


mtgCards <- mtgCardsList[,which(vars %in% c(
  "id",             #unique id
  "name",           #unique card name
  "released_at",    #date of release
  "scryfall_uri",   #link to card details
  "layout",         #normal, split, flip, transform, etc.
  "mana_cost",      #needs to be parsed, but specifies exactly how much of each
  "cmc",            #converted mana cost
  "type_line",      #card type; has 2 properties usually
  "oracle_text",    #card abilities
  "power",
  "toughness",
  "edhrec_rank",    #populatrity on edhrec.com
  "loyalty",        #planeswalker life
  "life_modifier",  # if it's a a vanguard card, a modifier like "+2"
  "hand_modifier"   # if vanguard, a delta like "-1"
))]

# parse mana costs
mtgCards <- mtgCards %>%
  mutate(
    white_mana = ifelse(
        grepl("W",mana_cost),
        nchar(gsub("[^W]*(W)[^W]*","\\1",mana_cost, perl = TRUE)),
        0),
    blue_mana = ifelse(
        grepl("U", mana_cost),
        nchar(gsub("[^U]*(U)[^U]*","\\1",mana_cost, perl = TRUE)),
        0),
    green_mana = ifelse(
        grepl("G",mana_cost),
        nchar(gsub("[^G]*(G)[^G]*","\\1",mana_cost, perl = TRUE)),
        0),
    red_mana = ifelse(
        grepl("R", mana_cost),
        nchar(gsub("[^R]*(R)[^R]*","\\1",mana_cost, perl = TRUE)),
        0),
    black_mana = ifelse(
        grepl("B", mana_cost),
        nchar(gsub("[^B]*(B)[^B]*","\\1",mana_cost, perl = TRUE)),
        0),
    colorless_mana = ifelse(
        grepl("C", mana_cost),
        nchar(gsub("[^C]*(C)[^C]*","\\1",mana_cost, perl = TRUE)),
        0),
    generic_mana = ifelse(
        is.na(gsub("[^1-9]*([0-9]*).*$","\\1",mana_cost, perl = TRUE))==FALSE,
        as.integer(gsub("[^1-9]*([0-9]*).*$","\\1",mana_cost, perl = TRUE)),
        0)
  ) %>%
  select(-mana_cost)

mtgCards$generic_mana[which(is.na(mtgCards$generic_mana))] <- 0

# parse type and subtype into columns

mtgCards <- mtgCards %>%
  separate(type_line, c("type","subtype"), sep = " — ")

# flag for commander and put columns in reasonable order
mtgCards <- mtgCards %>%
  mutate(commanderFlag = grepl("Legendary", type)) %>%
  select(id, 
         name, 
         type, 
         subtype, 
         power, 
         toughness, 
         cmc,
         oracle_text,
         white_mana, 
         blue_mana, 
         green_mana, 
         red_mana,
         black_mana,
         colorless_mana,
         generic_mana,
         layout,
         edhrec_rank,
         loyalty,
         life_modifier,
         hand_modifier,
         commanderFlag,
         released_at,
         scryfall_uri
         )

# there are only 17,991 actual cards that aren't dupes so let's boil it down
uniqueCards <- unique(mtgCards$name)
dedupedCards <- mtgCards[!duplicated(mtgCards$name),]

# export to .csv for analysis
write.csv(dedupedCards, "MTG_all-cards_basic-info.csv", row.names = FALSE)




