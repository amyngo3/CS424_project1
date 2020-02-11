library(shiny)
library(ggplot2)
library(leaflet)
library(lubridate)
library(stringr)

# read CSV file, separate by comma and header is TRUE - see environment tab
# data cells in tags column are switched to NA
litterdata <- read.csv(file = "p1excel.csv", sep = ",", 
                       header = TRUE, 
                       na.strings=c("","untagged"))
# change column names
colnames(litterdata) <- c("ChallengeId", "JoinId", "LitterId", "Timestamp","Lat", "Long","Tags", "URL", "UseId", "Username")
listNames <- c(colnames(litterdata))
#summary(litterdata)
# make csv file into data frame
dflitter <- data.frame(litterdata)
# remove rows containing a comma -- results in single tags of litter
singleTags <- dflitter[!grepl(",", dflitter$Tags),]

#unique(dflitter$tags, incomparables = FALSE)
#?unique

# ============= plotting data =============

# convert factor to string
#tagsToStr <- toString(dflitter$tags, width = NULL) #str_detect(strings, "bag", negate = FALSE)

# convert factor to character
tagsToChar <- as.character(dflitter$tags)

# Specific litter tables
plasticTable <- subset(dflitter, dflitter$tags == "plastic")
#table(plasticTable)
wrapperTable <- subset(dflitter, tags == "wrapper")
bagTable <- subset(dflitter, tags == "bag")
newspaperTable <- subset(dflitter, tags == "newspaper")
cigTable <- subset(dflitter, tags == "cigarette")
paperTable <- subset(dflitter, tags == "paper")
lidTable <- subset(dflitter, tags == "lid")
ptTable <- subset(dflitter, tags == "papertowel")
untaggedTable <- subset(dflitter, tags == "untagged") # used is.na(tags) to find the tags of untagged data cells
foilTable <- subset(dflitter, tags == "foil")

# make data.frames of specific subset tables
dfBagTable <- data.frame(bagTable)
print(dfBagTable)

# count the amount of single tag plastic
#a <- table(sum(str_count(plasticTable$tags, "plastic")))
#?split
split(dflitter, list(dflitter$username, dflitter$tags))

usernameTable <- split(litterdata, litterdata$Username)
tagTable <- split(dflitter, dflitter$Tags)


# sum of total plastic -- must use the specific table and pattern!!
plasticTotal <- sum(str_count(plasticTable$tags, "plastic"))
wrapperTotal <- sum(str_count(wrapperTable$tags, "wrapper"))
bagTotal <- sum(str_count(bagTable$tags, "bag"))
newspaperTotal <- sum(str_count(newspaperTable$tags, "newspaper"))
cigTotal <- sum(str_count(cigTable$tags, "cigarette"))
paperTotal <- sum(str_count(paperTable$tags, "paper"))
lidTotal <- sum(str_count(lidTable$tags, "lid"))
ptTotal <- sum(str_count(ptTable$tags, "papertowel")) # paper towel
# must fix!!
untagged <- sum(str_count(untaggedTable$tags, is.na(tags)))

plasBC <- ggplot(plasticTable, aes(tags, fill = username)) + geom_bar(position = "dodge") + labs(title = "Plastic Collected",
                                                                                       x = "Tag")

# ============= mapping =============

# = plotting map =
# Set value for the minZoom and maxZoom settings.
m <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))
m <- addTiles(m)
m <- addMarkers(m, lng = 174.768, lat = -36.852, popup = "The birthplace of R")
m

