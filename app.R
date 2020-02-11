#
# Amy Ngo - ango8
# Project 1 - No Time To Waste
#
library(rsconnect)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(lubridate)
library(stringr)
library(splitstackshape)

# ====================================================
# ========= Adjust raw dataframe to single tag litter, separate date and time, and real names =========

# read CSV file, separate by comma and header is TRUE - see environment tab
# data cells in tags column are switched to NA
litterdata <- read.csv(file = "p1excel.csv", sep = ",", header = TRUE,stringsAsFactors = FALSE)
litterdata$tags[litterdata$tags == ""] <- "untagged" 
# remove URL column
litterdata$url <- NULL
# change column names
colnames(litterdata) <- c("ChallengeId", "JoinId", "LitterId", "Hour", "Lat", "Long", "Tags", "UserId", "Username")

# ====================================================
# change longitude and latitude to character type
litterdata$Lat <- as.numeric(as.character(litterdata$Lat))
litterdata$Long <- as.numeric(as.character(litterdata$Long))

# ====================================================

# make csv file into data frame
dflitter <- data.frame(litterdata)

# remove rows containing a comma -- results in 3828 out of 8488 single tags of litter
#singleTags <- dflitter[!grepl(",", dflitter$Tags),]
singleTags <- cSplit(dflitter, "Tags", sep = ",", direction = "long")
singleTags <- as.data.frame(singleTags)

# ========= Splitting Data =========

# make separate dataframe after spliting the Timestamp in singleTags
dateAndTimeSplit <- data.frame(str_split_fixed(singleTags$Hour, " ", 2))
timeSplit <- data.frame(str_split_fixed(dateAndTimeSplit$X2, ":", 3)) # split time into Hour, Minute, Seconds

# change dateAndTimeSplit X1 date formate
#dateAndTimeSplit <- ymd_hms(singleTags$Hour)
#dateAndTimeSplit2 <- hour(ymd_hms(singleTags$Hour))

# ========= Adjust the Time and Date =========
#singleTags$Time <- dateAndTimeSplit$X2 # change Time column with the dataframe's split columns
singleTags$Date <- as.Date(dateAndTimeSplit$X1) # create new column
singleTags$Date <- dateAndTimeSplit$X1 # insert date column in the new column

singleTags$Hour <- timeSplit$X1

singleTags$Day <- wday(as.Date(singleTags$Date)) # day of the week column added into the data frame

# split singleTags table into usernames
usernameTable <- split(singleTags, singleTags$Username)

# remove all NA
singleTags <- na.omit(singleTags)

# ========= Varibles of Columns and Usernames Names =========

# take column header as list names
listColNames <- c(colnames(litterdata))
listColNamesGood <- listColNames[listColNames != "URL" & listColNames != "newDate"] # remove specific columns in variable

# find bad user names
badUN <- paste("litterati-", singleTags$UserId, sep = "")
# change usernames with User_# based on their UserId
for(i in 1:length(singleTags$Username)){
    if(badUN[i] == singleTags$Username[i]) {
        singleTags$Username[i] <- paste("User_", singleTags$UserId[i], sep ="")
    }
}

# take unique rows of usernames
listUsernames <- unique(singleTags$Username, incomparables = FALSE)
listUsernamesGood <- na.omit(listUsernames) # remove NA usernames

singleTags$Tags <- as.character(singleTags$Tags)

# take unique rows of litter
litterType <- unique(singleTags$Tags, incomparables = FALSE)

# final dataframe
allData <- singleTags

#test <- aggregate(allData, list(Date = allData$Date), sum)

# ====================================================

# descending order of litter count
dfLitterCount <- as.data.frame( sort(table(allData$Tags), decreasing = TRUE) )

# ====================================================

# Create shiny app
ui <- dashboardPage(
    dashboardHeader(title = "CS 424 Spring 2020 No Time To Waste"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     # URL items so then it's easier to reach on the big screen in classroom -- short people, ya' know?
                     sidebarMenu(
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Visualization Solution", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Data Used", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("GitHub", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Interests", tabName = "cheapBlankSpace", icon = NULL)),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     
                     # Added drop down menu
                     selectInput("User", "Select User to visualize", c("summary", listUsernamesGood), selected = "summary"),
                     selectInput("Litter", "Select litter type to visualize", c("summary", litterType), selected = "summary")
                     # ===================
    ),
    dashboardBody(
        fluidRow(
            column(2,
                   fluidRow(
                       box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
                           leafletOutput("leaf", height = 250)
                       )
                   ),
                   
                   
                   fluidRow(
                       box(title = "Video", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("vid", height = 250)
                       )
                   )
            ),
            
            column(8,
                   fluidRow(
                       box( title = "Each Day (April 2018 - January 2020)", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar1", height = 250)
                       )
                   ), 
                   fluidRow(
                       box( title = "Days Of The Week", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar2", height = 250)
                       )
                   ),
                   fluidRow(
                       box( title = "Hourly", solidHeader = TRUE, status = "primary", width = 12,
                            plotOutput("bar3", height = 250)
                       )
                   )
            ),
            column(2,
                   fluidRow(
                       box(title = "Top 10 Litter Collected", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("top10", height = 250)
                       )
                   ),
                   fluidRow(
                       box(title = "Most Litter Collected", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("mLc", height = 250)
                       )
                   )
            )
        )
    ))

server <- function(input, output) {
    
    # increase the default font size
    theme_set(theme_grey(base_size = 18)) 
    
    # calculate the values one time and re-use them in multiple charts to speed things up
    #justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year)})
    #newNoonsReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & Hour == 12)})
    #oneRoomNoonReactive <- reactive({subset(allData$input$Room, year(allData$newDate) == input$Year & Hour == 12)})
    
    # =======================
    
    # reactive function to return data if user changes the drop down menu selection
    reactiveFunc <- reactive({
        # both summary selection
        if(input$User == "summary" & input$Litter == "summary")
            return (allData)
        # specific user and summary litter type
        else if ( input$Litter == "summary")
            return (allData[allData$Username == input$User,])
        # summary users and specific litter
        else if (input$User == "summary")
            return (allData[allData$Tags == input$Litter,])
        # both specific selection
        else
            return (allData[allData$Username == input$User,]) & (allData[allData&Tags == input$Litter,])
    })
    
    # =======================
    
    # create hourly map
    output$bar3 <- renderPlot({
        #justOneYear <- justOneYearReactive()
        dataInfo <- reactiveFunc()
        ggplot(dataInfo, aes(x=dataInfo$Hour)) + 
            labs(x=paste("Hour of the Day")) +  geom_bar(position = "dodge")
    })
    
    
    # show all of the temperatures for a given room for a given year
    output$bar1 <- renderPlot({
        #justOneYear <- justOneYearReactive()
        dataInfo <- reactiveFunc()
        ggplot(dataInfo, aes(x=dataInfo$Date)) +
            labs(x=paste("Date")) + geom_bar(position = "dodge")
    })
    
    
    # show a line graph of the temperatures at noon for a given room for a given year
    output$bar2 <- renderPlot({
        #newNoons <- newNoonsReactive()
        dataInfo <- reactiveFunc()
        ggplot(dataInfo, aes(x=dataInfo$Day)) +
            labs(x=paste("Day of the Week")) + geom_bar()
    })
    
    
    # show a bar chart of the temperatures at noon for a given room for a given year
    #output$bar3 <- renderPlot({
    #newNoons <-  newNoonsReactive()
    #    dataInfo <- reactiveFunc()
    #    temperatures <- as.data.frame(table(dataInfo[,input$Room]))
    #    temperatures$Var1 <- as.numeric(as.character(temperatures$Var1))
    
    #    ggplot(temperatures, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
    #        labs(x="Temperature (F)", y = "Count") + xlim(60,90)
    #})
    
    
    # show box plot of the temperatures at noon for a given room for a given year
    output$mLc <- renderPlot({
        #newTemps2 <- justOneYearReactive()
        paste(dfLitterCount$Var1[1])
    })
    
    
    # use DT to help out with the tables - https://datatables.net/reference/option/
    output$tab1 <- DT::renderDataTable(
        DT::datatable({ 
            #newNoons <-  newNoonsReactive()
            dataInfo <- reactiveFunc()
            temperatures <- as.data.frame(table(dataInfo[,input$Litter], dnn = list("Litter")), responseName = "Count")
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
    )
    
    # add a leaflet map and put a marker on it at the location of the lab
    # while not overly useful this can ceratinly be expnded upon
    output$leaf <- renderLeaflet({
        dataInfo <- reactiveFunc()
        map <- leaflet()
        map <- addTiles(map)
        map <- setView(map, lng = dataInfo$Long[1], lat = dataInfo$Lat[1], zoom = 18)
        map <- addMarkers(map, lng = dataInfo$Long[1], lat = dataInfo$Lat[1], popup = paste("Long: ", dataInfo$Long[1], "Lat: ", dataInfo$Lat[1]))
        map
    })
    
    
}

shinyApp(ui = ui, server = server)