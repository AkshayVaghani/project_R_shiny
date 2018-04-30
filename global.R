library(leaflet.extras)
library(shiny)
library(ggplot2)
library(data.table)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(googleVis)
library(data.table)

library(leaflet,quietly=TRUE)
library(maps,quietly=TRUE)

library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
library(DT)
library(rgdal)
library(plotly)
library(lubridate)
library(dplyr)
library(ggmap)
library(leaflet.extras)
library(stringr)

# 
# 
# # convert matrix to dataframe
# state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# # remove row names
# rownames(state_stat) <- NULL
# # create variable with colnames as choice
# choice <- colnames(state_stat)[-1]

######################################################################################
######################################################################################
# read data file
yelp_insp_data <- fread(file = "./yelp_insp.csv")
yelp_insp_data <- as.data.frame(yelp_insp_data)

boro_layer <- readOGR(path.expand("/Users/akshay/Desktop/Project_1_shinyApp/shinyDashBoard/nybb_18a"), "nybb")
#Open and transform the shape file for the boroughs
boro_layer <- spTransform(boro_layer, CRS("+proj=longlat +datum=WGS84"))

# create variable with colnames as choice
#choice1 <- colnames(yelp_insp_data)[-1]


######################################################################################

# create vector of "All" and list of boro
borolist <- c('ALL',unique(yelp_insp_data$boro))

#create vector of "All" and list of top 15 restaurants ( base on number of restaurants in nyc)
topcuisine <- yelp_insp_data %>% group_by(cuisine) %>% summarise(Count=n()) %>% top_n(15, Count)
cuisinelist <- c('ALL',unique(topcuisine$cuisine)) 


# create variable(data frame) subsets to use in inspection/review plots
count.boro <- yelp_insp_data %>% group_by(boro,grade) %>% summarise(Count=n())

count.cuisine <- yelp_insp_data %>% group_by(cuisine,grade) %>% summarise(Count=n())

rev.count.cuisine <- yelp_insp_data %>% group_by(cuisine,ratings1) %>% summarise(Count=n())

rev.count.boro <- yelp_insp_data %>% group_by(boro,ratings1) %>% summarise(Count=n())

rev.insp.data <- yelp_insp_data %>% select(boro,score,grade,rating,ratings1)

