# library 
library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(googleVis)
library(data.table)
library(leaflet.extras)
library(rgdal)
library(ggmap)

######################################################################################
# read data file
yelp_insp_data <- fread(file = "./yelp_insp.csv")
yelp_insp_data <- as.data.frame(yelp_insp_data)

#boro_layer <- readOGR(path.expand("/Users/akshay/Desktop/Project_1_shinyApp/shinyDashBoard/nybb_18a"), "nybb")
boro_layer <- readOGR(path.expand("./nybb_18a"), "nybb")

#Open and transform the shape file for the boroughs
boro_layer <- spTransform(boro_layer, CRS("+proj=longlat +datum=WGS84"))

######################################################################################


# create variable(data frame) subsets to use in inspection/review plots
# filter the data frame to plot boro and num of counts for grades
count.boro <- yelp_insp_data %>% group_by(boro,grade) %>% summarise(Count=n())

# filter the data frame to plot boro and num of counts for reviews
rev.count.boro <- yelp_insp_data %>% group_by(boro,ratings1) %>% summarise(Count=n())

#create vector of "All" and list of top 15 restaurants ( base on number of restaurants in nyc)
#filter the data frame to plot cuisine and num of counts for grades
temp <- yelp_insp_data %>% group_by(cuisine,grade) %>% summarise(Count=n()) %>% mutate(sum1=sum(Count))
count.cuisine <- temp[order(-temp$sum1),][1:45,]

# filter the data frame to plot cuisine and num of counts for reviews
temp <- yelp_insp_data %>% group_by(cuisine,ratings1) %>% summarise(Count=n()) %>% mutate(sum1=sum(Count))
rev.count.cuisine <- temp[order(-temp$sum1),][1:60,]

# create vector of "All" and list of boro
borolist <- c('ALL',unique(yelp_insp_data$boro))

# vector for all the cuisine
cuisinelist <- c('ALL',unique(rev.count.cuisine$cuisine)) 


rev.insp.data <- yelp_insp_data %>% select(boro,score,grade,rating,ratings1)

