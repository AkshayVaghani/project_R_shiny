library(leaflet.extras)
library(shiny)
library(ggplot2)
library(data.table)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(DT)
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(shiny)
library(googleVis)
library(leaflet)
library(plotly)

library(leaflet,quietly=TRUE)
library(maps,quietly=TRUE)






# convert matrix to dataframe
state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
rownames(state_stat) <- NULL
# create variable with colnames as choice
choice <- colnames(state_stat)[-1]

######################################################################################
######################################################################################

library(data.table)
yelp_insp_data <- fread(file = "./yelp_insp.csv")
yelp_insp_data <- as.data.frame(yelp_insp_data)

# create variable with colnames as choice
choice1 <- colnames(yelp_insp_data)[-1]


######################################################################################

# create vector of "All" and list of boro
choice2 <- c('ALL',unique(yelp_insp_data$boro))

#create vector of "All" and list of top 15 restaurants ( base on number of restaurants in nyc)
topcuisine <- yelp_insp_data %>% group_by(cuisine) %>% summarise(Count=n()) %>% top_n(15, Count)
choice3 <- c('ALL',unique(topcuisine$cuisine)) 


# create variable(data frame) subsets to use in inspection/review plots

count.boro <- yelp_insp_data %>% group_by(boro,grade) %>% summarise(Count=n())

count.cuisine <- yelp_insp_data %>% group_by(cuisine,grade) %>% summarise(Count=n())

rev.count.cuisine <- yelp_insp_data %>% group_by(cuisine,ratings1) %>% summarise(Count=n())

rev.count.boro <- yelp_insp_data %>% group_by(boro,ratings1) %>% summarise(Count=n())

