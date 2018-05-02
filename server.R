# shiny app server 

shinyServer(function(input, output,session){
  
##### plot inspection grade by boro    
  reactboro=reactive({
    count.boro %>% 
      filter_(ifelse(input$boro1=="ALL",'boro %in% unique(count.boro$boro)','boro==input$boro1')) %>%
      group_by(boro,grade) %>%
      summarise(Total=sum(Count))
  })
  
  output$plotboro=renderPlotly({
    plot_ly(data=reactboro(), x = ~factor(boro, levels=c('QUEENS','BROOKLYN','MANHATTAN','BRONX','STATEN ISLAND')), 
            y =~Total, type = 'bar', color=~grade) %>% 
      layout(xaxis = list(title = "Boro", showticklabels = TRUE),
             yaxis = list(title = "Number of inspection score"), title = "Inspection grade by boro", showlegend = TRUE)
  })
  

##### plot of inspection grade by cuisine    
  reactcuisine=reactive({
    count.cuisine %>% 
      filter_(ifelse(input$cuisine1=="ALL",'cuisine %in% unique(count.cuisine$cuisine)','cuisine==input$cuisine1')) %>%
      group_by(cuisine,grade) %>%
      summarise(Total=sum(Count))
  })
  
  m <- list(
    l = 120,
    r = 70,
    b = 50,
    t = 50,
    pad = 4
  )
  
  output$plotcuisine=renderPlotly({
    plot_ly(data=reactcuisine(), x=~Total,y = ~factor(cuisine), 
            type = 'bar', color=~grade) %>% 
      layout(xaxis = list(title = "Number of inspection score", showticklabels = TRUE),
             yaxis = list(title = ""),title = "Inspection grade by cuisine",autosize = T, margin=m,showlegend = TRUE)
  })
  
  
##### plot of review by boro    
  reactboro_rev=reactive({
    rev.count.boro %>% 
      filter_(ifelse(input$boro2=="ALL",'boro %in% unique(rev.count.boro$boro)','boro==input$boro2')) %>%
      group_by(boro,ratings1) %>%
      summarise(Total=sum(Count))
  })
  
  output$plotboro_rev=renderPlotly({
    plot_ly(data=reactboro_rev(), x = ~factor(boro, levels=c('QUEENS','BROOKLYN','MANHATTAN','BRONX','STATEN ISLAND')), 
            y =~Total, type = 'bar', color=~ratings1) %>% 
      layout(xaxis = list(title = "Boro", showticklabels = TRUE),
             yaxis = list(title = "Number of Reviews"),title = "Reviews by borough", showlegend = TRUE)
  })       
  
##### plot of review by cuisine    
  reactcuisine_rev=reactive({
    rev.count.cuisine %>% 
      filter_(ifelse(input$cuisine2=="ALL",'cuisine %in% unique(rev.count.cuisine$cuisine)','cuisine==input$cuisine2')) %>%
      group_by(cuisine,ratings1) %>%
      summarise(Total=sum(Count))
  })
  
  output$plotcuisine_rev=renderPlotly({
    plot_ly(data=reactcuisine_rev(), x=~Total,y = ~factor(cuisine), 
            type = 'bar', color=~ratings1) %>% 
      layout(xaxis = list(title = "Number of Reviews", showticklabels = TRUE),
             yaxis = list(title = ""),title = "Reviews by cuisine", autosize = T,margin=m,showlegend = TRUE)
  })
  
 
# scattered plot to compare ratings and inspection score
  pal <- c("red","blue","green","orange","yellow")
  pal <- setNames(pal, c("BRONX", "BROOKLYN", "MANHATTAN","STATEN ISLAND","QUEENS"))
  
  output$rev_insp_scat= renderPlotly({plot_ly(data = rev.insp.data,type = 'scatter', x = ~rating,
                                              y = ~score, color = ~boro, colors = pal,
                                              text = paste("Name: ",rev.insp.data$name,  
                                                           "<br>cuisine: ",rev.insp.data$cuisine, 
                                                           "<br>grade: ",rev.insp.data$grade),
                                              hoverinfo = 'text',
                                              mode = 'markers') %>% 
                                              layout(xaxis = list(title = "Reviews", showticklabels = TRUE),
                                                    yaxis = list(title = "Inspection scores"),title = "Scattered plot",
                                                    showlegend = TRUE)
  })
      
  
# density plots
  p <- ggplot(rev.insp.data, aes(rating, fill = boro)) +
    geom_density(alpha = 0.5, position = "stack") 
  
  output$rev_insp_dens= renderPlotly({ggplotly(p) %>% layout(autosize = T,margin=m, width = 700, height = 450,title = "Density plot", showlegend=TRUE) 
  })
  
  
################################################################################################
# CLUSTER MAP ############
  output$map1 = renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      setView(-73.945242, 40.710610, 11)
  })
  
###PURPOSE: take in a data frame and assigns colors to map icons based ratings
###OUTPUT: Strings (of various colors)
###NOTE: Function had to be unnamed because of interaction between addingAwesomeMarkers and named vectors
  getColor = function(df) {
    unname(sapply(df$ratings1, function(revi) {
      if(revi == "*" | revi=="**") {
        "red"
      } 
      else if(revi == "***") {
        "orange"
      } 
      else if(revi == "****") {
        "blue"
      } 
      else if(revi == "*****") {
        "green"
      }
      else {
        "black"
      }
    }))
  }
  
  
###### ADD CLUSTER MARKERS ###########
  filtered_map = yelp_insp_data
#Check if any of the cluster map filters or borough layers have been triggered
  observeEvent(c(input$boro_layer, input$boro_map, input$cuisine_map), {
#Filter the data base on condition
    if(input$boro_map != "ALL") {
      filtered_map = filtered_map %>% filter(boro == input$boro_map)
    }
    if(input$cuisine_map != "ALL") {
      filtered_map = filtered_map %>% filter(cuisine == input$cuisine_map)
    }
    
#Initialize icons, calling getColor to find the proper color
    icons = awesomeIcons(
      icon = "ion-alert-circled",
      library = "ion",
      markerColor = getColor(filtered_map)
    )
    
#Add markers based on the filtered data
    leafletProxy("map1", data = filtered_map) %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~coordinates.longitude,~coordinates.latitude, icon = icons,
                        clusterOptions = markerClusterOptions(),
                        popup=paste("Name: ",filtered_map$name,
                                    "<br>cuisine: ",filtered_map$cuisine,
                                    "<br>grade: ",filtered_map$grade,
                                    "<br>ratings1: ",filtered_map$ratings1,
                                    "<br>address: ",filtered_map$location.address1,
                                    "<br>boro: " ,filtered_map$boro)) %>%
                                    {ifelse(input$boro_layer,
                                            leafletProxy("map1") %>%
                                              addPolygons(data=boro_layer,
                                                          color = topo.colors(5,alpha = NULL),
                                                          fillColor = topo.colors(5,alpha = NULL),
                                                          smoothFactor = .5,
                                                          layerId = LETTERS[1:6]),
                                            leafletProxy("map1") %>% removeShape(layerId = LETTERS[1:6]))}
    
  })
  
#Check to see if an address was put into the search bar
  observeEvent(c(input$search), {
    if(input$location != "") {
      loc = geocode(input$location)
      leafletProxy("map1") %>%
        setView(loc$lon,loc$lat,17)
    }
  })
  
###############################################################################################
##### HEAT MAP #################
  output$heat = renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(-73.945242, 40.710610, 11) %>% 
      addPolygons(data = boro_layer,
                  stroke = FALSE, 
                  smoothFactor = 0.5)
  })
  
### ADDING OF HEAT INDICATORS 
#filtered data base on boro and cuisine for heat map
  filtered_map=yelp_insp_data
#See if any of the heat map filters were triggered
  observeEvent(c(input$cuisine_heat,input$boro_heat), {
#Filter the data accordingly
    if(input$boro_heat != "ALL") {
      filtered_map = filtered_map %>% filter(.,boro == input$boro_heat)
    }
    if(input$cuisine_heat != "ALL") {
      filtered_map = filtered_map %>% filter(.,cuisine == input$cuisine_heat)
    }
    
#Add heat indicators based on the filtered data
    leafletProxy("heat", data = filtered_map) %>%
      clearWebGLHeatmap() %>%
      addWebGLHeatmap(lng=~coordinates.longitude, lat=~coordinates.latitude,
                      size=15, units = "px", alphaRange = .5)
  })
  
#Check to see if an address was put into the search bar
  observeEvent(c(input$search_heat), {
    if(input$location_heat != "") {
      loc = geocode(input$location_heat)
      leafletProxy("heat") %>%
        setView(loc$lon,loc$lat,17)
    }
  })
  
###############################################################################################
################################## PROVIDING CHOICES FOR SELECTIZEINPUT ##################################
  ##### INPUT FILTERS FOR THE TABLE#####
  updateSelectizeInput(session, "boro_tb", choices = unique(yelp_insp_data$boro), server = TRUE)
  updateSelectizeInput(session, "cuisine_tb", choices = unique(yelp_insp_data$cuisine), server = TRUE)
  updateSelectizeInput(session, "score_tb", choices = unique(yelp_insp_data$grade), server = TRUE)
  updateSelectizeInput(session, "rating_tb", choices = unique(yelp_insp_data$rating), server = TRUE)
  
  
################################## FILTERING OF THE DATATABLE ##################################
  filtered_data = yelp_insp_data
  
  data_filter = reactive({
    if(length(input$boro_tb)) {
      filtered_data = filtered_data %>% filter(.,boro == input$boro_tb)
    }
    if(length(input$cuisine_tb)) {
      filtered_data = filtered_data %>% filter(.,cuisine == input$cuisine_tb)
    }
    if(length(input$score_tb)) {
      filtered_data = filtered_data %>% filter(.,grade== input$score_tb)
    }
    if(length(input$rating_tb)) {
      filtered_data = filtered_data %>% filter(.,rating == input$rating_tb)
    }
    return(filtered_data)
  })
  
################################################################################################
# show data using DataTable
  output$table1 <- DT::renderDataTable({
    datatable(data_filter(), rownames=FALSE, options=list(scrollX=TRUE)) %>% 
      formatStyle(input$selected1, background="skyblue", fontWeight='bold')
  })
  
  
})