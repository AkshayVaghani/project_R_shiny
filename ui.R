# UI for shiny app

shinyUI(dashboardPage(skin='green',
                      dashboardHeader(title = "NYC Restaurents inspection scores and reviews", titleWidth = 800),
                      dashboardSidebar(
                        width = 250,
                        sidebarUserPanel("AKSHAY VAGHANI", 'laltaki@gmail.com',
                                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
                        sidebarMenu(
                          
                          menuItem("Overview", icon = icon("bar-chart"),
                                   menuSubItem('inspection grade by boro',tabName='boro',icon = icon("bar-chart")),
                                   menuSubItem('inspection grade by cuisine',tabName='cuisine',icon = icon("bar-chart")),
                                   menuSubItem('reviews by boro',tabName='boro_rev',icon = icon("bar-chart")),
                                   menuSubItem('reviews by cuisine',tabName='cuisine_rev',icon = icon("bar-chart")),
                                   menuSubItem('reviews vs inspection score',tabName='rev_insp_scat',icon = icon("bar-chart"))),
                          menuItem("Map1", tabName = "map1", icon = icon("map")),
                          menuItem("HeatMap",tabName = "heatmap", icon = icon('fire')),
                          menuItem("Data1", tabName = "data1", icon = icon("database"))
                        )
                      ),
                      
                      dashboardBody(
                        #Creation of CSS custom edits (altering header text)
                        tags$head(
                          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                        ),
                        
                        # menu items 
                        tabItems(
                          
                          # inspection grade by boro
                          tabItem(tabName='boro',
                                  # title header
                                  h2('resraurant inspection grade by boro'),
                                  # filter restaurents by boro
                                  fluidRow(box(selectizeInput("boro1","Boro", 
                                                              borolist, selected='ALL'), width=10)),
                                  fluidRow(box(plotlyOutput("plotboro"), width=10))),
                          
                          # inspection grade by cuisine
                          tabItem(tabName='cuisine',
                                  h2('resraurant inspection grade by cuisine'),
                                  fluidRow(box(selectizeInput("cuisine1","Cuisine", 
                                                              cuisinelist, selected='ALL'), width=10)),
                                  fluidRow(box(plotlyOutput("plotcuisine"), width=10))),
                          
                          #review by boro
                          tabItem(tabName='boro_rev',
                                  h2('resraurant reviews by boro'),
                                  fluidRow(box(selectizeInput("boro2","Boro", 
                                                              borolist, selected='ALL'), width=10)),
                                  fluidRow(box(plotlyOutput("plotboro_rev"), width=10))),
                          
                          # review by cuisine
                          tabItem(tabName='cuisine_rev',
                                  h2('resraurant reviews by cuisine'),
                                  fluidRow(box(selectizeInput("cuisine2","Cuisine", 
                                                              cuisinelist, selected='ALL'), width=10)),
                                  fluidRow(box(plotlyOutput("plotcuisine_rev"), width=10))),
                          
                          # scatter plot for reviews and inspection score
                          tabItem(tabName='rev_insp_scat',
                                  h2('resraurant reviews and inspection scatter'),
                                  fluidRow(box(plotlyOutput("rev_insp_scat"), height = "150%",width='100%'))),
                          
                          
                          # restaurents on map
                          tabItem(tabName = "map1",
                                  #Creation of data filter panel for the cluster map
                                  # absolutePanel(id = "controls", class = "panel panel-default",
                                  #               draggable = TRUE, top = 0, right = "auto", bottom = "auto",
                                  #               width = 330, height = "auto", style = "padding: 8px; opacity: 0.92; background: #f7f6fc;",
                                  #               h2("Data Filter"),
                                  #               selectInput("cuisine_map", label = "Select a Cuisine:", cuisinelist),
                                  #               selectInput("boro_map", label = "Select a Borough:", borolist),
                                  #               textInput("location", label = "Type an address below:", value = ""),
                                  #               actionButton("search", label = "Find Address"),
                                  #               checkboxInput("boro_layer", "Show Boroughs", value = FALSE)),
                                  
                                  # fluidRow(box(leafletOutput("map1",height = "900", width="100%"),height =1000,width = 100)),
                                  fluidRow(
                                    absolutePanel(id = "controls", class = "panel panel-default",
                                                  draggable = TRUE, top = 0, right = "auto", bottom = "auto", fixed=TRUE,
                                                  width = 330, height = "auto", #style = "padding: 8px; opacity: 0.92; background: #f7f6fc; z-index: 1000;",
                                                  h2("Data Filter"),
                                                  selectInput("cuisine_map", label = "Select a Cuisine:", cuisinelist),
                                                  selectInput("boro_map", label = "Select a Borough:", borolist),
                                                  textInput("location", label = "Type an address below:", value = ""),
                                                  actionButton("search", label = "Find Address"),
                                                  checkboxInput("boro_layer", "Show Boroughs", value = FALSE)),
                                    leafletOutput("map1",height = "900", width="100%")

                                  )
                                  
                                  
                                  

                          ),
                          
                          # heat map base on reviews
                          # heat map base on inspection score
                          # indicate the location wise eviews or inspection score
                          
                          tabItem(tabName = "heatmap",
                                  #Creation and outputting of heat map
                                  leafletOutput("heat", height = "900", width="100%"),
                                  
                                  #Creation of data filter panel for the heat map
                                  absolutePanel(id = "controls", class = "panel panel-default",
                                                draggable = TRUE, top = 160, right = "auto", bottom = "auto",
                                                width = "300", height = "auto", style = "padding: 8px; opacity: 0.92; background: #f7f6fc;",
                                                h2("Data Filter"),
                                                selectInput("cuisine_heat", label = h3("Select Cuisine:"), cuisinelist),
                                                selectInput("boro_heat", label = h3("Select a Borough:"), borolist),
                                                textInput("location_heat", label = h3("Type an address below:"), value = ""),
                                                actionButton("search_heat", label = "Find Address")
                                  )
                          ),
                          
                          
                          
                          # data table
                          
                          #Tab to view the entire datatable
                          tabItem(tabName = "data1",
                                  #A row of filters for the datatable
                                  fluidRow(column(3,selectizeInput("boro_tb",label="Pick a Borough:",choices=NULL, multiple = TRUE)),
                                           column(3,selectizeInput("cuisine_tb", label="Pick a cuisine:",choices=NULL, multiple = TRUE)),
                                           column(3,selectizeInput("score_tb", label="Pick a inspection score:", choices = NULL, multiple = TRUE)),
                                           column(3,selectizeInput("rating_tb", label="Pick a rating:", choices = NULL, multiple = TRUE)),
                                           dataTableOutput("table1")))
                          
                          
                        )
                        
                        
                      )
))