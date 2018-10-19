library(shiny)
library(rvest) 
library(tidyverse)
library(ggmap) 
library(DT)
library(leaflet)
library(shinythemes)

options(DT.options = list(pageLength = 5, order = list(2, "desc"),
                          lengthMenu = list(c(5, 10, -1), c('5', '15', 'All'))))

ui <- fluidPage(theme = shinytheme("yeti"),
  
  leafletOutput("map"),
  
  hr(),
  
  tabsetPanel(
    tabPanel("NFL",
             dataTableOutput("nfl_table")),
    tabPanel("NBA",
             dataTableOutput("nba_table")),
    tabPanel("MLB",
             dataTableOutput("mlb_table"))
  )
)

server <- function(input, output) {
  
  mlb <- readRDS(file = "mlb.rds")
  nfl <- readRDS(file = "nfl.rds")
  nba <- readRDS(file = "nba.rds")
  
  find_center <- function(df){
    
    df2 <- df %>%
      mutate(rad_lon = df[,"lon"]*pi/180, rad_lat = df[,"lat"]*pi/180) %>% 
      mutate(X = cos(rad_lat) * cos(rad_lon)) %>%
      mutate(Y = cos(rad_lat) * sin(rad_lon)) %>%
      mutate(Z = sin(rad_lat)) %>%
      summarise(X = mean(X), Y = mean(Y), Z = mean(Z)) %>% #find mean
      mutate(Lon = atan2(Y,X), Hyp = sqrt(X*X+Y*Y), Lat = atan2(Z, Hyp)) %>%  
      select(Lon, Lat) %>%
      mutate(Lon = Lon*180/pi, Lat = Lat*180/pi)
    
    return(df2)
  }
  
  #locate center of gravity for each league
  nfl_center <- find_center(nfl)
  nba_center <- find_center(nba)
  mlb_center <- find_center(mlb)
  
  #find center of gravity after each year
  for (i in 1:nrow(nfl)) {
    nfl$lon_center[i] <- find_center(nfl[1:i,])[[1]]
    nfl$lat_center[i] <- find_center(nfl[1:i,])[[2]]
  }
  
  
  for (i in 1:nrow(nba)) {
    nba$lon_center[i] <- find_center(nba[1:i,])[[1]]
    nba$lat_center[i] <- find_center(nba[1:i,])[[2]]
  }
  
  for (i in 1:nrow(mlb)) {
    mlb$lon_center[i] <- find_center(mlb[1:i,])[[1]]
    mlb$lat_center[i] <- find_center(mlb[1:i,])[[2]]
  }
  
  nfl_total <- nfl %>%
    group_by(Winner) %>%
    select(Winner, lon, lat) %>%
    add_tally()
  
  mlb_total <- mlb %>%
    group_by(Winner) %>%
    select(Winner, lon, lat) %>%
    add_tally()
  
  nba_total <- nba %>%
    group_by(Winner) %>%
    select(Winner, lon, lat) %>%
    add_tally()
  
  manipulate_data <- function(df){
    df2 <- df %>%
      group_by(Winner) %>%
      rename(City = Winner) %>%
      summarise(Championships = n())
    
    return(df2)
  }
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(data = nfl_total, radius = ~n+3,  fillColor = "red",  stroke = FALSE, fillOpacity  = 0.2,
                       label = ~paste(Winner, ": ", n), group = "NFL") %>%
      addMarkers(data = nfl_center, popup =
                   ~paste0("Lombardi Trophy Center of Gravity"), group = "NFL") %>%
      addCircleMarkers(data = nba_total, radius = ~n+3, fillColor = "blue", label =
                         ~paste0(Winner,": ", n), group = "NBA",  stroke = FALSE, fillOpacity = 0.2) %>%
      addMarkers(data = nba_center, popup = 
                   ~paste0("Larry O'Brien Trophy Center of Gravity"), group = "NBA") %>%
      addCircleMarkers(data = mlb_total, radius = ~n+3, fillColor = "forestgreen", label =
                         ~paste0(Winner,": ", n), group = "MLB",  stroke = FALSE, fillOpacity = 0.2) %>%
      addMarkers(data = mlb_center, popup =
                   ~paste0("Commissioner's Trophy Center of Gravity"), group = "MLB") %>%
      addLayersControl(baseGroups = c("NFL", "NBA", "MLB"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("NBA", "MLB"))
  })
  
  output$nfl_table <- renderDataTable(
    manipulate_data(nfl)
  )
  
  output$nba_table <- renderDataTable(
    manipulate_data(nba)
  )
  
  output$mlb_table <- renderDataTable(
    manipulate_data(mlb)
  )
}

# Run the application 
shinyApp(ui,server)


