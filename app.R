---
output: html_document
runtime: shiny
---
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages 
library(shiny)
library(png)
library(tidyverse)
library(sf)
library(zoo)
library(lubridate)

# Read in data and fix missing value column
arrests = read_delim("arrests_cleaned.csv", delim = ',')
arrests$PD_CD[is.na(arrests$PD_CD)] = "UNKNOWN"

# Read in shapefile of the NYC police precincts
sf = read_sf('.', 'nypp')

# Define UI for application that plots visualizations
ui <- fluidPage(

    # Application title
    titlePanel("NYPD Arrests Data Interactive Visualizations"),
    
    # Introductory lines
    mainPanel(
        p("Created by Darshan Patel"), 
        p("This R Shiny application will show a few example of interactive visualizations that can be shown publicly. 
        Furthermore, the plots may take a few seconds to load. Please be patient!"),
        p("Note: This is only a proof of concept and there are many improvements that can be made.")),
    
    # Create the map of arrests by year
    sidebarLayout(position = "right",
                  sidebarPanel("Pick a year:",
                               sliderInput(inputId = "x", 
                                           label = '', 
                                           min = 2015, 
                                           max = 2018, 
                                           value = 2015,
                                           sep= "",
                                           width = '200px')),
                   mainPanel(h3("Map of NYPD Arrests"),
              p("This map shows the location of all crimes that have occured in a single year. Use the slider on the right to pick the year of interest."))), 
    mainPanel(plotOutput("cityMap")),
    
    # Create the map of arrests by year and type
    sidebarLayout(position = "right",
                  sidebarPanel("Pick a crime:",
                               selectInput(inputId = "cMap",
                                           label = '',
                                           choices = as.list(sort(unique(arrests$PD_DESC))),
                                           width = '300px'), 
                               "Pick a year:", 
                               sliderInput(inputId = "y",
                                           label = '',
                                           min = 2015, 
                                           max = 2018,
                                           value = 2015,  
                                           sep = "",
                                           width = '200px')), 
                  mainPanel(h3("Map of Crimes"), 
              p("This map shows the location of a particular crime that have occured in a single year. Use the drop-box to select a crime of interest. Use the slider on the right to pick the year of interest."))), 
    mainPanel(plotOutput("crimeMap")),
    
    # Create a bar graph of arrests by month from 2015 to 2018
    sidebarLayout(position = "right",
                  sidebarPanel("Pick a crime:",
                               selectInput(inputId = "cBar",
                                           label = '',
                                           choices = as.list(sort(unique(arrests$PD_DESC))), 
                                           width = '300px')), 
                  mainPanel(h3("Trend of Crime"), 
              p("This bar graph shows the frequency of a crime that has occured by month from 2015 to 2018. Use the drop-box to select a crime of interest."))), 
    mainPanel(plotOutput("crimeBarPlot"))
)
    

# Define server logic required to draw the visualizations
server <- function(input, output) {

    # Creates the first map using coordinates of all crimes in a particular year
    # chosen by the user
    output$cityMap <- renderPlot({
        ggplot(sf) + 
            geom_sf(color = "black", fill = "white") +
            geom_point(data = arrests[year(arrests$ARREST_DATE) == input$x,], 
                       aes(x = X_COORD_CD, y = Y_COORD_CD),
                       size = 0.01, alpha = 0.1, color = "dodgerblue1") + 
            labs(x = "Longitude", y = "Latitude") + 
            coord_sf(crs = st_crs(2263)) + 
            theme_minimal() %>% print
    }, height = 400, width = 400)
    
    # Creates the second map using coordinates of a single crime in a particular year
    # chosen by the user
    output$crimeMap <- renderPlot({
        ggplot(sf) + 
            geom_sf(color = "blacK", fill = "white") + 
            geom_point(data = arrests[arrests$PD_DESC == input$cMap & 
                                          year(arrests$ARREST_DATE) == input$y,],
                       aes(x = X_COORD_CD, y = Y_COORD_CD),
                       size = 3, alpha = 1, color = "dodgerblue4") + 
            labs(x = "Longitude", y = "Latitude") + 
            coord_sf(crs = st_crs(2263)) + 
            theme_minimal() %>% print 
    }, height = 400, width = 400)
    
    # Creates the bar graph of occurrences of a single crime by month from 2015 to
    # 2018 where the crime is chosen by the user
    output$crimeBarPlot <- renderPlot({
        
        arrests[arrests$PD_DESC == input$cBar,] %>% 
            mutate(ym = as.yearmon(ARREST_DATE)) %>% 
            group_by(ym) %>% mutate(freq = n()) %>% 
            ggplot(aes(x = ym, y = freq)) + 
            geom_col(fill = "steelblue") + 
            scale_x_yearmon(format = "%Y") + 
            labs(x = "year", y = "number of arrests") + 
            theme_minimal() %>% print
    }, height = 400, width = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
