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
library(rgdal)
library(tidyverse)
library(sf)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(wesanderson)

# Read in data and fix missing value column
arrests = read_delim("arrests_cleaned.csv", delim = ',')
arrests$PD_CD[is.na(arrests$PD_CD)] = "UNKNOWN"

# Read in shapefile of the NYC police precincts
sf = read_sf('.', 'nypp')
sf_2 = readOGR('.', 'nypp')

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
    mainPanel(h3("Yearly Arrests Made by the NYPD"),
              p("This map shows the location of all crimes that have occured in a single year. Use the slider on the right to pick the year of interest.")),
    sidebarLayout(
        position = "right",
        sidebarPanel(
            "Pick a year:",
            sliderInput(
                inputId = "x", 
                    label = '', 
                    min = 2015, 
                    max = 2018, 
                    value = 2015,
                    sep= "",
                    width = '200px')), 
        mainPanel(plotOutput("cityMap"))),
    #mainPanel(plotOutput("cityMap")),
    
    # Create the map of arrests by year and type per precinct
    mainPanel(h3("Occurrences of Arrest by Year and Precinct"), 
              p("This map will show which precincts have higher number of a certain crime occuring in a single year. Use the drop-box to select a crime of interest. Use the slider on the right to pick the year of interest.")),
    sidebarLayout(
        position = "right",
        sidebarPanel(
            "Pick a crime:",
            selectInput(
                inputId = "cPrecinct",
                label = '',
                selected = "AGGRAVATED HARASSMENT 2",
                choices = as.list(sort(unique(arrests$PD_DESC))),
                width = '300px'), 
            "Pick a year:", 
            sliderInput(
                inputId = "y",
                label = '',
                min = 2015, 
                max = 2018,
                value = 2015,  
                sep = "",
                width = '200px')), 
        mainPanel(plotOutput("crimePrecinctMap"))), 
    #mainPanel(plotOutput("crimePrecinctMap")),
    mainPanel(p("The best use of this map is for more frequent arrests. This map will return errors if there is no data on certain arrests in the year since quintiles cannot be calculated on no data.")),
    
    # Create the map of arrests by year and type
    mainPanel(h3("Location of Arrest by Year"), 
              p("This map shows the location of a particular crime that have occured in a single year. Use the drop-box to select a crime of interest. Use the slider on the right to pick the year of interest.")), 
    sidebarLayout(
        position = "right",
        sidebarPanel(
            "Pick a crime:",
            selectInput(
                inputId = "cMap",
                label = '',
                choices = as.list(sort(unique(arrests$PD_DESC))),
                width = '300px'), 
            "Pick a year:", 
            sliderInput(
                inputId = "z",
                label = '',
                min = 2015, 
                max = 2018,
                value = 2015,  
                sep = "",
                width = '200px')), 
        mainPanel(plotOutput("crimeMap"))), 
    #mainPanel(plotOutput("crimeMap")),
    
    # Create a bar graph of arrests by month from 2015 to 2018
    mainPanel(h3("Crime vs. Time"), 
              p("This bar graph shows the frequency of a crime that has occured by month from 2015 to 2018. Use the drop-box to select a crime of interest.")),
    sidebarLayout(
        position = "right",
        sidebarPanel(
            "Pick a crime:",
            selectInput(
                inputId = "cBar",
                label = '',
                choices = as.list(sort(unique(arrests$PD_DESC))), 
                width = '300px')), 
        mainPanel(plotOutput("crimeBarPlot")))
    #mainPanel(plotOutput("crimeBarPlot")),
    
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
    
    # Create the second map that shows percentile of crimes occuring by precinct
    output$crimePrecinctMap <- renderPlot({
        
        # Filter out relevant information queried by user and sum by precinct
        mapping = arrests %>% filter(PD_DESC == input$cPrecinct & 
                                         year(ARREST_DATE) == input$z) %>%
            group_by(ARREST_PRECINCT) %>% summarize(freq = n())
 
        # Get the quintiles for the frequency values
        quintiles = round(quantile(mapping$freq, probs = seq(0, 1, 0.2), na.rm = TRUE), 3)
        
        # Create a vector to be used in a map legend for the quintiles calculated
        key = c()
        for(i in 1:5){
            key = c(key, paste(quintiles[i], quintiles[i+1], sep = ' to '))
        }
        
        # Determine which precincts and its frequency value belong in which quintile region
        mapping = mapping %>% mutate(quintile = cut(mapping$freq, 
                                                    breaks = quintiles, 
                                                    include.lowest = TRUE, 
                                                    labels = brewer.pal(n = 5, "Blues"))) %>% 
            add_row(ARREST_PRECINCT = setdiff(sf$Precinct, mapping$ARREST_PRECINCT), freq = 0, quintile = '#FFFFFF')
        
        
        # Match the precincts in the shape file with the ones in the data frame created above
        mapping = mapping %>% slice(match(sf$Precinct, mapping$ARREST_PRECINCT))
        
        # Plot the map with its appropriate arrest quintile and add a legend
        ggplot(sf) + geom_sf(color = "black", aes(fill = mapping$quintile), show.legend = TRUE) + 
            scale_fill_brewer(palette = "Blues", name = "Number of Arrests",
                              labels = c("No data", key)) + 
            labs(x = "Longitude", y = "Latitude") + 
            coord_sf(crs = st_crs(2263)) + 
            theme_minimal() + 
            theme(legend.position = "bottom") %>% print
        
    }, height = 400, width = 400)
    
        
    # Creates the third map using coordinates of a single crime in a particular year
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

