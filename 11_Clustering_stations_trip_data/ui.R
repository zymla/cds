#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(plotly)
library(DT)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Divvy stations clustering based on trip data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            actionButton("flush", label = "Flush data to disk"),
            actionButton("recompute", label = "Recompute"),
            checkboxInput("Mon", "Monday", TRUE),
            checkboxInput("Tue", "Tueday", TRUE),
            checkboxInput("Wed", "Wednesday", TRUE),
            checkboxInput("Thu", "Thursday", TRUE),
            checkboxInput("Fri", "Friday", FALSE),
            checkboxInput("Sat", "Saturday", FALSE),
            checkboxInput("Sun", "Sunday", FALSE),
            sliderInput("nb_clusters",
                        "Number of clusters:",
                        min = 1L,
                        max = 16L,
                        value = 12L,
                        step = 1L),
            sliderInput("low_trips_thd",
                        "Regroup stations with less than # trips:",
                        min = 10,
                        max = 1000,
                        value = 500),
            radioButtons("kmeansAlgo", label = "k-means algo", choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), select = "Hartigan-Wong"),
            sliderInput("alpha",
                        "Alpha:",
                        min = 0,
                        max = 1,
                        value = .1),
            sliderInput("hovered_map_zoom",
                        "Map zoom level (hovered):",
                        min = 10L,
                        max = 20L,
                        value = 15L,
                        step = 1L)
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          verbatimTextOutput('debugText', placeholder = TRUE),
          fluidRow(
            column(
              6,
              leafletOutput('stationMap')#, height = 300)
              ),
            column(
              6,
              plotOutput("stationtripratepPlot")
            )
          ),
#          tableOutput('latlonghoverDT'),
          withSpinner(plotlyOutput("latlongclusterPlot")),
          withSpinner(plotlyOutput("tripratePlot")),
          withSpinner(plotlyOutput("triprateclustermeansPlot")),
          withSpinner(plotlyOutput("triptotalNPlot"))
        )
    )
))
