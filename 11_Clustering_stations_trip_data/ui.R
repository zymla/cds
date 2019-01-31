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




# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Divvy stations clustering based on trip data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            actionButton("flush", label = "Flush data to disk"),
            actionButton("recompute", label = "Recompute"),
            checkboxInput("Mon", "Monday", FALSE),
            checkboxInput("Tue", "Tueday", TRUE),
            checkboxInput("Wed", "Wednesday", FALSE),
            checkboxInput("Thu", "Thursday", FALSE),
            checkboxInput("Fri", "Friday", FALSE),
            checkboxInput("Sat", "Saturday", FALSE),
            checkboxInput("Sun", "Sunday", FALSE),
            sliderInput("nb_clusters",
                        "Number of clusters:",
                        min = 1,
                        max = 16,
                        value = 3),
            radioButtons("kmeansAlgo", label = "k-means algo", choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), select = "Hartigan-Wong")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            withSpinner(plotOutput("latlongclusterPlot")),
            withSpinner(plotOutput("tripratePlot"))
        )
    )
))