#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Divvy stations clustering"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("nb_clusters",
                        "Number of clusters:",
                        min = 1,
                        max = 50,
                        value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("latlongclusterPlot"),
            plotOutput("useratePlot")
        )
    )
))
