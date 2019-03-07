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
  titlePanel("Clustering des trajets"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("km_cent",
                   "Nombre de clusters:",
                   min = 1,
                   max = 7,
                   value = 4), 
      checkboxGroupInput("JourSemaine", label = h3("Jour de la semaine"), 
                          choices = list("Lundi" = "lundi", "Mardi" = "mardi", "Mercredi" = "mercredi", "Jeudi" = "jeudi",
                                         "Vendredi" = "vendredi", "Samedi" = "samedi", "Dimanche" = "dimanche"),
                          selected = "lundi"), 
      sliderInput("HeureRange", label = h3("Echelle de temps"), min = 1, 
                  max = 24, value = c(6, 12))
  
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       leafletOutput("TheMap")
    )
  )
))
