#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Ameliorer en mettant sur plusieurs page
# Ajouter l'option entre choisir l'ensemble des trajets et seulement une portion
# Ajouter stat des description taux de remplissage des bornes et part du temps en service

library(shiny)
library(DT)

####
#### Importation du fichier des trips 
####

trips_suscrib <- readRDS("D:/Formations/CEPE_2018_Formation_DataScience/Projet_formation/fichier_anissa/trips_suscrib.rda")

#### Premiere selection des trips qui nous interessent

#### Construction des variables qui vont nous etres utiles dans l'analyse

# On genere une liste contenant les noms des stations de depart
List_Id_from=unique(trips_suscrib[,from_station_id])
List_Id_to  =unique(trips_suscrib[,to_station_id])

trips_suscrib2<-trips_suscrib[,c("gender","trip_duration","age","from_station_id","to_station_id")]



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Trips - Customers characteristics"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         h4(p(strong("Choisir un trajet :"))),
         hr(),
         selectInput("from", "From Id num:", List_Id_from, multiple=FALSE),
         selectInput("to", "To Id num:", List_Id_to, multiple=FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
           # DT::dataTableOutput("data"),
           h4("*** Nombre de trajets realises sur la periode"),
            textOutput("volume"),
           h4("*** Ditribution de la duree des trajets"),
            verbatimTextOutput("duration"),
         # Je veux dessiner la fonction de densite de l age 
          # Et un tableau de stat des par classe d age
           h4("*** Fonction de densite de l age"),
            plotOutput("ageplot"),
            verbatimTextOutput("agetab"),
         h4("*** Combien d hommes et de femmmes"),
          # Je veux representer la repartition des sexes avec un histogramme
            plotOutput("genderplot")
      ))
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # On cree une fonction reactive qui va gerer le choix du trajet fait initialement
  # On va selectionner dans la table les lignes associees
  
  datab <- reactive({
    trips_suscrib2[from_station_id==input$from & to_station_id==input$to,]
  })
  
 # output$data <- DT::renderDataTable(
#    DT::datatable(datab())
#  )
  
  # Je veux le nb de lignes selectionnees soit le nb de trajets realises
  
  output$volume <- renderText({
    paste("Sur la periode la portion de trajet ",input$from, " - ",input$to, " a ete realise ", nrow(datab())," fois")
  })
  
  tags$hr()
  
  output$duration <- renderText({
    print("\n")
    paste("La duree pour la portion de trajet ",input$from, " - ",input$to," est :")
    summary(datab()[,c("trip_duration")])
  }) 
   
  output$ageplot <- renderPlot({
    plot(density(datab()$age,na.rm = TRUE))
  })
  
  # en fait la je veux cosntruire des classes d age et presenter cela sous forme d histogrammes
  output$agetab <- renderText({
    summary(datab()[,c("age")])
  })
  
  # Ajouter percent faire idem pour age
  output$genderplot <- renderPlot({
    barplot(table(datab()$gender),col=c("yellow","green"))
  })
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

