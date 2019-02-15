#####################################################################################
# En cours                                                                          #
# Etude de communautés (zones de circulation) avec l'algorithme de Louvain.         #
# Si on sélectionne une station sur le graphe, un popup s'affiche sur la carte et   #
# les informations sur la station s'affichent au-dessous du graphe                  #
#####################################################################################


library(shiny)
library(data.table)
library(magrittr)
library(igraph)
library(visNetwork)
library(leaflet)
library(htmltools)

# Lecture et mise en forme de fichiers
fichier_graphe <- fread("graphes_matrice.csv")
fichier_stations <- fread("graphes_stations.csv")
fichier_graphe$from_station_id <- as.character(fichier_graphe$from_station_id)
fichier_graphe$to_station_id <- as.character(fichier_graphe$to_station_id)
fichier_stations$id <- as.character(fichier_stations$id)
print("FICHIERS IMPORTES")


ui <- fluidPage(titlePanel("Zones de circulation"),
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("Jours", "Jours",
                               choices=c("Lundi"= "lundi",
                                         "Mardi"="mardi",
                                         "Mercredi"="mercredi",
                                         "Jeudi"="jeudi",
                                         "Vendredi"="vendredi",
                                         "Samedi"="samedi",
                                         "Dimanche"="dimanche"),
                               selected = c("lundi","mardi","mercredi","jeudi")),
            sliderInput("Choix_heure", label = "Plage horaire", 
                        min = 0, 
                        max = 24, 
                        value = c(0, 24),
                        width = "100%"),
            width = 3
           
          ),
          mainPanel( 
            fluidRow(
              column(width = 7,
                     fluidRow(visNetworkOutput('Graphe_trajets')),
                     fluidRow(textOutput('txtStation')),
                     fluidRow(textOutput('txtBornes')),
                     fluidRow(textOutput('txtLatitude')),
                     fluidRow(textOutput('txtLongitude'))),
              column(width = 5,
                     fluidRow(leafletOutput('Carte_stations',height="800px"))) )
          )
        )
)

server <- function(input, output) {
  
  freact <- reactive({
  jours <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche") 
  hr_depart_min <- 0 
  hr_depart_max <- 24 
  
  co_occ_oriented <- fichier_graphe[
    weekday %in% input$Jours & hr_depart <= input$Choix_heure[2] & hr_depart >= input$Choix_heure[1] & to_station_id != from_station_id,    .(from_station_id, to_station_id, N)
    ][
      ,sum(N), by = .(from_station_id,to_station_id)
      ]
  names(co_occ_oriented) <- c("from_station_id","to_station_id","N")
  

  N_min <- length(input$Jours) * 365 / 7 * (hr_depart_max - hr_depart_min) / 24 
  co_occ_oriented <- co_occ_oriented[N >= N_min]
  
  # Construction du graphe orienté
  g <- graph.data.frame(co_occ_oriented,directed = T)
  
  # Paramètrage du graphe : poids des différents trajets
  edges <- as_ids(E(g))
  head(edges)
  edges_mat<- do.call("rbind",strsplit(edges,"\\|"))
  edges_df <- data.frame(edges_mat,stringsAsFactors = F)
  names(edges_df) <- c("from_station_id","to_station_id")
  
  setkey(co_occ_oriented,from_station_id,to_station_id)
  E(g)$weight <- co_occ_oriented[edges_df]$N
  
  # On souhaite avoir l'epaisseur du trait entre 1 et 30
  minN <- min(co_occ_oriented[edges_df]$N)
  maxN <- max(co_occ_oriented[edges_df]$N)
  a <- (30 - 1)/(maxN - minN)
  b <- 30 - a * maxN 
  
  E(g)$width <- a * co_occ_oriented[edges_df]$N + b
  summary(E(g)$width)
  
  # Paramètrage du graphe : taille des noeuds en fonction de nombre de bornes
  setkey(fichier_stations,id)
  nb_bornes <- fichier_stations[names(V(g))]
  V(g)$size <- nb_bornes$total_docks
  
  # Paramètrage du graphe : noms des stations dans les libellés des noeuds
  setkey(fichier_stations,id)
  noms_stations <- fichier_stations[names(V(g))]
  head(noms_stations)
  V(g)$label <- noms_stations$station_name
  
  # Analyse de communautés avec l'algorithme de Louvain
  c <- cluster_louvain(as.undirected(g))
  
  # Visualisation avec un graphe
  col_split <- c$membership
  ramp_pal <- colorRampPalette(c("red","yellow","springgreen","royalblue"))
  color_vector <- ramp_pal(uniqueN(col_split))
  V(g)$color <- color_vector[col_split]
  
  # Visualisation sur une carte
  
  visu_stations <- data.table(V(g)$name, V(g)$color, V(g)$label, V(g)$size)
  setnames(visu_stations, old = c("V1", "V2","V3","V4"), new = c("id", "station_color","station_name","total_docks"))
  
  coord_geo <- fichier_stations[,.(id, latitude, longitude)]
  visu_stations <- merge(visu_stations,coord_geo, by="id")
  
  return(list(visu_stations,g))
  
  })
  

  output$Carte_stations <- renderLeaflet({
    
    m <- leaflet(data=freact()[[1]]) %>%
      addTiles() %>%
      setView(lng=-87.6297982, lat=41.8781136, zoom=11) %>%
      addCircleMarkers(~longitude, ~latitude, radius=2, color = ~station_color, 
        label = ~ paste("Station : ", station_name, sep = ""), 
        opacity = 1)
    
    if(nrow(fichier_stations[which(myNode$selected == fichier_stations$id),])!=0){
      m <- m %>%
      addPopups(fichier_stations[id == myNode$selected]$longitude, 
                fichier_stations[id == myNode$selected]$latitude, 
                fichier_stations[id == myNode$selected]$station_name,
                options = popupOptions(closeButton = FALSE))
    }
    m
  })
  
  output$Graphe_trajets <- renderVisNetwork({
        visIgraph(freact()[[2]], idToLabel = FALSE) %>%
          visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
  })
  
  myNode <- reactiveValues(selected = '')
  
  observeEvent(input$current_node_id, {
    myNode$selected <<- input$current_node_id
  })
  
  observeEvent(input$Jours, {
    myNode$selected <<- ''
  })
  
  observeEvent(input$Choix_heure, {
    myNode$selected <<- ''
  })
  
  
  output$txtStation = renderText({
    if(nrow(fichier_stations[which(myNode$selected == fichier_stations$id),])!=0){
      paste0("Station : ", fichier_stations[id == myNode$selected]$station_name)
    } else {}
  }) 
  
  output$txtBornes = renderText({
    if(nrow(fichier_stations[which(myNode$selected == fichier_stations$id),])!=0){
      paste0("Nombre de bornes : ", fichier_stations[id == myNode$selected]$total_docks)
    } else {}
  }) 
  
  output$txtLatitude = renderText({
    if(nrow(fichier_stations[which(myNode$selected == fichier_stations$id),])!=0){
      paste0("Latitude : ", fichier_stations[id == myNode$selected]$latitude)
    } else {}
  }) 
  
  output$txtLongitude = renderText({
    if(nrow(fichier_stations[which(myNode$selected == fichier_stations$id),])!=0){
      paste0("Longitude : ", fichier_stations[id == myNode$selected]$longitude)
    } else {}
  }) 
  
}

shinyApp(ui = ui, server = server)
