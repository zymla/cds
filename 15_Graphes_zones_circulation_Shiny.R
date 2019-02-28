#####################################################################################
# Lorsqu'on sélectionne une station sur le graphe ou sur la carte, un popup         # 
# s'affiche sur la carte et le sommet devient noir                                  #
# Les informations sur la station s'affichent au-dessous du graphe                  #
# Les couleurs dans les 2 méthodes de détection des communautés sont affectés       #
# de façon semblable (en fonction de la taille du cluster)                          #
#####################################################################################


library(shiny)
library(data.table)
library(magrittr)
library(igraph)
library(visNetwork)
library(leaflet)
library(htmltools)
library(shinyWidgets)

# Lecture et mise en forme de fichiers
fichier_graphe <- fread("graphes_matrice.csv")
fichier_stations <- fread("graphes_stations.csv")
fichier_graphe$from_station_id <- as.character(fichier_graphe$from_station_id)
fichier_graphe$to_station_id <- as.character(fichier_graphe$to_station_id)
fichier_stations$id <- as.character(fichier_stations$id)
print("FICHIERS IMPORTES")


ui <- fluidPage(titlePanel(h1(id="Titre","Réseau Divvy")),
                tags$style(HTML("#Titre{color: #0B0B61; font-weight: 900;}")),    
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
            sliderInput("Nb_tr_min", label = "Nombre minimum de trajets par heure", 
                        min = 0, 
                        max = 0.2, 
                        value = 0.05,
                        step = 0.05,
                        width = "100%"),
            radioButtons("methode", "Information affichée",
                         choices = list("Degré entrant" = 1,
                                        "Degré sortant" = 2,
                                        "Communautés - Algorithme de Louvain" = 3, 
                                        "Communautés - Décomposition spectrale" = 4), 
                         selected = 1),
            
            width = 3
           
          ),
          mainPanel( 
            setBackgroundColor("#EBEBEB"),
            
            fluidRow(
              
              column(width = 7,
                     fluidRow(visNetworkOutput('Graphe_trajets')),
                     br(),
                     br(),
                     br(),
                     column(7,""),
                     column(5,
                            # Un peu de CSS
                            # https://www.w3.org/Style/Examples/007/fonts.fr.html
                            # https://shiny.rstudio.com/articles/css.html
                            tags$style(type = "text/css", "#txtStation {font-family: Georgia, serif; font-weight: 800; color: #0B0B61}"),
                            tags$style(type = "text/css", "#txtBornes {font-family: Georgia, serif; color: #0B0B61}"),
                            tags$style(type = "text/css", "#txtLatitude {font-family: Georgia, serif; color: #0B0B61}"),
                            tags$style(type = "text/css", "#txtLongitude {font-family: Georgia, serif; color: #0B0B61}"),
                            tags$style(type = "text/css", "#txtDeparts {font-family: Georgia, serif; color: #0B0B61}"),
                            tags$style(type = "text/css", "#txtArrivees {font-family: Georgia, serif; color: #0B0B61}"),
                            tags$style(type = "text/css", "#txtMesure {font-family: Georgia, serif; color: #0B0B61}"),
                            fluidRow(textOutput('txtStation')),
                            fluidRow(textOutput('txtBornes')),
                            fluidRow(textOutput('txtLatitude')),
                            fluidRow(textOutput('txtLongitude')), 
                            fluidRow(textOutput('txtDeparts')),
                            fluidRow(textOutput('txtArrivees'))
                            
                            )
                     ),
              column(width = 5,
                     tags$style(type = "text/css", "#Carte_stations {height: calc(80vh) !important;}"),
                     leafletOutput('Carte_stations'))
            ) 
          )
        )
)

server <- function(input, output) {
  
  freact <- reactive({
  
  co_occ_oriented <- fichier_graphe[
    weekday %in% input$Jours & hr_depart <= input$Choix_heure[2] & hr_depart >= input$Choix_heure[1] & to_station_id != from_station_id, .(from_station_id, to_station_id, N)
    ][
      ,sum(N), by = .(from_station_id,to_station_id)
      ]
  names(co_occ_oriented) <- c("from_station_id","to_station_id","N")
  
  
  # Comptage de departs et arrivées
  departs <- co_occ_oriented[,sum(N), by = .(from_station_id)]
  arrivees <- co_occ_oriented[,sum(N), by = .(to_station_id)]

  # Elimination de trajets peu fréquents
  N_min <- length(input$Jours) * (input$Choix_heure[2] - input$Choix_heure[1]) * 52 * input$Nb_tr_min 
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
  
  # Calcul des métriques locales
  centrality_w <- data.frame(index=V(g)$name,
                             degre_pondere_out=graph.strength(g,mode="out"), 
                             degre_pondere_in=graph.strength(g,mode="in"))
  
  # Degré entrant
  if(input$methode == 1){ 
    col_split <- cut(centrality_w$degre_pondere_in, breaks = unique(quantile(centrality_w$degre_pondere_in,0:9/9)), include.lowest = T)
    ramp_pal <- colorRampPalette(c("brown","red","yellow","springgreen","royalblue","darkmagenta"))
    color_vector <- ramp_pal(length(levels(col_split)))
    V(g)$color <- color_vector[col_split]
  }
  
  # Degré sortant
  else if(input$methode == 2){
    col_split <- cut(centrality_w$degre_pondere_out, breaks = unique(quantile(centrality_w$degre_pondere_out,0:9/9)), include.lowest = T)
    ramp_pal <- colorRampPalette(c("brown","red","yellow","springgreen","royalblue","darkmagenta"))
    color_vector <- ramp_pal(length(levels(col_split)))
    V(g)$color <- color_vector[col_split]
  }
  
  
  # Clustering avec l'algorithme de Louvain
  else if(input$methode == 3){
    c <- cluster_louvain(as.undirected(g))
    
    # Gestion de l'ordre de couleurs
      memb_init <- as.data.table(table(c$membership))
      setorder(memb_init,-N)
      memb_init$compteur <- 1
      memb_init[, cluster0 := rowid(compteur)]
      
      cluster_col1 <- function(ligne){cluster_def <- ligne * 2 - 1}
      
      cluster_col2 <- function(ligne, nlignes){cluster_def <- (nlignes - ligne) * 2 + 2}
      
      
      memb_init[cluster0 <= ceiling(nrow(memb_init)/2), cluster_def := cluster_col1(cluster0)]
      memb_init[cluster0 > ceiling(nrow(memb_init)/2), cluster_def := cluster_col2(cluster0,nrow(memb_init))]
      
      memb_init$N <- NULL
      memb_init$compteur <- NULL
      memb_init$cluster0 <- NULL
      memb_init$V1 <- as.integer(memb_init$V1)
      
      
      membership <- data.table(as.integer(c$membership))
      membership$compteur <- 1
      membership[, idx := rowid(compteur)]
      membership$compteur <- NULL
      
      clusters <- merge(membership, memb_init)
      setorder(clusters,idx)
      clusters$idx <- NULL
      
      # col_split <- c$membership
      col_split <- clusters$cluster_def
    # Fin de gestion de couleurs
    
    
    ramp_pal <- colorRampPalette(c("brown","red","yellow","springgreen","royalblue","darkmagenta"))
    color_vector <- ramp_pal(uniqueN(col_split))
    V(g)$color <- color_vector[col_split]
    V(g)$label <- paste0(V(g)$label,", Communauté : ",col_split)
  }
  
  # Clustering spectral
  else if(input$methode == 4){
    c <- cluster_leading_eigen(as.undirected(g),options=list(maxiter=1000000))
    
    # Gestion de l'ordre de couleurs
      memb_init <- as.data.table(table(c$membership))
      setorder(memb_init,-N)
      memb_init$compteur <- 1
      memb_init[, cluster0 := rowid(compteur)]
      
      cluster_col1 <- function(ligne){cluster_def <- ligne * 2 - 1}
      
      cluster_col2 <- function(ligne, nlignes){cluster_def <- (nlignes - ligne) * 2 + 2}
      
      
      memb_init[cluster0 <= ceiling(nrow(memb_init)/2), cluster_def := cluster_col1(cluster0)]
      memb_init[cluster0 > ceiling(nrow(memb_init)/2), cluster_def := cluster_col2(cluster0,nrow(memb_init))]
      
      memb_init$N <- NULL
      memb_init$compteur <- NULL
      memb_init$cluster0 <- NULL
      memb_init$V1 <- as.integer(memb_init$V1)
      
      
      membership <- data.table(as.integer(c$membership))
      membership$compteur <- 1
      membership[, idx := rowid(compteur)]
      membership$compteur <- NULL
      
      clusters <- merge(membership, memb_init)
      setorder(clusters,idx)
      clusters$idx <- NULL
      
      # col_split <- c$membership
      col_split <- clusters$cluster_def
    # Fin de gestion de couleurs
    
    # Visualisation avec un graphe
    ramp_pal <- colorRampPalette(c("brown","red","yellow","springgreen","royalblue","darkmagenta"))
    color_vector <- ramp_pal(uniqueN(col_split))
    V(g)$color <- color_vector[col_split]
    V(g)$label <- paste0(V(g)$label,", Communauté : ",col_split)
  }

  
  visu_stations <- data.table(V(g)$name, V(g)$color, V(g)$label, V(g)$size)
  setnames(visu_stations, old = c("V1", "V2","V3","V4"), new = c("id", "station_color","station_name","total_docks"))
  
  coord_geo <- fichier_stations[,.(id, latitude, longitude)]
  visu_stations <- merge(visu_stations,coord_geo, by="id")
  
  V(g)[which(V(g)$name == myNode$selected)]$color <- "black"
  V(g)[which(V(g)$name == myNode$selected)]$size <- 60
  
  return(list(visu_stations, g, departs, arrivees))
  
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
        visIgraph(freact()[[2]], idToLabel = FALSE, randomSeed = 123) %>%
          visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
  })
  
  myNode <- reactiveValues(selected = '')
  
  observeEvent(input$current_node_id, {
    myNode$selected <<- input$current_node_id
  })
  
  observeEvent(input$Carte_stations_marker_click, { 
    myNode$selected <- as.character(fichier_stations[
        input$Carte_stations_marker_click$lat == latitude &
          input$Carte_stations_marker_click$lng == longitude,
        .(id)])
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
  
  output$txtDeparts = renderText({
    if(nrow(freact()[[3]][which(myNode$selected == freact()[[3]]$from_station_id),])!=0){
      paste0("Nombre de départs par heure : ", 
             round((freact()[[3]][from_station_id == myNode$selected]$V1 / (length(input$Jours) * (input$Choix_heure[2] - input$Choix_heure[1]) * 52)),1)
             )
    } else {}
  }) 
  
  output$txtArrivees = renderText({
    if(nrow(freact()[[4]][which(myNode$selected == freact()[[4]]$to_station_id),])!=0){
      paste0("Nombre d'arrivées par heure : ", 
             round((freact()[[4]][to_station_id == myNode$selected]$V1 / (length(input$Jours) * (input$Choix_heure[2] - input$Choix_heure[1]) * 52)),1)
      )
    } else {}
  }) 
  
}

shinyApp(ui = ui, server = server)
