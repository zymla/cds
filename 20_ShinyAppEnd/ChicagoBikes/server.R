#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(tidyverse)
library(lubridate)
library(magrittr)
library(data.table)
library(leaflet)
library(tictoc)
library(plotly)
library(scales)
library(igraph)
library(cluster)
library(ggthemes)
library(ggplot2)





#------------------------------------------------------------------------------------
# Jan's stuffs 

## We load data outside of shinyServer function, so as it is loaded only once
## for all concurrent users of the server

night_thd = 5
#= Load dataset ===========================================================
tic()
wdays <- 
  (
    seq.Date(ymd('2019-01-21'), ymd('2019-01-28'), 'day') %>% as.data.table()
  )[
    , 
    .(
      wday_n   = data.table::wday(`.`),
      wday_abr = lubridate::wday(`.`, label = TRUE)
    ),
    ][
      , 
      .N, 
      .(wday_n, wday_abr)
      ][
        order(wday_n), 
        .(wday_n, wday_abr)
        ]
print(getwd())
if(length(list.files('.', pattern = 'station_movements.rds'))){
  station_movements <- read_rds('./station_movements.rds')
} else {
  trips_filename <- list.files('../data_raw', pattern = 'divvy_trips_[0-9]{4}(_[0-9]{2}){2}_.*\\.csv', full.names = TRUE) %>% max()
  print(trips_filename)
  trips <- 
    fread(
      #          nrows = 1e6,
      trips_filename, 
      select = c('from_latitude', 'from_longitude', 'from_station_id', 'from_station_name', 'start_time', 'stop_time', 'to_latitude', 'to_longitude', 'to_station_id', 'to_station_name'), 
      na.strings = c("")
    )
  trips[
    , 
    `:=`(
      start_time = as_datetime(start_time),
      stop_time = as_datetime(stop_time),
      from_longitude = as.numeric(from_longitude),
      from_latitude = as.numeric(from_latitude),
      from_station_id = as.integer(from_station_id),
      to_latitude = as.numeric(to_latitude),
      to_longitude = as.numeric(to_longitude),
      to_station_id = as.integer(to_station_id)
    ), 
    ]
  
  print(head(trips))
  print(toc())
  
  # Selection et mise en forme des données de `trips`
  station_trips <- 
    (trips[
      from_station_id != to_station_id,
      .(start_time, stop_time, from_station_id, to_station_id)
      ] %>% 
      { 
        rbind(
          .[,.(station_id = from_station_id, timestamp = start_time, type = 'from')],
          .[,.(station_id = to_station_id, timestamp = stop_time, type = 'to')]
        )
      })
  coor_st <-
    trips[
      !is.na(from_longitude) & !is.na(from_latitude), 
      .(latitude = last(from_latitude), longitude = last(from_longitude), station_name = last(from_station_name)), 
      .(id = from_station_id)
      ]
  print(head(coor_st))
  
  station_movements <-
    station_trips[
      , 
      .(hr = hour(timestamp), wday_n = data.table::wday(timestamp - night_thd * 3600), station_id, type)
      ][
        , 
        .N, 
        .(hr, wday_n, station_id, type)
        ][
          coor_st, 
          on = .(station_id = id), 
          nomatch = 0
          ]
  write_rds(x = station_movements, path = './station_movements.rds')
}

station_movements <- station_movements[wdays, on = 'wday_n']
coor_st <- station_movements[, .(longitude = last(longitude), latitude = last(latitude), station_name = last(station_name)), .(id = station_id)]

hrtf <-
  union_all(
    tibble(hr = 0:23) %>% 
      mutate(hr = as.factor(hr) %>% fct_shift(night_thd)) %>% 
      mutate(type = 'from', hrtf = fct_relabel(hr, ~paste0('f', .))), #%>% 
    #        mutate(hrtf = fct_reorder(hrtf, as.integer(hr))),
    tibble(hr = 0:23) %>% 
      mutate(hr = as.factor(hr) %>% fct_shift(night_thd)) %>%
      mutate(type = 'to',   hrtf = fct_relabel(hr, ~paste0('t', .))) #%>% 
    #        mutate(hrtf = fct_reorder(hrtf, as.integer(hr)))
  ) %>%
  mutate(hrtf = fct_reorder(hrtf, as.numeric(hr))) %>% 
  arrange(hrtf) %>% 
  as.data.table()

stid_hrtf <- CJ(station_id = station_movements[, unique(station_id)], hrtf = hrtf[, hrtf], unique = TRUE)


print(toc())
tic()
print("Done creating features")

# End of Jan's stuffs
# ==============================================================================================  


# Anissa's stuffs
# Read data files
trajets <- readRDS("trajets.rda")
des_trajets <- readRDS("des_trajets.rda")  


# Magda's stuffs : Lecture et mise en forme de fichiers
#------------------------------------------------------------------------------------
fichier_graphe <- fread("../../graphes_matrice.csv")
fichier_stations <- fread("../../graphes_stations.csv")
fichier_graphe$from_station_id <- as.character(fichier_graphe$from_station_id)
fichier_graphe$to_station_id <- as.character(fichier_graphe$to_station_id)
fichier_stations$id <- as.character(fichier_stations$id)
print("FICHIERS IMPORTES")
#------------------------------------------------------------------------------------


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 
  # ==============================================================================================  
  # Magda's functions
  # ==============================================================================================  
  
 
  freact <- reactive({
    input$recompute_magda
    fg <<- fichier_graphe
    co_occ_oriented <- fichier_graphe[
      weekday %in% input$Jours & hr_depart <= input$Choix_heure[2] & hr_depart >= input$Choix_heure[1] & to_station_id != from_station_id, .(from_station_id, to_station_id, N)
      ][
        ,sum(N), by = .(from_station_id,to_station_id)
        ]
    co <<- co_occ_oriented
    names(co_occ_oriented) <- c("from_station_id","to_station_id","N")	
    print("check the names")
    print(head(names))
    
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
    
    print('Head of edges')
    print(head(edges))
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
    
    print("check color!!!")
    print(color_vector)
    print(head(V(g)$color))
    visu_stations <- data.table(V(g)$name, V(g)$color, V(g)$label, V(g)$size)
    setnames(visu_stations, old = c("V1", "V2","V3","V4"), new = c("id", "station_color","station_name","total_docks"))
    
    coord_geo <- fichier_stations[,.(id, latitude, longitude)]
    visu_stations <- merge(visu_stations,coord_geo, by="id")
    
    V(g)[which(V(g)$name == myNode$selected)]$color <- "black"
    V(g)[which(V(g)$name == myNode$selected)]$size <- 60
    
    
    return(list(visu_stations, g, departs, arrivees))
  
    print(head())
  })

  # End of freact_magda  
  #-------------------------------------------------------------------------------------------------------------
  
  
  # Carte des stations  
  #-------------------------------------------------------------------------------------------------------------  
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
  
  
  
  # Graphe 
  #-------------------------------------------------------------------------------------------------------------  
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
             round((freact()[[3]][from_station_id == myNode$selected]$V1 / (length(input$Jours) * (input$Choix_heure[2] - input$Choix_heure[1]) * 52)),2)
      )
    } else {}
  }) 
  
  output$txtArrivees = renderText({
    if(nrow(freact()[[4]][which(myNode$selected == freact()[[4]]$to_station_id),])!=0){
      paste0("Nombre d'arrivées par heure : ", 
             round((freact()[[4]][to_station_id == myNode$selected]$V1 / (length(input$Jours) * (input$Choix_heure[2] - input$Choix_heure[1]) * 52)),2)
      )
    } else {}
  }) 
 
# End Magda's stuffs   
#==========================================================================================================================================================
  
  
#==========================================================================================================================================================
# Jan's functions 
#==========================================================================================================================================================
  hover_station_id <- reactiveVal(0)
  
  reactive_hover <- observe({
    eventdata <- event_data("plotly_hover", source = 'latlongcluster')
    print(str(eventdata))
    d <- reactiveClustering()
    print('output$stationtripratepPlot')
    if((!is.null(eventdata)) && nrow(eventdata) && "key" %in% colnames(eventdata)){
      print(paste("reactive_hover(", eventdata$key, ")"))
      hover_station_id(eventdata$key)
    } else {
      isolate({
        last_hover_station_id <- hover_station_id()
        if(last_hover_station_id > 0){
          print('Back to all cluster triprate chart')
          hover_station_id(0)
        }
      })
    }
  }) # end of reactive hover
  
  
  reactiveClustering <- reactive({
    input$recompute
    print(wdays[c(input$Sun, input$Mon, input$Tue, input$Wed, input$Thu, input$Fri, input$Sat)])
    #= Select data sample ===========================================================
    station_movements_subset <- 
      station_movements[#
        wdays[c(input$Sun, input$Mon, input$Tue, input$Wed, input$Thu, input$Fri, input$Sat), .(wday_abr)],
        #          wdays[c(1), .(wday_abr)],
        on = 'wday_abr'
        ][
          , 
          .(N = sum(N, na.rm = TRUE) * if_else(type == 'to', 1, -1)), 
          .(hr, station_id, type)
          ][
            hrtf, 
            on = c('hr', 'type'),
            nomatch = 0
            ][
              order(station_id, hrtf)
              ][
                ,
                totalN := sum(abs(N), na.rm = TRUE),
                station_id
                ][
                  ,
                  rate := N/totalN
                  ]
    
    station_movements_subset <- station_movements_subset[stid_hrtf, on = c('hrtf', 'station_id'), ]
    station_movements_subset[is.na(rate), rate := 0]
    st_dn <- station_movements_subset[, .(totalN = min(totalN, na.rm = TRUE)), station_id]
    st_dn[is.infinite(totalN), totalN := 0]
    station_movements_subset <-
      station_movements_subset[, .(rate, station_id, hrtf)][, .(rate, station_id, hrtf)][st_dn, on = 'station_id', all = TRUE]
    print("Done selecting sample")
    
    #= Format data sample for kmeans ===========================================================
    station_movements_kmeans_input <-
      station_movements_subset[!is.na(station_id)] %>% dcast(station_id + totalN ~ hrtf, fill = 0, value.var = 'rate')
    station_movements_kmeans_input[, `:=`(totalN = 1000*(totalN <= input$low_trips_thd))]
    #    station_movements_kmeans_input[, `:=`(totalN = 1000*(totalN <= 1000))]
    # station_movements_kmeans_input %<>% na.omit()
    
    km_cent <- input$nb_clusters # PARAMETRE A FAIRE VARIER
    cluster_palette <- hue_pal()(km_cent)
    classifST <- kmeans(station_movements_kmeans_input[, 2:ncol(station_movements_kmeans_input)], centers = km_cent, algorithm = input$kmeansAlgo, iter.max=50) 
    stationsKM <- cbind(station_movements_kmeans_input, classeKM = factor(classifST$cluster))
    print(paste("Nb_cluster=", km_cent))
    
    
    # 5 - Ajout des coordonnées géographiques et visualisation
    stations_visu <- merge(stationsKM[, .(id = station_id, classeKM)], coor_st, by = "id") #stations_visu
    
    list(
      station_movements_subset = station_movements_subset,
      station_movements_kmeans_input = station_movements_kmeans_input,
      km_cent = km_cent,
      stations_visu = stations_visu,
      stationsKM = stationsKM,
      coor_st = coor_st,
      st_dn = st_dn,
      classifST = classifST,
      cluster_palette = cluster_palette
    ) 
  }) # end of reactive clustering
  
#===========================================================================================================================================================
# End of Jan's functions
  #===========================================================================================================================================================
  
  output$debugText <- renderText({
    eventdata <- event_data("plotly_hover", source = 'latlongcluster')
    #print('got hover')
    print(str(eventdata))
    validate(need(!is.null(eventdata) & 'key' %in% colnames(eventdata), "Hover over the chart"))
    #as.character(eventdata)
    d <- reactiveClustering()
    d$stations_visu[d$st_dn[, .(totalNlog10 = as.integer(log10(pmax(1, totalN, na.rm = TRUE))), totalN, id = station_id)], on = 'id'][id %in% eventdata$key] %>% as.character
  })    
  
  # Generate a map for each user
  m <- 
    leaflet() %>%
    addTiles() %>%
    setView(lng = coor_st %>% pull(longitude) %>% mean(), lat = coor_st %>% pull(latitude) %>% mean() , zoom=10)
  
  # Update the map (do not recreate a new map)
  output$stationMap <- renderLeaflet({
    eventdata <- event_data("plotly_hover", source = 'latlongcluster')
    #validate(need(is.null(eventdata) || "key" %in% colnames(eventdata)))
    d <- reactiveClustering()
    hovered_station <- 
      d$stations_visu[d$st_dn[, .(totalNlog10 = as.integer(log10(pmax(1, totalN, na.rm = TRUE))), totalN, id = station_id)], on = 'id'][id %in% eventdata$key]
    print('output$stationMap')
    
    mm <- m
    if(nrow(hovered_station)){
      lat <- hovered_station %>% pull(latitude)
      lon <- hovered_station %>% pull(longitude)
      mm <-
        m %>%
        setView(lng = lon, lat = lat, zoom = input$hovered_map_zoom) %>%
        addCircleMarkers(lng = d$stations_visu$longitude, lat = d$stations_visu$latitude, color = d$cluster_palette[d$stations_visu$classeKM], radius = 5, opacity = 1) %>% 
        addCircleMarkers(lng = lon, lat = lat, color = 'black', opacity = 1)
    } else {
      mm <-
        m %>% 
        addCircleMarkers(lng = coor_st$longitude, lat = coor_st$latitude, color = 'gray', radius = 2) 
    }
    mm
  })
  
  observe({
    hover_station_id()
  })
  
  output$stationtripratepPlot <- renderPlot({
    #eventdata <- event_data("plotly_hover", source = 'latlongcluster')
    #print(str(eventdata))
    hv_st_id <- hover_station_id()
    d <- reactiveClustering()
    print(paste('output$stationtripratepPlot', hv_st_id))
    
    p <- NULL
    
    #if((!is.null(eventdata)) && nrow(eventdata) && "key" %in% colnames(eventdata)){
    if(hv_st_id > 0){
      hover_class <- d$stationsKM[station_id == hv_st_id][, classeKM]
      (
        d$station_movements_subset[hrtf, on = 'hrtf'][d$stationsKM[,.(station_id, classeKM)], on = 'station_id'][classeKM == hover_class] %>% {
          print(str(.))
          print(str(. %>% filter(station_id == hv_st_id)))
          ggplot(data = ., aes(as.numeric(hr), rate)) +
            geom_line(
              aes(group = interaction(station_id, type)),
              alpha = input$alpha,
              color = d$cluster_palette[hover_class]
            ) +
            geom_line(
              data = . %>% filter(station_id == hv_st_id),
              aes(group = interaction(station_id, type)),
              alpha = 1,
              size = 1,
              color = d$cluster_palette[hover_class]
            ) +
            geom_line(
              data = 
                d$classifST$centers %>% 
                melt(value.name = 'rate') %>% 
                mutate(classeKM = as.factor(Var1), hrtf = Var2) %>% 
                left_join(hrtf %>% as.tibble(), by = 'hrtf') %>% 
                filter(classeKM == hover_class),
              aes(group = interaction(classeKM, type))
            ) +
            # Scale to 5-23 0-4. as.numeric(hr) return 1 (not 0) for the first hour (i.e. night_thd)
            scale_x_continuous(labels = function(x) { (x + night_thd - 1) %% 24 } ) +
            labs(x = 'heure', y = "% d'arrivées et départs (+ arrivées, - départs)") +
            ylim(-.2, .2)
        }
      ) -> p
    } else {
      p <-
        d$station_movements_subset[hrtf, on = 'hrtf'][d$stationsKM[,.(station_id, classeKM)], on = 'station_id'] %>% 
        ggplot(aes(as.numeric(hr), rate)) +
        geom_line(
          aes(group = interaction(station_id, type), color = classeKM),
          alpha = input$alpha
        ) +
        geom_line(
          data = 
            d$classifST$centers %>% 
            melt(value.name = 'rate') %>% 
            mutate(classeKM = as.factor(Var1), hrtf = Var2) %>% 
            left_join(hrtf %>% as.tibble(), by = 'hrtf'),
          aes(group = interaction(classeKM, type))
        ) +
        facet_wrap(~classeKM) +
        # Scale to 5-23 0-4. as.numeric(hr) return 1 (not 0) for the first hour (i.e. night_thd)
        scale_x_continuous(labels = function(x) { (x + night_thd - 1) %% 24 } ) +
        labs(x = 'heure', y = "% d'arrivées et départs (+ arrivées, - départs)") +
        ylim(-.2, .2)
    }
    p
  })
  
  
  output$latlonghoverDT <- renderTable({
    eventdata <- event_data("plotly_hover", source = 'latlongcluster')#, source = "source")
    #print('got hover')
    print(str(eventdata))
    validate(need(!is.null(eventdata) & 'key' %in% colnames(eventdata), "Hover over the chart"))
    #as.character(eventdata)
    d <- reactiveClustering()
    d$stations_visu[d$st_dn[, .(totalNlog10 = as.integer(log10(pmax(1, totalN, na.rm = TRUE))), totalN, id = station_id)], on = 'id'][id %in% eventdata$key]
  })
  
  output$latlongclusterPlot <- renderPlotly({
    d <- reactiveClustering()
    (
      d$stations_visu[d$st_dn[, .(totalNlog10 = as.integer(log10(pmax(1, totalN, na.rm = TRUE))), totalN, id = station_id)], on = 'id'] %>% 
        ggplot() +
        geom_point(data = d$stations_visu[, .(longitude, latitude)], aes(longitude, latitude), color = 'white', key = NA) +
        geom_point(aes(longitude, latitude, color = classeKM, alpha = totalNlog10, key = id, text = paste('Total trips:', totalN, '<br>', station_name))) +
        facet_wrap(~classeKM) 
    ) %>% ggplotly(source = 'latlongcluster') 
    #%>% onRender("
    #    function(el, x) {
    #      el.on('plotly_click', function(d) {
    #        var url = d.points[0].customdata;
    #        window.open('http://news.google.com');
    #      });
    #    }
    #  ")
  })
  
  output$tripratePlot <- renderPlotly({
    d <- reactiveClustering()
    (
      d$station_movements_subset[hrtf, on = 'hrtf'][d$stationsKM[,.(station_id, classeKM)], on = 'station_id'] %>% 
        ggplot(aes(as.numeric(hr), rate)) +
        geom_line(
          aes(group = interaction(station_id, type), color = classeKM),
          alpha = input$alpha
        ) +
        geom_line(
          data = 
            d$classifST$centers %>% 
            melt(value.name = 'rate') %>% 
            mutate(classeKM = as.factor(Var1), hrtf = Var2) %>% 
            left_join(hrtf %>% as.tibble(), by = 'hrtf'),
          aes(group = interaction(classeKM, type))
        ) +
        facet_wrap(~classeKM) +
        # Scale to 5-23 0-4. as.numeric(hr) return 1 (not 0) for the first hour (i.e. night_thd)
        scale_x_continuous(labels = function(x) { (x + night_thd - 1) %% 24 } ) +
        labs(x = 'heure', y = "% d'arrivées et départs (+ arrivées, - départs)") +
        ylim(-.2, .2)
    ) %>% ggplotly() 
  })
  
  output$triprateclustermeansPlot <- renderPlotly({
    d <- reactiveClustering()
    (
      d$classifST$centers %>% 
        melt(value.name = 'rate') %>% 
        mutate(classeKM = as.factor(Var1), hrtf = Var2) %>% 
        left_join(hrtf %>% as.tibble(), by = 'hrtf') %>%
        left_join(d$stationsKM[,.(station_id, classeKM)][d$st_dn, on = 'station_id'][, .(sum_dn = sum(totalN)), classeKM], on = 'classeKM') %>% 
        ggplot(aes(as.numeric(hr), rate)) +
        geom_line(
          aes(group = interaction(classeKM, type), color = classeKM, alpha = log10(sum_dn))
        ) +
        # Scale to 5-23 0-4. as.numeric(hr) return 1 (not 0) for the first hour (i.e. night_thd)
        scale_x_continuous(labels = function(x) { (x + night_thd - 1) %% 24 } ) +
        labs(x = 'heure', y = "% d'arrivées et départs (+ arrivées, - départs)") +
        ylim(-.2, .2)
    ) %>% ggplotly()
  })
  
  output$triptotalNPlot <- renderPlotly({
    d <- reactiveClustering()
    (
      d$stations_visu[d$st_dn[, .(totalNlog10 = as.integer(log10(totalN)), totalN, id = station_id)], on = 'id'] %>% 
        ggplot() +
        geom_point(data = d$stations_visu[, .(longitude, latitude)], aes(longitude, latitude), color = 'gray') +
        geom_point(aes(longitude, latitude, color = classeKM, text = paste('Total trips:', totalN))) +
        facet_wrap(~totalNlog10)
    ) %>% ggplotly()
  })
    
  
  # ==============================================================================================  
  # Anissa's functions
  # ==============================================================================================  
  
  freact_anissa <- reactive({
    
    if (input$unique == "tous"){
      trajets_cl<-copy(trajets[wday_n == input$jour & nb_trips > 0 , input$maselec, with=FALSE])
      trajets_id<-copy(trajets[wday_n == input$jour & nb_trips > 0 , c("trajet_id")])
      print("in unique loop")
    } else {
      if (input$unique == "nonunique"){
        trajets_cl<-copy(trajets[wday_n == input$jour & nb_trips > 1 , input$maselec, with=FALSE])
        trajets_id<-copy(trajets[wday_n == input$jour & nb_trips > 1 , c("trajet_id")])
      }else{
        trajets_cl<-copy(trajets[wday_n == input$jour & nb_trips == 1 , input$maselec, with=FALSE])
        if (is.na(match("nb_trips",names(trajets_cl)))==FALSE) {
          trajets_cl<-trajets_cl[,-c("nb_trips")]
        }
        trajets_id<-copy(trajets[wday_n==input$jour & nb_trips == 1 ,c("trajet_id")])
      }
    } # if loop 
    
    set.seed(12345)
    if (input$methode_anissa == "kmeans"){
      km_hw<- kmeans(scale(trajets_cl, center=T, scale=T), centers=input$nbcl, iter.max=2000, algorithm = input$algo, nstart=50)
    } # if loop 
    
    if (input$methode_anissa == "kmeanspp"){
      km_hw <- kmeanspp(scale(trajets_cl,center=T,scale=T), k = input$nbcl, start = "random", iter.max = 1000, nstart = 20, 
                        algorithm=input$algo)
    } # if loop            
    
    if (input$methode_anissa == "dbscan"){       
      dbscan::kNNdistplot(scale(trajets_cl,center=T,scale=T), k=input$ch_minPts)
      averageDist <- colMeans(dbscan::kNNdist(scale(trajets_cl,center=T,scale=T),k=input$ch_minPts))
      eps_opt<-mean(averageDist)
      
      res_dbscan <- dbscan::dbscan(scale(trajets_cl, center=T, scale=T), eps=eps_opt, minPts=input$ch_minPts)
      rm(eps_opt,averageDist)
      
      # pour cohérence avec les autres routines 
      km_hw = res_dbscan
      rm(res_dbscan)
    } # if loop    
    
    if (input$methode_anissa == "clara"){     
      clara <- clara(scale(trajets_cl, center=T, scale=T), metric = input$distm, pamLike =TRUE, stand=TRUE, samples=500, input$nbcl)
  
      # Ajout des coordonn?ees g?ographiques pour la visualisation graphique des classes
      trajets_res <- cbind.data.frame(trajets_cl, trajets_id, cl_km_hw=factor(clara$clustering))
      trajets_res <- merge(trajets_res,des_trajets,by=c("trajet_id"),all.x=TRUE)
      rm(clara)
     } else {
      # Ajout des coordonn?ees g?ographiques pour la visualisation graphique des classes
      trajets_res <- cbind.data.frame(trajets_cl, trajets_id, cl_km_hw = factor(km_hw$cluster))
      trajets_res <- merge(trajets_res, des_trajets, by=c("trajet_id"), all.x=TRUE)
      rm(km_hw)
     }
    return(trajets_res)
  }) # freact
  

  
  output$trajets_from <- renderPlotly({
    trajets_res <<- freact_anissa()
    from <-trajets_res[, .(N = .N), .(from_longitude, from_latitude, from_station_name, cl_km_hw)] %>% 
      ggplot() +
      geom_point(data = trajets_res[, .(.N), .(from_longitude, from_latitude)], aes(from_longitude, from_latitude),
                 color = 'gray') +
      geom_point(aes(from_longitude, from_latitude, color = cl_km_hw, 
                     text = paste("Classe: ", cl_km_hw, '<br>',"Station:", from_station_name,'<br>', "Longitude:",
                                  from_longitude,'<br>',"Latitude:",from_latitude, "<br> Nb trajets:", N))) +
      labs(x="Longitude", y="Latitude", color="Classes")+
      #labs(title = paste("Visualisation des ", input$nbcl,"classes de trajets"), subtitle = "Selon la station de d?part")+
      facet_wrap(~cl_km_hw)
    from
  }) # renderplotly
  
  
  output$trajets_to <- renderPlotly({
    trajets_res <- freact_anissa()
    to <- trajets_res[, .(N = .N), .(to_longitude, to_latitude, to_station_name, cl_km_hw)] %>% 
      ggplot() +
      geom_point(data = trajets_res[, .(.N), .(to_longitude, to_latitude)], aes(to_longitude, to_latitude),
                 color = 'gray') +
      geom_point(aes(to_longitude, to_latitude, color = cl_km_hw,
                     text = paste("Classe: ", cl_km_hw, '<br>',"Station:", to_station_name,'<br>', 
                                  "Longitude:",to_longitude,'<br>',"Latitude:",to_latitude, "<br> Nb trajets:", N))) +
      labs(x="Longitude",y="Latitude",color="Classes") +
      #labs(title = paste("Visualisation des ",input$nbcl,"classes de trajets"), subtitle = "Selon la station d'arriv?e")+
      facet_wrap(~cl_km_hw)
    to   
    
  }) # renderplotly
  
  url <- a("Divvy Bikes Homepage", href="https://www.divvybikes.com/")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
 
  
  
  
 
  
  output$trajets_par_jours <- renderPlot({
  jours <- readRDS("jours.rda")
  
  g <- ggplot(jours, aes(fmois, nb_jours))
  g + geom_boxplot(aes(fill=factor(mois))) + 
    theme(axis.text.x = element_text(vjust=0.6)) + 
    theme(legend.position='none') +
    labs(x="Mois",
         y="Nombre de trajets quotidiens") +
    scale_y_continuous(breaks=seq(0,24000,2000),limits=c(0, 24000))
  })
  
  output$trajets_par_heure<- renderPlot({
  repart_heure <- fichier_graphe[,.(Nb_trajets = sum(N)), by=c("hr_depart")]
  repart_heure[,Nb_trajets_moy:=as.numeric(Nb_trajets/365)]
  
  p <- ggplot(data=repart_heure,aes(x=hr_depart,y=Nb_trajets_moy)) + geom_bar(stat="identity", fill="steelblue")+
    theme(plot.title=element_text(hjust = 0.5)) + labs(title="", x="Heure", y="Nombre de trajets moyen")
  p
  })
  
  
     
  # ==============================================================================================  
  # Geyser historgram
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  # ==============================================================================================  
  
  
})
