#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(magrittr)
library(data.table)
library(leaflet)
library(tictoc)


# Define server logic required to draw a histogram
shinyServer(
    function(input, output) {

#= Load dataset ===========================================================
    stations_filename <- list.files('../data_raw', pattern = 'divvy_stations_[0-9]{4}(_[0-9]{2}){2}_.*\\.csv', full.names = TRUE) %>% max()
    print(stations_filename)
    stations <- fread(stations_filename, na.strings = c(""), nrows = 1E6)
    print("Done loading data file")
    
#= Convert types of dataset ===========================================================
    stations[
        , 
        `:=`(
            available_bikes  = as.integer(available_bikes),
            available_docks  = as.integer(available_docks),
            docks_in_service = as.integer(docks_in_service),
            latitude         = as.double(latitude),
            longitude        = as.double(longitude),
            percent_full     = as.integer(percent_full),
            timestamp        = as_datetime(timestamp),
            total_docks      = as.integer(total_docks)
        ), 
        ]
#= Create features ===========================================================
    stations[,tx_utilisation := available_bikes/docks_in_service]
    stations[,date := as.Date(timestamp)]
    tic()
    # lubridate::wday is replaced by data.table::wday and a data.table join for higher performance 
    #stations[, weekday := lubridate::wday(timestamp, label=TRUE, abbr = FALSE)]
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
    stations[, `:=`(wday_n = data.table::wday(timestamp))]
    stations <- stations[wdays, on = 'wday_n']
    print(toc())
    tic()
    stations[, month := lubridate::month(timestamp, label=TRUE, abbr = FALSE)]
    print("Done creating features")
    

 #   reactiveStationsCluster <- reactive({
 #       #= kmeans clustering ===========================================================
 #       #set.seed(1234)
#        km_cent <- input$nb_clusters # PARAMETRE A FAIRE VARIER
#        print(paste("Nb_cluster=", km_cent))
 #       classifST <- kmeans(base3[,2:ncol(base3)], centers = km_cent) 
 #       #  rapide, ajouter nstart = 100?
 #       
 #       stationsKM <- cbind(base3,classeKM = factor(classifST$cluster))
        
        # 5 - Ajout des coordonnées géographiques et visualisation
 #       coor_st <- stations[!duplicated(id), .(id, latitude, longitude)]
 #       merge(stationsKM[, .(id, classeKM)], coor_st, by = "id") #station_visu
  #  })
    
 #   reactiveKmCent <- reactive({
 #       colorFactor(topo.colors(input$nb_clusters), (reactiveStationsCluster()$classeKM))
 #   })
    output$latlongclusterPlot <- renderPlot({
        print(wdays[c(input$Sun, input$Mon, input$Tue, input$Wed, input$Thu, input$Fri, input$Sat)])
        #= Select data sample ===========================================================
        stations3 <- 
            stations[
                    id != 582
                ][
                    wdays[c(input$Sun, input$Mon, input$Tue, input$Wed, input$Thu, input$Fri, input$Sat), .(wday_abr)],
                    on = 'wday_abr'
                ][
                    , 
                    .(tx_utilisation = mean(tx_utilisation, na.rm = TRUE)), 
                    .(id, hr = hour(timestamp))][hr > 6 | hr < 3
                                                 ]
        print("Done selecting sample")
        
        #= Format data sample for kmeans ===========================================================
        base3 <- stations3 %>% dcast(id ~ hr)
        print("Done pivoting data")
        
        km_cent <- input$nb_clusters # PARAMETRE A FAIRE VARIER
        print(paste("Nb_cluster=", km_cent))
        classifST <- kmeans(base3[,2:ncol(base3)], centers = km_cent) 
        #  rapide, ajouter nstart = 100?
        
        stationsKM <- cbind(base3,classeKM = factor(classifST$cluster))
        
        # 5 - Ajout des coordonnées géographiques et visualisation
        coor_st <- stations[!duplicated(id), .(id, latitude, longitude)]
        stations_visu <- merge(stationsKM[, .(id, classeKM)], coor_st, by = "id") #stations_visu
        
        factpal <- colorFactor(topo.colors(input$nb_clusters), stations_visu$classeKM)
        stations_visu[id != 622] %>% 
            ggplot() +
            geom_point(data = stations_visu[id != 622, .(longitude, latitude)], aes(longitude, latitude), color = 'gray') +
            geom_point(aes(longitude, latitude, color = classeKM)) +
            scale_color_manual(values = factpal(1:km_cent)) +
            facet_wrap(~classeKM)
    })
 #   output$useratePlot <-({
 #       stations3[reactiveStationsCluster()[, .(id, classeKM)], on = 'id'] %>% 
 #           ggplot() +
 #           geom_point(aes(x = hr, y = tx_utilisation, group = id, color = classeKM), alpha = .1) +
 #           scale_color_manual(values = factpal(1:reactiveKmCent())) +
 #           facet_wrap(~classeKM)
 #   })

})