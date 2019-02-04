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

if(length(list.files('../data_gold', pattern = 'station_movements.rds'))){
  station_movements <- read_rds('../data_gold/station_movements.rds')
} else {
  trips_filename <- list.files('../data_raw', pattern = 'divvy_trips_[0-9]{4}(_[0-9]{2}){2}_.*\\.csv', full.names = TRUE) %>% max()
  print(trips_filename)
  trips <- 
    fread(
      #    nrows = 1e6,
      trips_filename, 
      select = c('from_latitude', 'from_longitude', 'from_station_id', 'start_time', 'stop_time', 'to_latitude', 'to_longitude', 'to_station_id'), 
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
          .(start_time, stop_time = start_time + trip_duration, from_station_id, to_station_id)
          ] %>% 
           { 
               rbind(
                   .[,.(station_id = from_station_id, timestamp = start_time, type = 'from')],
                   .[,.(station_id = to_station_id, timestamp = stop_time, type = 'to')]
               )
           })
  
  station_movements <-
      station_trips[
          , 
          .(hr = hour(timestamp), wday_n = data.table::wday(timestamp - night_thd * 3600), station_id, type)
          ][
              , 
              .N, 
              .(hr, wday_n, station_id, type)
              ]
}

station_movements <- station_movements[wdays, on = 'wday_n']
coor_st <- station_movements[, .(longitude = last(longitude), latitude = last(latitude)), .(id = station_id)]

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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  recactiveClustering <- reactive({
    input$recompute
    print(wdays[c(input$Sun, input$Mon, input$Tue, input$Wed, input$Thu, input$Fri, input$Sat)])
    #= Select data sample ===========================================================
    station_movements_subset <- 
      (
        station_movements[
#          wdays[c(1), .(wday_abr)],
          wdays[c(input$Sun, input$Mon, input$Tue, input$Wed, input$Thu, input$Fri, input$Sat), .(wday_abr)],
          on = 'wday_abr'
        ][
          , 
          .(N = sum(N)), 
          .(hr, station_id, type)
        ] %>%
          { 
            rbind(
              .[type == 'from', .(type, hr, N = -N), station_id],
              .[type == 'to',   .(type, hr, N),      station_id]
            )
          }
      )[
        hrtf, 
        on = c('hr', 'type'),
        nomatch = 0
      ][
        order(station_id, hrtf)
      ][
        , 
        .(hr, hrtf, N, dailyN = sum(abs(N))), 
        station_id
      ][
        , 
        .(N = N/dailyN, dailyN, station_id, hrtf)
      ]
    
    station_movements_subset <- station_movements_subset[stid_hrtf, on = c('hrtf', 'station_id'), ]
    station_movements_subset[is.na(N), N := 0]
    station_movements_subset <-
      station_movements_subset[, .(N, station_id, hrtf)][station_movements_subset[, .(dailyN = first(dailyN)), station_id], on = 'station_id', all = TRUE]
    print("Done selecting sample")
      
    #= Format data sample for kmeans ===========================================================
    station_movements_kmeans_input <-
      station_movements_subset[!is.na(station_id)] %>% dcast(station_id + dailyN ~ hrtf, fill = 0, value.var = 'N')
    station_movements_kmeans_input[, `:=`(dailyN = 1000*(dailyN <= input$low_trips_thd))]
    
    km_cent <- input$nb_clusters # PARAMETRE A FAIRE VARIER
    classifST <- kmeans(station_movements_kmeans_input[,3:ncol(station_movements_kmeans_input)], centers = km_cent, algorithm = input$kmeansAlgo) 
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
      classifST = classifST
    ) 
  })

    observeEvent(input$flush, {
        d <- recactiveClustering()
        tmpf <- paste0('data_log_stations_trips_', now() %>% str_replace_all('[^0-9]+', '_'), '.rds')
        print(tmpf)
        print(getwd())
        write_rds(d, path = tmpf)
    })
    
    output$latlongclusterPlot <- renderPlot({
        d <- recactiveClustering()
        d$stations_visu %>% 
            ggplot() +
            geom_point(data = d$stations_visu[, .(longitude, latitude)], aes(longitude, latitude), color = 'gray') +
            geom_point(aes(longitude, latitude, color = classeKM)) +
            facet_wrap(~classeKM)
    })
    
    output$tripratePlot <- renderPlot({
        d <- recactiveClustering()
        d$station_movements_subset[hrtf, on = 'hrtf'][d$stationsKM[,.(station_id, classeKM)], on = 'station_id'] %>% 
#        (d$station_movements_kmeans_input %>% melt('station_id', variable.name = 'hrtf', value.name = 'N'))[hrtf, on='hrtf'][stationsKM[,.(station_id, classeKM = classeKM)], on = 'station_id'] %>% 
            ggplot(aes(as.numeric(hr), N)) +
            geom_line(
                aes(group = interaction(station_id, type), color = classeKM),
                alpha = input$alpha
            ) +
            geom_line(
                data = 
                  d$classifST$centers %>% 
                    melt(value.name = 'N') %>% 
                    mutate(classeKM = as.factor(Var1), hrtf = Var2) %>% 
                    left_join(hrtf %>% as.tibble(), by = 'hrtf'),
                aes(group = interaction(classeKM, type))
            ) +
            facet_wrap(~classeKM) +
            # Scale to 5-23 0-4. as.numeric(hr) return 1 (not 0) for the first hour (i.e. night_thd)
            scale_x_continuous(labels = function(x) { (x + night_thd - 1) %% 24 }) +
            ylim(-.2, .2)
    })

})
