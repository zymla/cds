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


#= Load dataset ===========================================================
tic()
trips_filename <- list.files('../data_raw', pattern = 'divvy_trips_[0-9]{4}(_[0-9]{2}){2}_.*\\.csv', full.names = TRUE) %>% max()
print(trips_filename)
trips <- fread(trips_filename, na.strings = c(""))
trips[
    , 
    `:=`(
        start_time = as_datetime(start_time),
        stop_time = as_datetime(stop_time)
    ), 
    ]

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
#trips[, `:=`(wday_n = data.table::wday(start_time - 4 * ))]
#trips <- trips[wdays, on = 'wday_n']
print(head(trips))
print(toc())
#trips[, weekday := lubridate::wday(start_time, label=TRUE, abbr = FALSE)]
#trips[, start_hr := hour(start_time)]

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
        .(hr = hour(timestamp), wday_n = data.table::wday(timestamp - 4 * 3600), station_id, type)
        ][
            , 
            .N, 
            .(hr, wday_n, station_id, type)
            ][wdays, on = 'wday_n']
hrtf <-
    union_all(
        tibble(hr = 0:23) %>% 
            mutate(hr = as.factor(hr) %>% fct_shift(5)) %>% 
            mutate(type = 'from', hrtf = fct_relabel(hr, ~paste0('f', .))), #%>% 
        #        mutate(hrtf = fct_reorder(hrtf, as.integer(hr))),
        tibble(hr = 0:23) %>% 
            mutate(hr = as.factor(hr) %>% fct_shift(5)) %>%
            mutate(type = 'to',   hrtf = fct_relabel(hr, ~paste0('t', .))) #%>% 
        #        mutate(hrtf = fct_reorder(hrtf, as.integer(hr)))
    ) %>%
    mutate(hrtf = fct_reorder(hrtf, as.numeric(hr))) %>% 
    arrange(hrtf) %>% 
    as.data.table()


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
                    wdays[c(input$Sun, input$Mon, input$Tue, input$Wed, input$Thu, input$Fri, input$Sat), .(wday_abr)],
                    on = 'wday_abr'
                    ][, .(N = sum(N)), .(hr, station_id, type)] %>%
                    { 
                        rbind(
                            .[type == 'from', .(type, hr, N = -N), station_id],
                            .[type == 'to',   .(type, hr, N),      station_id]
                        )
                    }
            )[hrtf, on = c('hr', 'type')][order(station_id, hrtf)][, .(hr, hrtf, N, dailyN = sum(abs(N))), station_id][, .(N = N/dailyN, dailyN, station_id, hrtf)] 
        
        print("Done selecting sample")
        
        #= Format data sample for kmeans ===========================================================
        station_movements_kmeans_input <-
            station_movements_subset[!is.na(station_id)] %>% dcast(station_id + dailyN ~ hrtf, fill = 0, value.var = 'N')
        station_movements_kmeans_input[, `:=`(dailyN = 1000*(dailyN <= input$low_trips_thd))]
        
        km_cent <- input$nb_clusters # PARAMETRE A FAIRE VARIER
        classifST <- kmeans(station_movements_kmeans_input[,3:ncol(station_movements_kmeans_input)], centers = km_cent, algorithm = input$kmeansAlgo) 
        stationsKM <- cbind(station_movements_kmeans_input, classeKM = factor(classifST$cluster))
        print(paste("Nb_cluster=", km_cent))
        
        coor_st = trips[!is.na(from_longitude) & !is.na(from_latitude), .(latitude = last(from_latitude), longitude = last(from_longitude)), .(id = from_station_id)]
        
        # 5 - Ajout des coordonnées géographiques et visualisation
        stations_visu <- merge(stationsKM[, .(id = station_id, classeKM)], coor_st, by = "id") #stations_visu
        
        factpal <- colorFactor(topo.colors(input$nb_clusters), stations_visu$classeKM)
        
        list(
            station_movements_subset = station_movements_subset,
            station_movements_kmeans_input = station_movements_kmeans_input,
            km_cent = km_cent,
            stations_visu = stations_visu,
            stationsKM = stationsKM,
            coor_st = coor_st,
            factpal = factpal,
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
            #    scale_color_manual(values = d$factpal(1:d$km_cent)) +
            facet_wrap(~classeKM)
    })
    
    output$tripratePlot <- renderPlot({
        d <- recactiveClustering()
        d$station_movements_subset[hrtf, on = 'hrtf'][d$stationsKM[,.(station_id, classeKM)], on = 'station_id'] %>% 
            ggplot() +
            geom_line(
                aes(hr, N, group = interaction(station_id, type), color = classeKM), alpha = .1
            ) +
            geom_line(
                data = d$classifST$centers %>% melt(value.name = 'N') %>% mutate(classeKM = as.factor(Var1), hrtf = Var2)%>% left_join(hrtf %>% as.tibble(), by = 'hrtf'),
                aes(hr, N, group = interaction(classeKM, type))
            ) +
            facet_wrap(~classeKM) +
            ylim(-.2, .2)
    })

})
