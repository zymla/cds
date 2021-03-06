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
library(plotly)
library(scales)


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

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
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
    
  })
  
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
    classifST <- kmeans(station_movements_kmeans_input[, 2:ncol(station_movements_kmeans_input)], centers = km_cent, algorithm = input$kmeansAlgo) 
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
  })

    observeEvent(input$flush, {
        d <- reactiveClustering()
        tmpf <- paste0('data_log_stations_trips_', now() %>% str_replace_all('[^0-9]+', '_'), '.rds')
        print(tmpf)
        print(getwd())
        write_rds(d, path = tmpf)
    })

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
            labs(x = 'hour', y = 'pct of daily a/d (+ arrival, - departures)') +
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
            labs(x = 'hour', y = 'pct of daily a/d (+ arrival, - departures)') +
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
            labs(x = 'hour', y = 'pct of daily a/d (+ arrival, - departures)') +
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
          labs(x = 'hour', y = 'pct of daily a/d (+ arrival, - departures)') +
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
})
