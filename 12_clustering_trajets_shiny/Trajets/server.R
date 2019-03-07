#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(lubridate)
library(ggplot2)
library(leaflet)
library(geosphere)
library(dbscan)



#======Load dataset=========================
#trips_filename <- list.files('../../data_raw', pattern = 'divvy_trips_[0-9]{4}(_[0-9]{2}){2}_.*\\.csv', full.names = TRUE) %>% max()
trips_filename <- "../../data_raw/plouf.csv"
trips_filename
trips <- fread(trips_filename, na.strings = c(""))
trips[
  , 
  `:=`(
    start_time = as_datetime(start_time),
    stop_time = as_datetime(stop_time)
  ), 
  ]
trips[, weekday := lubridate::wday(start_time, label=TRUE, abbr = FALSE)]
trips[, hr_depart := hour(start_time)]


#========== Attribution du nom et des coordonnées géographiques uniques à chaque station==========================================
nc_stations <- trips[!duplicated(trips[, from_station_id]),.(from_station_id,from_station_name,from_latitude,from_longitude) ]
trips$from_station_name <- NULL
trips$from_latitude <- NULL
trips$from_longitude <- NULL
trips$to_station_name <- NULL
trips$to_latitude <- NULL
trips$to_longitude <- NULL
trips <- merge(trips,nc_stations,by= c("from_station_id"))
setnames(nc_stations, old = c("from_station_id", "from_station_name","from_latitude","from_longitude"), new = c("to_station_id", "to_station_name","to_latitude","to_longitude"))
trips <- merge(trips,nc_stations,by= c("to_station_id"))



#======================================== Assimilation des trajets de la station A à la station B et à l'inverse
trips_1 <- trips[from_latitude < to_latitude & from_station_id != to_station_id, 
                 .(from_station_id,to_station_id,from_latitude,to_latitude,from_longitude,to_longitude,weekday,hr_depart)]
trips_2 <- trips[from_latitude > to_latitude & from_station_id != to_station_id, 
                 .(from_station_id,to_station_id,from_latitude,to_latitude,from_longitude,to_longitude,weekday,hr_depart)]
trips_3 <- trips[from_latitude == to_latitude & from_station_id != to_station_id, ]
nrow(trips_3) # 0

trips_1[,station1 := from_station_id]
trips_1[,station2 := to_station_id]
trips_1[,latitude1 := from_latitude]
trips_1[,latitude2 := to_latitude]
trips_1[,longitude1 := from_longitude]
trips_1[,longitude2 := to_longitude]
trips_2[,station1 := to_station_id]
trips_2[,station2 := from_station_id]
trips_2[,latitude1 := to_latitude]
trips_2[,latitude2 := from_latitude]
trips_2[,longitude1 := to_longitude]
trips_2[,longitude2 := from_longitude]
trips2 <- rbind(trips_1,trips_2)
trips2$to_station_id <- NULL
trips2$from_station_id <- NULL
trips2$to_latitude <- NULL
trips2$from_latitude <- NULL
trips2$to_longitude <- NULL
trips2$from_longitude <- NULL
head(trips2)

## 2 - Calcul de la distance de façon simplifiée pour Chicago
trips2[,latitude1m := latitude1*111130]
trips2[,latitude2m := latitude2*111130]
trips2[,longitude1m := longitude1*82717]
trips2[,longitude2m := longitude2*82717]

## 3 - Clustering des trajets avec la méthode K-means

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$TheMap <- renderLeaflet({
    
    #set.seed(1234)
    
    #km_cent <- input$km_cent  # Nombre de clusters 4 ou 5 paraît raisonnable
    classifKM <- kmeans(trips2[,.(latitude1m,latitude2m,longitude1m,longitude2m)], centers=input$km_cent) 
    trips2.KM <- cbind(trips2,classeKM = factor(classifKM$cluster))
    trips2.KM[,.N, by=.(classeKM)]
    
    jour_semaine <- input$JourSemaine #"samedi" # PARAMETRE A FAIRE VARIER
    hr_min <- input$HeureRange[0] # PARAMETRE A FAIRE VARIER
    hr_max <- input$HeureRange[1] # PARAMETRE A FAIRE VARIER
    
    # A comparer par exemple les graphiques obtenus pour samedi et jeudi.
    # Sans sélection du jour ni de la plage horaire : 
    tripsKM <- trips2.KM[,.N, by=.(latitude1,longitude1,latitude2,longitude2,classeKM)]

    # Avec sélection :
    ###tripsKM <- trips2.KM[weekday == jour_semaine & hr_depart >= hr_min & hr_depart <= hr_max,.N, by=.(latitude1,longitude1,latitude2,longitude2,classeKM)]
    setorder(tripsKM,-N)
    trips_dessin <- tripsKM[,head(.SD,200)]
    # affiche des classes pour lesquelles au moins 1 trajet sera dessiné :
    unique(trips_dessin$classeKM) 
    
    # Couleurs html
    # http://www.letoileauxsecrets.fr/couleurs/couleurs-web.html
    trips_dessin[classeKM==1,col:="#0000FF"] # blue 
    trips_dessin[classeKM==2,col:="#FF0000"] # red
    trips_dessin[classeKM==3,col:="#FFFF00"] # yellow
    trips_dessin[classeKM==4,col:="#8B4513"] # saddlebrown
    trips_dessin[classeKM==5,col:="#9370DB"] # mediumpurple
    trips_dessin[classeKM==6,col:= "#808080"] # grey
    m2 <- leaflet(data=trips_dessin) %>%
      addTiles() %>%
      setView(lng=-87.6297982, lat=41.8781136, zoom=11)
    for (i in 1:nrow(trips_dessin)){
      m2 <- m2 %>%
        addPolylines(lat=c(trips_dessin[i,]$latitude1,trips_dessin[i,]$latitude2),lng=c(trips_dessin[i,]$longitude1,trips_dessin[i,]$longitude2),color = trips_dessin[i,]$col,label = trips_dessin[i,]$classeKM,opacity = 1)
    }
    m2    
    
    
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    #km_cent <- seq(min(x), max(x), length.out = input$km_cent + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = km_cent, col = 'darkgray', border = 'white')
    
  })
  
})
