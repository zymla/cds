#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(visNetwork)
library(leaflet)
library(plotly)
library(mlxR)


shinyUI(dashboardPage(
            dashboardHeader(title = "Les triplettes de Chicago"),
              dashboardSidebar(sidebarMenu(menuItem("Dashboard"                 , tabName = "dashboard"      , icon = icon("dashboard")),
                                           menuItem("Stations de Vélos"         , tabName = "StationsTab"    , icon = icon("th")),
                                           menuItem("Clustering des Trajets"    , tabName = "TrajetsClusTab" , icon = icon("th")),
                                           menuItem("Analyse du réseau"         , tabName = "TrajetsGraphTab", icon = icon("th")),
                                           menuItem("Widgets"                   , tabName = "widgets"        , icon = icon("th"))
                                          ) # sidebarMenu
                
                
                
                                  ), # dashboardSidebar 

                          dashboardBody(
                              # Premiere page  
                            tabItems(
                              # First tab content : a homepage? to describe data? 
                              #==========================================================================================================================
                              tabItem(tabName = "dashboard", h2("dashboard tab content"),
                                      fluidRow(
                                        column(width = 4,
                                               box(
                                                 title = " 1 Extraction des données du site divyy", width = NULL, status = "primary",
                                                 "site web divyy,
                                                 2 jeux de données : relevé des stations et des trajets, 
                                                 année 2017, XX Go, 
                                                 XX stations, XX trajets/jour/moyenne"
                                               ),
                                               box(
                                                 title = "Contenu de la base stations", width = NULL, solidHeader = TRUE, status = "primary",
                                                 "identifiant, localisation geographique, capacité, disponibilité des vélos, statut de fonctionnement des bornes, le tout horodaté, 
                                                 "
                                               ),
                                               box(
                                                 title = "Contenu de la base des trajets", width = NULL, background = "black",
                                                 "Identifiant des stations de départ et d'arrivée, localisation des stations de départ et d'arrivée, heures
                                                 d'arrivée et de départ, durée du trajet, identifiant du vélo et identifiant du trajet, statut d'abonnement du cycliste, 
                                                 si abonné information sur l'age et le sexe"
                                               )
                                               ), # column 1
                                        
                                        column(width = 4,
                                               box(
                                                 status = "warning", width = NULL,
                                                 "Box content"
                                               ),
                                               box(
                                                 title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
                                                 "Box content"
                                               ),
                                               box(
                                                 title = "Title 5", width = NULL, background = "light-blue",
                                                 "A box with a solid light-blue background"
                                               )
                                        ), # column 2
                                        
                                        column(width = 4,
                                               box(
                                                 title = "Title 2", width = NULL, solidHeader = TRUE,
                                                 "Box content"
                                               ),
                                               box(
                                                 title = "Title 6", width = NULL, background = "maroon",
                                                 "A box with a solid maroon background"
                                               )
                                        ) # column 3
                                      ) # fluidrow
                                      
                              
                                ), # tabitem1 

                              
                              
                              # First tab content : Jan's stuffs
                              #==========================================================================================================================
                              tabItem(tabName = "StationsTab", h2("Analyse des stations"),
                                    fluidRow(
                                      box(title = "Paramètres d'entrée", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 3, height = 991,             
                                          actionButton("flush", label = "Flush data to disk"),
                                          actionButton("recompute", label = "Re-calcul"),
                                          checkboxInput("Mon", "Lundi", TRUE),
                                          checkboxInput("Tue", "Mardi", TRUE),
                                          checkboxInput("Wed", "Mercredi", TRUE),
                                          checkboxInput("Thu", "Jeudi", TRUE),
                                          checkboxInput("Fri", "Vendredi", FALSE),
                                          checkboxInput("Sat", "Samedi", FALSE),
                                          checkboxInput("Sun", "Dimanche", FALSE),
                                          sliderInput("nb_clusters",  "Nombre de clusters:",  min = 1L,  max = 16L, value = 12L, step = 1L),                                                                                                          
                                          sliderInput("low_trips_thd",  " Nombre de trajets minimal pour regrouper la station (better word ??):",  min = 10,  max = 1000, value = 500),
                                          radioButtons("kmeansAlgo", label = "Algorithme k-means", choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), select = "Hartigan-Wong"),
                                          sliderInput("alpha",  "Valeur de Alpha:",   min = 0,   max = 1,  value = .1),
                                          sliderInput("hovered_map_zoom",   "Niveau de zoom de la carte:",   min = 10L, max = 20L,  value = 15L, step = 1L)
                                          ), # box1
                                      
                                      box(title = "Carte des stations", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4,
                                          leafletOutput("stationMap")
                                           ), # box2  
                                      
                                      box(title = "Départs et arrivées en fonction de l'heure", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 5,
                                          plotOutput("stationtripratepPlot")
                                           ), # box3
                                      
                                      box(title = "Positions des stations de chaque cluster", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 9, height = 510, 
                                          withSpinner(plotlyOutput("latlongclusterPlot"))
                                          ), # box4

                                      box(title = "Taux journalier d'arrivées et de départs pour chaque cluster", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                                          width = 6, height = 520, 
                                          withSpinner(plotlyOutput("tripratePlot"))
                                      ), # box5
                                      
                                      box(title = "Comparaison des taux journaliers d'arrivées et de départs", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                                          width = 6, height = 520, 
                                          withSpinner(plotlyOutput("triprateclustermeansPlot"))
                                      ), # box6
                                      
                                      box(title = "Positions des stations", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                                          width = 9, height = 500, 
                                          withSpinner(plotlyOutput("triptotalNPlot"))
                                      ) # box7
                                      
                                      
                                      ) # fluidRow1
                                ), # tabitem1 
                              
                              
                              
                              
                              
                              # Second tab content : Stats des
                              #===========================================================================================================================
                              tabItem(tabName = "dashboard",  h2("Widgets tab content"),
                                      fluidRow(
                                        column(width = 4,
                                               box(
                                                 title = " 1 Extraction des données du site divyy", width = NULL, status = "primary",
                                                 "site web divyy,
                                                  2 jeux de données : relevé des stations et des trajets, 
                                                  année 2017, XX Go, 
                                                 XX stations, XX trajets/jour/moyenne"
                                               ),
                                               box(
                                                 title = "Contenu de la base stations", width = NULL, solidHeader = TRUE, status = "primary",
                                                 "identifiant, localisation geographique, capacité, disponibilité des vélos, statut de fonctionnement des bornes, le tout horodaté, 
                                                 "
                                               ),
                                               box(
                                                 title = "Contenu de la base des trajets", width = NULL, background = "black",
                                                 "Identifiant des stations de départ et d'arrivée, localisation des stations de départ et d'arrivée, heures
                                                 d'arrivée et de départ, durée du trajet, identifiant du vélo et identifiant du trajet, statut d'abonnement du cycliste, 
                                                 si abonné information sur l'age et le sexe"
                                               )
                                        ), # column 1
                                        
                                        column(width = 4,
                                               box(
                                                 status = "warning", width = NULL,
                                                 "Box content"
                                               ),
                                               box(
                                                 title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
                                                 "Box content"
                                               ),
                                               box(
                                                 title = "Title 5", width = NULL, background = "light-blue",
                                                 "A box with a solid light-blue background"
                                               )
                                        ), # column 2
                                        
                                        column(width = 4,
                                               box(
                                                 title = "Title 2", width = NULL, solidHeader = TRUE,
                                                 "Box content"
                                               ),
                                               box(
                                                 title = "Title 6", width = NULL, background = "maroon",
                                                 "A box with a solid maroon background"
                                               )
                                        ) # column 3
                                      ) # fluidrow
                        
                                       ),  # tabitem2      
                            
                            
                            
                              # Third tab content : Anissa's stuffs
                              #===========================================================================================================================
                              tabItem(tabName = "TrajetsClusTab",  h2("Clustering des trajets"),
                                      fluidRow( 
                                              box(title = "Paramètres d'entrée", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 3, 
                                                   checkboxGroupInput("jour", label = "Jours",
                                                                      choices=c("Lundi"= 2,  "Mardi"=3, "Mercredi"=4,  "Jeudi"=5,
                                                                                "Vendredi"=6,  "Samedi"=7, "Dimanche"=0),
                                                                      selected = c(2)),     
                                                   
                                                   checkboxGroupInput("unique", label = "Type de trajets",
                                                                      choices=c("Tous"= "tous",  "Unique"="unique", "Non unique"="nonunique"), 
                                                                      selected = c("tous")),  
                                                   
                                                   checkboxGroupInput("methode", label = "Méthode",
                                                                      choices=c( "K-means"=  "kmeans",   "K-means ++"= "kmeanspp", "dbscan"="dbscan"), 
                                                                      selected = c("dbscan")),  
                                                   
                                                   checkboxGroupInput("algo", label = "Algorithme",
                                                                      choices=c( "Hartigan-Wong"=  "Hartigan-Wong",   "Lloyd"= "Lloyd", "Forgy"="Forgy", 
                                                                                 "MacQueen"="MacQueen"), 
                                                                      selected = c("MacQueen")),  
                                                   sliderInput("nbcl",  "Number of clusters:", min = 1L, max = 12L, value = 6L, step = 1L),
                                                  
                                                   sliderInput("ch_minPts", "Pour dbscan: Nombre de pts dans le voisinage", 
                                                                min = 1L, max = 12L, value = 4L, step = 1L),
                                                   
                                                   checkboxGroupInput("maselec", label = "Sélection de variables explicatives",
                                                                      choices = c("Nombre de trajets" = "nb_trips", "Abonnement" = "pct_abonne",
                                                                                  "moy_from_bikes" =  "moy_from_bikes","moy_to_bikes" = "moy_to_bikes",
                                                                                  "moy_from_trips"="moy_from_trips","moy_to_trips" = "moy_to_trips",
                                                                                  "from_nbst_300m"= "from_nbst_300m","to_nbst_300m" = "to_nbst_300m",
                                                                                  "from_nbst_1km"= "from_nbst_1km","to_nbst_1km"="to_nbst_1km",
                                                                                  "moy_distHav"="moy_distHav","moy_trip_duration"="moy_trip_duration",
                                                                                  "start_moy_total_docks"="start_moy_total_docks","start_moy_docks_in_service"="start_moy_docks_in_service",
                                                                                  "start_moy_available_docks"= "start_moy_available_docks","start_moy_pct_full"="start_moy_pct_full",
                                                                                  "stop_moy_total_docks"="stop_moy_total_docks","stop_moy_docks_in_service"="stop_moy_docks_in_service",
                                                                                  "stop_moy_available_docks"="stop_moy_available_docks","stop_moy_pct_full"="stop_moy_pct_full"), 
                                                                      selected =  c("nb_trips", "pct_abonne",
                                                                                    "moy_from_bikes","moy_to_bikes","moy_from_trips","moy_to_trips",
                                                                                    "from_nbst_300m","to_nbst_300m","from_nbst_1km","to_nbst_1km",
                                                                                    "moy_distHav","moy_trip_duration",
                                                                                    "start_moy_total_docks","start_moy_docks_in_service","start_moy_available_docks","start_moy_pct_full",
                                                                                    "stop_moy_total_docks","stop_moy_docks_in_service","stop_moy_available_docks","stop_moy_pct_full"))
                                                                       
                                              ),
                                               box(
                                                 title = "Stations de départ", width = 9, height = 500, solidHeader = TRUE, status = "primary",
                                                 plotlyOutput("trajets_from")
                                               ),
                                               box(
                                                 title = "Stations d'arrivée", width = 9, height = 500, solidHeader = TRUE, status = "primary",
                                                 plotlyOutput("trajets_to")
                                               )
                                      ) # fluidrow
                              ),  # tabitem2      
                              
                            
                            
                            
                            
                            # Third tab content : Magda's stuffs  
                            #==========================================================================================================================
                            tabItem(tabName = "TrajetsGraphTab",  h2("Analyse du réseau"),
                                    fluidRow(
                                      
                                      column(width = 3, 
                                        box(title = "Paramètres d'entrée", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL, 
                                          checkboxGroupInput("Jours", label = "Jours",
                                                                 choices=c("Lundi"= "lundi",  "Mardi"="mardi", "Mercredi"="mercredi",  "Jeudi"="jeudi",
                                                                           "Vendredi"="vendredi",  "Samedi"="samedi", "Dimanche"="dimanche"),
                                                                 selected = c("lundi","mardi","mercredi","jeudi")),     
                                            sliderInput("Choix_heure", label = "Plage horaire",                        min = 0, max = 24,  value = c(5, 9),  width = "100%"),
                                            sliderInput("Nb_tr_min",   label = "Nombre minimum de trajets par heure",  min = 0, max = 0.2, value = 0.05, step = 0.05,  width = "100%"),
                                            radioButtons("methode", "Information affichée",
                                                         choices = list("Degré entrant" = 1, "Degré sortant" = 2,  "Communautés - Algorithme de Louvain" = 3, 
                                                                        "Communautés - Décomposition spectrale" = 4), 
                                                         selected = 1)
                                          ) # box1
                                        ), # column 1
                                      
                                      column(width = 5,
                                            box(title = "Visualisation des trajets", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL, setBackgroundColor("#EBEBEB"),
                                                height = 608, 
                                            visNetworkOutput('Graphe_trajets')), 
                                            
                                            box(title = "Caractéristiques de la station", background = "light-blue", width = NULL, collapsible = TRUE, solidHeader = TRUE, 
                                                #h2("A box with a solid light-blue background"), 
                                                column(10,
                                                       # Un peu de CSS
                                                       # https://www.w3.org/Style/Examples/007/fonts.fr.html
                                                       # https://shiny.rstudio.com/articles/css.html
                                                       tags$style(type = "text/css", "#txtStation {font-family: Georgia, serif; font-weight: 800; color: #FFFFFF}"),
                                                       tags$style(type = "text/css", "#txtBornes {font-family: Georgia, serif; color: #FFFFFF}"),
                                                       tags$style(type = "text/css", "#txtLatitude {font-family: Georgia, serif; color: #FFFFFF}"),
                                                       tags$style(type = "text/css", "#txtLongitude {font-family: Georgia, serif; color: #FFFFFF}"),
                                                       tags$style(type = "text/css", "#txtDeparts {font-family: Georgia, serif; color: #FFFFFF}"),
                                                       tags$style(type = "text/css", "#txtArrivees {font-family: Georgia, serif; color: #FFFFFF}"),
                                                       tags$style(type = "text/css", "#txtMesure {font-family: Georgia, serif; color: #FFFFFF}"),
                                                       fluidRow(textOutput('txtStation')),
                                                       fluidRow(textOutput('txtBornes')),
                                                       fluidRow(textOutput('txtLatitude')),
                                                       fluidRow(textOutput('txtLongitude')), 
                                                       fluidRow(textOutput('txtDeparts')),
                                                       fluidRow(textOutput('txtArrivees'))
                                                       
                                                )) # column #box
                                            ), # column 2
                                      
                                      column(width = 4,
                                            box(title = "Emplacement des stations", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL, height = 850, 
                                            column(width = 12,
                                                   tags$style(type = "text/css", "#Carte_stations {height: calc(80vh) !important;}"),
                                                   leafletOutput('Carte_stations'))
                                      
                                            ) # box3
                                          ) # column 3
                                      
                                      #box(title = "Caractéristiques de la station", background = "light-blue", width = 4, height = 850, collapsible = TRUE, solidHeader = TRUE, 
                                      #      h2("A box with a solid light-blue background")
                                          
                                      
                                      
                                      
                                  
                                    ) # fluidRow3
                            ) # tabitem3
                            
                            
                            ) # tabItems       
                            
) # dashboardbody

))
