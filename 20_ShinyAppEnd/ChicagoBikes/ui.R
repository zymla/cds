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


shinyUI(dashboardPage(skin = "blue", 
            dashboardHeader(title = "Les triplettes de Chicago", titleWidth = 300),
              dashboardSidebar(sidebarMenu(menuItem("Description des données"   , tabName = "dashboard"      , icon = icon("info")),
                                           menuItem("Typologie des stations"    , tabName = "StationsTab"    , icon = icon("bicycle")),
                                           menuItem("Typologie des trajets"     , tabName = "TrajetsClusTab" , icon = icon("bicycle")),
                                           menuItem("Analyse du réseau"         , tabName = "TrajetsGraphTab", icon = icon("bicycle"))
                                          ),  # sidebarMenu
                               width = 300
                                  ), # dashboardSidebar 

                          dashboardBody(
                              # Premiere page  
                            tabItems(
                              # First tab content : a homepage? to describe data? 
                              #==========================================================================================================================
                              tabItem(tabName = "dashboard", h2("Description des données"),
                                      fluidRow(
                                        column(width = 4,
                                               box(title = " Extraction des données du site Divvy",
                                                   solidHeader = TRUE, width = NULL, status = "primary",
                                                   tags$b("Deux jeux de données :"), br(), 
                                                   " - Activité des stations: 6 Go", br(), 
                                                   " - Description des trajets: 1 Go", br(), br(), 
                                                  "Données de l'année 2017", br(), 
                                                  uiOutput("tab")
                                               ),
                                               box(
                                                 title = "Contenu de la base des stations", width = NULL, solidHeader = TRUE, status = "primary",
                                                 tags$b("587 stations entre 11 et 55 bornes"), br(),
                                                 "Min : 11, Max = 55, Médiane = 15", br(), br(), 
                                                 tags$b("Données disponibles :"), br(), 
                                                 " - Identifiant", br(), 
                                                 " - Localisation géographique", br(), 
                                                 " - Capacité", br(), 
                                                 " - Disponibilité des vélos", br(),
                                                 " - Statut de fonctionnement des bornes", br(),
                                                 " - Le tout horodaté"
                                               ),
                                               box( title = "Contenu de la base des trajets", 
                                                    width = NULL, solidHeader = TRUE, status = "primary", 
                                                    "3.8 millions trajets", br(), 
                                                    "Min : 240/jour, Max = 22106/jour, Médiane = 9367/jour", br(), br(), 
                                                    tags$b("Données disponibles :"), br(), 
                                                    " - Identifiant des stations de départ et d'arrivée", br(), 
                                                    " - Localisation des stations de départ et d'arrivée", br(),
                                                    " - Heures d'arrivée et de départ", br(),
                                                    " - Durée du trajet", br(), 
                                                    " - Identifiant du vélo et identifiant du trajet", br(), 
                                                    " - Statut d'abonnement du cycliste", br(),
                                                    " - Age et sexe pour les abonnés : 78 % des trajets "
                                               )
                                               ), # column 1
                                        
                                        column(width = 8,
                                               box(title = "Distribution du nombre quotidien de trajets par mois",
                                                   solidHeader = TRUE, width = NULL, status = "primary", height = 500, 
                                                   plotOutput("trajets_par_jours")
                                                   #img(src = "gplot.png", height="50%", width="80%")
                                               ), # box
                                               box(title = "Nombre de trajets moyens par heure",
                                                   solidHeader = TRUE, width = NULL, status = "primary", height = 500, 
                                                   plotOutput("trajets_par_heure")
                                                   #img(src = "Nb_trajets_moyen.png", height="50%", width="80%")
                                               ) # box
                                        ) # column 2
                                      
                                      ) # fluidrow
                                      
                                ), # tabitem1 

                              # First tab content : Jan's stuffs
                              #==========================================================================================================================
                              tabItem(tabName = "StationsTab", h2("Analyse des stations"),
                                    fluidRow(
                                      box(title = "Paramètres d'entrée", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 3, height = 991,             
                                          #actionButton("flush", label = "Flush data to disk"),
                                          actionButton("recompute", label = "Re-calcul"),
                                          checkboxInput("Mon", "Lundi", TRUE),
                                          checkboxInput("Tue", "Mardi", TRUE),
                                          checkboxInput("Wed", "Mercredi", TRUE),
                                          checkboxInput("Thu", "Jeudi", TRUE),
                                          checkboxInput("Fri", "Vendredi", FALSE),
                                          checkboxInput("Sat", "Samedi", FALSE),
                                          checkboxInput("Sun", "Dimanche", FALSE),
                                          sliderInput("nb_clusters",  "Nombre de clusters:",  min = 1L,  max = 16L, value = 12L, step = 1L),                                                                                                          
                                          sliderInput("low_trips_thd",  " Regrouper les stations ayant moins de N trajets:",  min = 10,  max = 1000, value = 500),
                                          radioButtons("kmeansAlgo", label = "Algorithme k-means", choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), select = "Hartigan-Wong"),
                                          sliderInput("alpha",  "Transparence du graphe : valeur de Alpha:",   min = 0,   max = 1,  value = .1),
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
                                      ) # box6
                                      
      
                                      
                                      
                                      ) # fluidRow1
                                ), # tabitem1 
 
                              
                              # Third tab content : Anissa's stuffs
                              #===========================================================================================================================
                              tabItem(tabName = "TrajetsClusTab",  h2("Typologie des trajets"),
                                      fluidRow( 
                                              box(title = "Paramètres d'entrée", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 3, 
                                                    # checkboxGroupInput("jour", label = "Jours",
                                                    #                  choices=c("Lundi"= 2,  "Mardi"=3, "Mercredi"=4,  "Jeudi"=5,
                                                    #                            "Vendredi"=6,  "Samedi"=7, "Dimanche"=1),
                                                    #                  selected = c(2)),     
                                                  radioButtons("jour", label = "Jours",
                                                               choices = c("Lundi"= 2,  "Mardi"=3, "Mercredi"=4,  "Jeudi"=5,
                                                                           "Vendredi"=6,  "Samedi"=7, "Dimanche"=1), select = c(5)),
                                                  
                                                  radioButtons("methode_anissa", label = "Méthode",
                                                               choices=c( "K-means"=  "kmeans", "dbscan"="dbscan", 
                                                                          "clara" = "clara"), 
                                                               selected = c("kmeans")),  
                                                  
                                                  
                                                  conditionalPanel(
                                                                  condition = "input.methode_anissa == 'kmeans' || input.methode_anissa == 'clara'", 
                                                                  sliderInput("nbcl",  "Nombre de clusters:", min = 1L, max = 12L, value = 6L, step = 1L)),
    
                                                  conditionalPanel(
                                                                  condition = "input.methode_anissa == 'kmeans'", 
                                                                  radioButtons("algo", label = "Algorithme k-means", 
                                                                               choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), 
                                                                              select = "MacQueen")), 
                                                  
                
                                                  conditionalPanel(
                                                                  condition = "input.methode_anissa == 'dbscan'", 
                                                                  sliderInput("ch_minPts", "Nombre de points dans le voisinage", 
                                                                  min = 1L, max = 12L, value = 4L, step = 1L)
                                                                  ), # conditional panel 
                                                  
                                                  conditionalPanel(  
                                                                     condition = "input.methode_anissa == 'clara'", 
                                                                     radioButtons("distm", label = "Distance Minimale",
                                                                     choices=c( "euclidean"="euclidean", "manhattan" = "manhattan", 
                                                                                "jaccard" = "jaccard"), 
                                                                     selected = c("euclidean"))),  
                                                  
                                                  radioButtons("unique", label = "Type de trajets",
                                                               choices=c("Tous"= "tous",  "Unique"="unique", "Non unique"="nonunique"), 
                                                               selected = c("tous")),  
                                                   checkboxGroupInput("maselec", label = "Sélection de variables explicatives",
                                                                      choices = c("Nombre de trajets" = "nb_trips", 
                                                                                  "Pourcentage de trajets realisés par les abonnés" = "pct_abonne",
                                                                                  "Distance moyenne du trajet"="moy_distHav",
                                                                                  "Durée moyenne du trajet"="moy_trip_duration",
                                                                                  
                                                                                  "Départ : nombre de vélos distincts" =  "moy_from_bikes",
                                                                                  "Départ : nombre moyen de trajets différents"="moy_from_trips",
                                                                                  "Départ : nombre de stations dans un rayon de 300m"= "from_nbst_300m",
                                                                                  "Départ : nombre de stations dans un rayon de 1km"= "from_nbst_1km",
                                                                                  "Départ : nombre de stations dans un rayon de 2km"= "from_nbst_2km",
                                                                                  "Départ : nombre de bornes total"="start_moy_total_docks",
                                                                                  "Départ : nombre moyen de bornes en service"="start_moy_docks_in_service",
                                                                                  "Départ : nombre moyen de bornes disponibles"= "start_moy_available_docks",
                                                                                  "Départ : taux de remplissage moyen"="start_moy_pct_full",
                                                                                  "Départ : taux de remplissage moyen des bornes fonctionnelles"="start_moy_pct_full_av",
                                                                                  
                                                                                  "Arrivée : nombre de vélos distincts" = "moy_to_bikes",                                                                                  
                                                                                  "Arrivée : nombre moyen de trajets différents" = "moy_to_trips",                                                                                  
                                                                                  "Arrivée : nombre de stations dans un rayon de 300m" = "to_nbst_300m",                                                                                
                                                                                  "Arrivée : nombre de stations dans un rayon de 1km"="to_nbst_1km",
                                                                                  "Arrivée : nombre de stations dans un rayon de 2km"="to_nbst_2km",        
                                                                                  "Arrivée : nombre de bornes total"="stop_moy_total_docks",
                                                                                  "Arrivée : nombre moyen de bornes en service"="stop_moy_docks_in_service",
                                                                                  "Arrivée : nombre moyen de bornes disponibles"="stop_moy_available_docks",
                                                                                  "Arrivée : taux de remplissage moyen"="stop_moy_pct_full", 
                                                                                  "Arrivée : taux de remplissage moyen des bornes fonctionnelles"="stop_moy_pct_full_av"
                                                                                  ), 
                                                                    
                                                                        selected =  c("nb_trips", "pct_abonne",
                                                                                    "moy_from_bikes","moy_to_bikes",
                                                                                    "moy_from_trips","moy_to_trips",
                                                                                    "from_nbst_300m","to_nbst_300m",
                                                                                    "from_nbst_1km","to_nbst_1km",
                                                                                    "moy_distHav","moy_trip_duration",
                                                                                    "start_moy_total_docks","start_moy_docks_in_service",
                                                                                    "start_moy_available_docks","start_moy_pct_full",
                                                                                    "stop_moy_total_docks","stop_moy_docks_in_service",
                                                                                    "stop_moy_available_docks","stop_moy_pct_full"))
                                                                       
                                              ),
                                               box(
                                                 title = "Classes de trajets en fonction de la station de départ", width = 9, height = 500, solidHeader = TRUE, status = "primary",
                                                 plotlyOutput("trajets_from")
                                               ),
                                               box(
                                                 title = "Classes de trajets en fonction de la station d'arrivée", width = 9, height = 500, solidHeader = TRUE, status = "primary",
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
                                          actionButton("recompute_magda", label = "Re-calcul"),
                                          checkboxGroupInput("Jours", label = "Jours",
                                                                 choices=c("Lundi"= "lundi",  "Mardi"="mardi", "Mercredi"="mercredi",  "Jeudi"="jeudi",
                                                                           "Vendredi"="vendredi",  "Samedi"="samedi", "Dimanche"="dimanche"),
                                                                 selected = c("lundi","mardi","mercredi","jeudi", "vendredi")),     
                                            sliderInput("Choix_heure", label = "Plage horaire",                        min = 0, max = 24,  value = c(15, 20),  width = "100%"),
                                            sliderInput("Nb_tr_min",   label = "Nombre minimum de trajets par heure",  min = 0, max = 0.2, value = 0.05, step = 0.05,  width = "100%"),
                                            radioButtons("methode", "Information affichée",
                                                         choices = list("Degré entrant" = 1, "Degré sortant" = 2,  "Communautés - Algorithme de Louvain" = 3, 
                                                                        "Communautés - Décomposition spectrale" = 4), 
                                                         selected = 2)
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
