#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(mlxR)


shinyUI(dashboardPage(
            dashboardHeader(title = "Les triplettes de Chicago"),
              dashboardSidebar(sidebarMenu(menuItem("Dashboard"             , tabName = "dashboard"      , icon = icon("dashboard")),
                                           menuItem("Stations de Vélos"     , tabName = "StationsTab"    , icon = icon("th")),
                                           menuItem("Clustering des Trajets", tabName = "TrajetsClusTab" , icon = icon("th")),
                                           menuItem("Graphe des trajets"    , tabName = "TrajetsGraphTab", icon = icon("th")),
                                           menuItem("Widgets"               , tabName = "widgets"        , icon = icon("th"))
                                          ) # sidebarMenu
                
                
                
                                  ), # dashboardSidebar 

                          dashboardBody(
                              # Premiere page  
                            tabItems(
                              # First tab content : a homepage? to describe data? 
                              #==========================================================================================================================
                              tabItem(tabName = "dashboard", h2("dashboard tab content")#,
                              #  fluidRow(
                                  
                              #    box(plotOutput("distPlot", height = 200)),
                              #    box(title = "Number of bins", 
                              #        sliderInput("bins",
                              #                    "Number of bins:",
                              #                    min = 1,
                              #                    max = 50,
                              #                    value = 30))
                              #        ) # fluidRow1
                                ), # tabitem1 

                              
                              
                              # First tab content : Jan's stuffs
                              #==========================================================================================================================
                              tabItem(tabName = "StationsTab", h2("Stations tab content")#,
                             #         fluidRow(
                                        
                            #            box(plotOutput("distPlot", height = 200)),
                            #            box(title = "Number of bins", 
                            #                sliderInput("bins",
                            ##                            "Number of bins:",
                            #                            min = 1,
                            #                            max = 50,
                            #                            value = 30))
                            #          ) # fluidRow1
                              ), # tabitem1 
                              
                              
                              
                              
                              
                              # Second tab content : Anissa's stuffs
                              #===========================================================================================================================
                              tabItem(tabName = "widgets",  h2("Widgets tab content"),
                                      fluidRow(
                                        column(width = 4,
                                               box(
                                                 title = "Box title", width = NULL, status = "primary",
                                                 "Box content"
                                               ),
                                               box(
                                                 title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
                                                 "Box content"
                                               ),
                                               box(
                                                 width = NULL, background = "black",
                                                 "A box with a solid black background"
                                               )
                                        ),
                                        
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
                                        ),
                                        
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
                                      )
 
                        
                                       ),  # tabitem2      
                            
                            
                            
                            
                            
                            
                            
                            
                            # Third tab content : Magda's stuffs  
                            #==========================================================================================================================
                            tabItem(tabName = "TrajetsGraphTab",
                                    fluidRow(
                                      
                                      column(width = 3, 
                                        box(title = "Inputs (find another name)", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL, 
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
                                            box(title = "Graphes", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL, setBackgroundColor("#EBEBEB"),
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
                                            box(title = "Carte", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL, height = 850, 
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
