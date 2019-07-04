# shiny::runApp(appDir = "/home/tecnico/EstadisticasWEB", launch.browser = FALSE, port = 8080, host = "0.0.0.0")


library(shiny)

shinyUI(navbarPage("StatiX", id="navbar", position="static-top", inverse=F, theme = "bootstrap.css",
                   
                   
        
        tabPanel("Usuarios PPPoE", value=1, 
                 

            #headerPanel("Usuarios PPPoE en cada MKT"),
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"),
            
            

      
            mainPanel( width = 12,
                plotOutput("distPlot", height = 550),
                splitLayout(
                  uiOutput("rangeDate1"),
                  textOutput("Nactual")
                )
#                 sliderInput(inputId = "rango",label =  "Escoja rango: ", timeFormat = "%F %T", width = "95%",
#                             min = as.POSIXlt("2015-09-14 09:45:00"), max = as.POSIXlt(Sys.time()+60*60), value = c(as.POSIXlt(Sys.time()-60*60*24), as.POSIXlt(Sys.time())))
                #actionButton("reboot", "Reiniciar el servidor", icon("refresh", "font-awesome"))
            ),

            sidebarPanel( width= 12, 
              uiOutput("MKTs")
              #selectInput("fechas", "Escoja tiempo de representación:", c("3 Horas", "12 Horas", "1 Día", "3 Días", "1 Semana", "1 Mes", "Origen de los tiempos"), "3 Horas")
            )

        ),
        
        
        tabPanel("Usuarios por Servicio", value=2, 
                 
                 
                 #headerPanel("Total de usuarios PPPoE por Servicio"),
                 mainPanel(width=12,
                           plotOutput("servicePLOT", height = 550),
                           splitLayout(
                             uiOutput("rangeDate2"),
                             textOutput("NactualService")
                           )
                 ),
                 
                 sidebarPanel(width=12,
                   selectInput("servicio", "Escoja Servicio", 
                               setNames(c("Total Activos", "Total Impagos y Bajas", "Total Telefonia"),
                                        c("Activos Internet", "Impagos y Bajas", "Telefonia")),
                               multiple = FALSE, selectize = TRUE, selected = "Total Activos")
                 )
                 
                 
                 
        ),
        
        
        tabPanel("Tráfico", value=3, 
                 
                 
                 #headerPanel("Tráfico en Area Border Routers"),
                 mainPanel( width=12,
                            plotOutput("PlotTrafico", height = 550),
                            splitLayout(
                              uiOutput("rangeDate3"),
                              textOutput("NTrafico")
                            )
                 ),
                 
                 sidebarPanel( width=12,
                   selectInput("salida", "Escoja ABR", 
                               setNames(c("Total", "Ono", "Aire"),
                                        c("Total", "Ono", "Aire Networks")),
                               multiple = FALSE, selectize = TRUE, selected = "Total")
                 )
                 
                 
                 
        ),

        tabPanel("Usuarios HotSpot", value=12, 
                 
                 
                 #headerPanel("Usuarios HotSpot en cada MKT"),
                 
                 
                 
                 
                 mainPanel(width=12,
                   plotOutput("distPlotHS", height = 550),
                   splitLayout(
                     
                     uiOutput("rangeDate4"),
                     textOutput("NactualHotSpot")
                   )
                 ),
                 sidebarPanel(width=12,
                              uiOutput("HSMKTs")
                 )
        ),
        
        tabPanel("Búsqueda de usuarios HotSpot", value=5, 
                 
                 
                 headerPanel("Búsqueda de Usuarios en cada HotSpot"),
                 
                 sidebarPanel(width=2,
                     uiOutput("searcher")
#                      textInput("client", "Usuario Hotspot: "),
#                      actionButton("search", "Buscar")
                 ),
                 
                 
                 mainPanel(
                   dataTableOutput("dataTABLE")
                 )
        )
        
))
