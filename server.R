# shiny::runApp(appDir = "/home/tecnico/EstadisticasWEB", launch.browser = FALSE, port = 8080, host = "0.0.0.0")

library("RMongo")
library("dplyr")
library("ggplot2")
library("shiny")
library("scales")
library("stringr")

shinyServer(function(input, output, session) {
  
  
  output$rangeDate1 <- renderUI({
    invalidateLater(1000*60*60, session)
    dateRangeInput("rangeDate", "Escoja rango de fechas", start = Sys.Date()-1, end = Sys.Date()+1,
                   min = "2015-09-14",
                   max = Sys.Date()+1,
                   format = "yyyy-mm-dd", startview = "day", weekstart = 1,
                   language = "es", separator = " hasta ")
  })
  
  output$rangeDate2 <- renderUI(({
    invalidateLater(1000*60*60, session)
    dateRangeInput("rangeDateServicios", "Escoja rango de fechas", start = Sys.Date()-1, end = Sys.Date()+1,
                   min = "2015-09-21",
                   max = Sys.Date()+1,
                   format = "yyyy-mm-dd", startview = "day", weekstart = 1,
                   language = "es", separator = " hasta ")
  }))
  
  output$rangeDate3 <- renderUI({
    invalidateLater(1000*60*60, session)
    dateRangeInput("rangeDateTrafico", "Escoja rango de fechas", start = Sys.Date()-1, end = Sys.Date()+1,
                   min = "2015-11-25",
                   max = Sys.Date()+1,
                   format = "yyyy-mm-dd", startview = "day", weekstart = 1,
                   language = "es", separator = " hasta ")
  })
  
  output$rangeDate4 <- renderUI({
    invalidateLater(1000*60*60, session)
    dateRangeInput("rangeDateHS", "Escoja rango de fechas", start = Sys.Date()-1, end = Sys.Date()+1,
                   min = "2015-10-22",
                   max = Sys.Date()+1,
                   format = "yyyy-mm-dd", startview = "day", weekstart = 1,
                   language = "es", separator = " hasta ")
  })
  
  output$MKTs <- renderUI({
      
    listaMKT <- read.table("listaMKT.csv", header = T, sep = " ", stringsAsFactors = F)
    choices <- setNames(c("Total", listaMKT$nasname), c("Gran total", listaMKT$shortname))
    
    selectInput("inputMKT", "Escoja MikroTik", choices, multiple = FALSE, selectize = TRUE, selected = "Total")
    
  })
  
  ####################################################################################
  #################################### PPPoE #########################################
  ####################################################################################
  
  output$distPlot <- renderPlot({
    
    invalidateLater(1000*60*5, session)
    
    print("-------------------------")
    print(input$rangeDate)
    print(input$inputMKT)
    print(Sys.time())
    
    mongo <- RMongo::mongoDbConnect("estadisticas", "localhost", 27017)
    
#     query <- vector("list", length=length(input$inputMKT))
#     for(i in 1:length(input$inputMKT)){
#       #query[[i]] <- dbGetQueryForKeys(mongo, input$inputMKT[i], skip = 0, limit = 1000000, 
#       #sprintf('{ "DateTime": { $regex : "%s" }}', '2015-11-08'), '{ "DateTime":1, "PPPoE-Users":1, "_id":0 }')
#       query[[i]] <- RMongo::dbGetQueryForKeys(mongo, input$inputMKT[i], skip = 0, limit = 1000000, '{  }', '{ "DateTime":1, "PPPoE-Users":1, "_id":0 }')
#       query[[i]] <- query[[i]][-3]
#       query[[i]]$DateTime <- as.POSIXct(strptime(query[[i]]$DateTime, "%Y-%m-%d %H:%M:%S"))
#     }
    
    query <- RMongo::dbGetQueryForKeys(mongo, input$inputMKT, skip = 0, limit = 1000000, '{  }', '{ "DateTime":1, "PPPoE-Users":1, "_id":0 }')
    query <- query[-3]
    query$DateTime <- as.POSIXct(strptime(query$DateTime, "%Y-%m-%d %H:%M:%S"))
    
    RMongo::dbDisconnect(mongo)
    merged <- query
    
#     if(length(input$inputMKT) >= 2){
#       for(i in 2:length(input$inputMKT)){
#         merged <- merge(merged, query[[i]], by="DateTime")
#       }
#       merged$PPPoE.Users.x <- rowSums(merged[,-1])
#       merged <- merged[,1:2]
#       names(merged)[2] <- "PPPoE.Users"
#     }

    # rangeDate <- c(as.Date("2016-01-31"), as.Date("2016-02-02"))
    limit <- input$rangeDate[1]
    
    if(!(input$rangeDate[2] == Sys.Date()+1)){
      limit <- c(limit, input$rangeDate[2])
    } else { limit <- c(limit, NA) }
    
    limit <- as.POSIXct(limit)
    
    output$Nactual <- renderText(paste0("Usuarios activos actuales: ", merged$PPPoE.Users[length(merged$PPPoE.Users)]))
    
    merged <- as.data.frame(merged)
    pp <- ggplot(merged, aes(x=DateTime, y=PPPoE.Users, group=1)) + 
      geom_line(color="blue", size = 0.8) +
      labs(x="Time", y="PPPoE Users") +
      ggtitle(input$inputMKT) +
      scale_y_continuous(breaks= pretty_breaks(n = 10)) + #, limits = c(2900,3300)
      scale_x_datetime(limits=limit) +
      #coord_fixed(ratio = 1000) +
      geom_smooth(method="lm", color="red", size=0.7, se = FALSE) +
      theme(panel.grid.minor = element_line(colour = "black", linetype = "dotted"), 
            panel.grid.major = element_line(colour = "grey50"),
            panel.background = element_rect(fill = "grey92") #http://sape.inf.usi.ch/quick-reference/ggplot2/colour
            )
    
      
      if(is.na(limit[2])) limit[2] <- as.POSIXct(strsplit(x = as.character(tail(merged$DateTime,1)), split = " ")[[1]][1]) # la ultima fecha, no hora
      
      fechasplit <- strsplit(as.character(merged$DateTime), split = " ")
      fechafinal <- lapply(fechasplit, '[[', 1)
      
      limiteN <- 0
      limiteN[1] <- which(as.character(as.Date(limit[1], tz = "Europe/Madrid")) == fechafinal)[1]
      limiteN[2] <- tail(which(as.character(as.Date(limit[2], tz = "Europe/Madrid")) == fechafinal), 1)
      if(is.na(limiteN[1]) || is.na(limiteN[2])) pp
      else pp + coord_cartesian(ylim = c(max(c(min( merged$PPPoE.Users[limiteN[1]:limiteN[2]] )-9, 0)), max( merged$PPPoE.Users[limiteN[1]:limiteN[2]] )+9))

  })
  
  ####################################################################################
  ################################## SERVICIOS #######################################
  ####################################################################################
  
  output$servicePLOT <- renderPlot({
    
    invalidateLater(1000*60*5, session)
    
    print("-------------------------")
    print(input$rangeDateServicios)
    print(input$servicio)
    print(Sys.time())
    
    mongo <- mongoDbConnect("estadisticas", "localhost", 27017)
    
    query <- vector("list", length=length(input$servicio))
    for(i in 1:length(input$servicio)){
      query[[i]] <- dbGetQueryForKeys(mongo, input$servicio[i], skip = 0, limit = 1000000,
                                      '{}', '{ "DateTime":1, "PPPoE-Users":1, "_id":0 }')
      query[[i]] <- query[[i]][-3]
      query[[i]]$DateTime <- as.POSIXct(strptime(query[[i]]$DateTime, "%Y-%m-%d %H:%M:%S"))
    }
    
    merged <- query[[1]]
    if(length(input$servicio) >= 2){
      for(i in 2:length(input$servicio)){
        merged <- merge(merged, query[[i]], by="DateTime")
      }
      merged$PPPoE.Users.x <- rowSums(merged[,-1])
      merged <- merged[,1:2]
      names(merged)[2] <-  "PPPoE.Users"
    }
    
    limit <- input$rangeDateServicios[1]
    
    if(!(input$rangeDateServicios[2] == Sys.Date()+1)){
      limit <- c(limit, input$rangeDateServicios[2])
    } else { limit <- c(limit, NA) }
    
    limit <- as.POSIXct(limit)
    
    output$NactualService <- renderText(paste0("Usuarios activos actuales: ", merged$PPPoE.Users[length(merged$PPPoE.Users)]))
    
    pp <- ggplot(as.data.frame(merged), aes(x=DateTime, y=PPPoE.Users, group=1)) + 
      geom_line(color="blue", size=0.8) +
      labs(x="Time", y="PPPoE Users") +
      ggtitle(input$servicio) +
      scale_y_continuous(breaks= pretty_breaks(n = 10)) +
      scale_x_datetime(limits=limit) +
      geom_smooth(method="lm", color="red", size=0.7, se=FALSE) +
      theme(panel.grid.minor = element_line(colour = "black", linetype = "dotted"), 
            panel.grid.major = element_line(colour = "grey50"),
            panel.background = element_rect(fill = "grey92") #http://sape.inf.usi.ch/quick-reference/ggplot2/colour
      )
    
    if(is.na(limit[2])) limit[2] <- as.POSIXct(strsplit(x = as.character(tail(merged$DateTime,1)), split = " ")[[1]][1]) # la ultima fecha, no hora
    
    fechasplit <- strsplit(as.character(merged$DateTime), split = " ")
    fechafinal <- lapply(fechasplit, '[[', 1)
    
    limiteN <- 0
    limiteN[1] <- which(as.character(as.Date(limit[1], tz = "Europe/Madrid")) == fechafinal)[1]
    limiteN[2] <- tail(which(as.character(as.Date(limit[2], tz = "Europe/Madrid")) == fechafinal), 1)
    if(is.na(limiteN[1]) || is.na(limiteN[2])) pp
    else pp + coord_cartesian(ylim = c(max(c(min( merged$PPPoE.Users[limiteN[1]:limiteN[2]] )-9, 0)), max( merged$PPPoE.Users[limiteN[1]:limiteN[2]] )+9))
    
    
  })
  
  ####################################################################################
  ################################### TRÁFICO ########################################
  ####################################################################################
  
  output$PlotTrafico <- renderPlot({
    
    invalidateLater(1000*60*5, session)
    
    print("-------------------------")
    print(input$rangeDateTrafico)
    print(input$salida)
    print(Sys.time())
    
    mongo <- RMongo::mongoDbConnect("Trafico", "localhost", 27017)
    
    if(input$salida == "Aire"){
      Aire1 <- RMongo::dbGetQueryForKeys(mongo, "Aire1", skip = 0, limit = 1000000,
                                         '{}', '{ "DateTime":1, "SpeedIN":1, "SpeedOUT":1, "_id":0 }')
      Aire2 <- RMongo::dbGetQueryForKeys(mongo, "Aire2", skip = 0, limit = 1000000,
                                         '{}', '{ "DateTime":1, "SpeedIN":1, "SpeedOUT":1, "_id":0 }')
      merged <- merge(Aire1, Aire2, by="DateTime")
    
      merged <- merged[,c(-4,-7)]
      
      merged$SpeedIN <- rowSums(merged[,c(-1, -3, -5)])
      merged$SpeedOUT <- rowSums(merged[,c(-1, -2, -4, -6)])
      merged <- merged[, c(-2,-3,-4,-5)]
      
    } else {
      merged <- RMongo::dbGetQueryForKeys(mongo, input$salida, skip = 0, limit = 1000000,
                                          '{}', '{ "DateTime":1, "SpeedIN":1, "SpeedOUT":1, "_id":0 }')
      merged <- merged[-4]
    }
    merged$DateTime <- as.POSIXct(strptime(merged$DateTime, "%Y-%m-%d %H:%M:%S"))
    limit <- input$rangeDateTrafico[1]
    
    if(!(input$rangeDateTrafico[2] == Sys.Date()+1)){
      limit <- c(limit, input$rangeDateTrafico[2])
    } else { limit <- c(limit, NA) }
    
    limit <- as.POSIXct(limit)
    
    output$NTrafico <- renderText(paste0("Tráfico actual - Entrada: ", formatC(as.numeric(merged$SpeedIN[length(merged$SpeedIN)]), digits = 4),
                                         " Mbit/s. Salida: ", formatC(as.numeric(merged$SpeedOUT[length(merged$SpeedOUT)]), digits = 4), " Mbit/s."))

    if(input$salida == "Total") {
      trafMAX <- 1560
      trafWARNING <- 1000*0.75 + 560
    } else if(input$salida == "Ono"){
      trafMAX <- 1000
      trafWARNING <- 1000*0.75
    } else if(input$salida == "Aire"){
      trafMAX <- 720
      trafWARNING <- 560
    }
    
    
    pp <- ggplot(as.data.frame(merged), aes(x=DateTime)) + 
      geom_line(aes(y=SpeedIN, colour="SpeedIN"), size=0.8) +
      geom_line(aes(y=SpeedOUT, colour="SpeedOUT"), size=0.8) +
      labs(x="Time", y="Traffic") +
      ggtitle("Tráfico de red") +
      geom_hline(yintercept = trafMAX, color="purple", size=1) +
      geom_hline(yintercept = trafWARNING, color = "orange", size=1) +
      scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      scale_x_datetime(limits = limit) +
      theme(panel.grid.minor = element_line(colour = "black", linetype = "dotted"), 
            panel.grid.major = element_line(colour = "grey50"),
            panel.background = element_rect(fill = "grey92") #http://sape.inf.usi.ch/quick-reference/ggplot2/colour
      )
    
    if(is.na(limit[2])) limit[2] <- as.POSIXct(strsplit(x = as.character(tail(merged$DateTime,1)), split = " ")[[1]][1]) # la ultima fecha, no hora
    
    fechasplit <- strsplit(as.character(merged$DateTime), split = " ")
    fechafinal <- lapply(fechasplit, '[[', 1)
    
    limiteN <- 0
    limiteN[1] <- which(as.character(as.Date(limit[1])) == fechafinal)[1]
    limiteN[2] <- tail(which(as.character(as.Date(limit[2])) == fechafinal), 1)
    
    if(is.na(limiteN[1]) || is.na(limiteN[2])) pp
    else pp + coord_cartesian(ylim = c(0, max( merged$SpeedIN[limiteN[1]:limiteN[2]] )+49))
    
    
  })
  
  ####################################################################################
  ################################### HOTSPOT ########################################
  ####################################################################################
  
  output$HSMKTs <- renderUI({
    
    listaHS <- read.table("listaHS.csv", header = T, sep = " ", stringsAsFactors = F)
    listaHS$names <- paste(listaHS$names, listaHS$ips, " ")
    choices <- setNames(c("Total", listaHS$ips), c("Total", listaHS$names))
    
    selectInput("inputMKTHS", "Escoja HotSpot", choices, multiple = FALSE, selectize = TRUE, selected = "Total")
    
  })
  
  output$distPlotHS <- renderPlot({
    
    invalidateLater(1000*60*5, session)
    
    print("-------------------------")
    print(input$rangeDate)
    print(input$inputMKT)
    print(Sys.time())
    
    mongo <- mongoDbConnect("estadisticasHS", "localhost", 27017)
    
    query <- vector("list", length=length(input$inputMKTHS))
    for(i in 1:length(input$inputMKTHS)){
      query[[i]] <- dbGetQueryForKeys(mongo, input$inputMKTHS[i], skip = 0, limit = 1000000,
                                      '{}', '{ "DateTime":1, "HS-Users":1, "_id":0 }')
      query[[i]] <- query[[i]][-3]
      query[[i]]$DateTime <- as.POSIXct(strptime(query[[i]]$DateTime, "%Y-%m-%d %H:%M:%S"))
    }
    
    merged <- query[[1]]
    if(length(input$inputMKTHS) >= 2){
      for(i in 2:length(input$inputMKTHS)){
        merged <- merge(merged, query[[i]], by="DateTime")
      }
      merged$HS.Users.x <- rowSums(merged[,-1])
      merged <- merged[,1:2]
      names(merged)[2] <-  "HS.Users"
    }
    
    limit <- input$rangeDateHS[1]
    if(!(input$rangeDateHS[2] == Sys.Date()+1)){
      limit <- c(limit, input$rangeDateHS[2])
    } else { limit <- c(limit, NA) }
    limit <- as.POSIXct(limit)
    
    output$NactualHotSpot <- renderText(paste0("Usuarios activos actuales: ", merged$HS.Users[length(merged$HS.Users)]))
    
    pp <- ggplot(as.data.frame(merged), aes(x=DateTime, y=HS.Users, group=1)) + 
      geom_line(size=0.8, color="blue") +
      labs(x="Time", y="HS Users") +
      ggtitle(input$inputMKTHS) +
      scale_y_continuous(breaks= pretty_breaks()) +
      scale_x_datetime(limits=limit) +
      theme(panel.grid.minor = element_line(colour = "black", linetype = "dotted"), 
            panel.grid.major = element_line(colour = "grey50"),
            panel.background = element_rect(fill = "grey92") #http://sape.inf.usi.ch/quick-reference/ggplot2/colour
      )
      
    if(is.na(limit[2])) limit[2] <- as.POSIXct(strsplit(x = as.character(tail(merged$DateTime,1)), split = " ")[[1]][1]) # la ultima fecha, no hora
    
    fechasplit <- strsplit(as.character(merged$DateTime), split = " ")
    fechafinal <- lapply(fechasplit, '[[', 1)
    
    limiteN <- 0
    limiteN[1] <- which(as.character(as.Date(limit[1], tz = "Europe/Madrid")) == fechafinal)[1]
    limiteN[2] <- tail(which(as.character(as.Date(limit[2], tz = "Europe/Madrid")) == fechafinal), 1)
    if(is.na(limiteN[1]) || is.na(limiteN[2])) pp
    else pp + coord_cartesian(ylim = c(max(min( merged$HS.Users[limiteN[1]:limiteN[2]] )-4,0 ), max( merged$HS.Users[limiteN[1]:limiteN[2]] )+2))
    
    
    
  })
  
  ####################################################################################
  ################################## Search HS #######################################
  ####################################################################################
  
  output$searcher <- renderUI({
    mongo <- mongoDbConnect("usuariosHS", "localhost", 27017)
    choices <- dbShowCollections(mongo)
    choices <- choices[order(choices)]
    
    selectInput("search", "Usuario Hotspot: ", choices, multiple = FALSE, selectize = TRUE, selected = NULL)
  })
  
  
  
  observeEvent(
    input$search,
    {
      output$dataTABLE <- renderDataTable({
        mongo <- mongoDbConnect("usuariosHS", "localhost", 27017)
        
        query <- dbGetQueryForKeys(
          mongo, input$search, skip = 0, limit = 1000000,
          '{}', '{ "macs":1, "address":1, "loginby":1, "uptime":1, "bytesin":1, "bytesout":1, "loc":1, "signal":1, "DateTime":1, "_id":0 }'
        )
        
        query <- query[,-10]
        query <- query[order(query$DateTime, decreasing = T),]
      }, options = list(pageLength = 8))
    }
  )

  
})
