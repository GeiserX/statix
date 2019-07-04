.libPaths(c(.libPaths(), "/home/tecnico/R/x86_64-pc-linux-gnu-library/3.2"))
library('rjson')
library('stringr')

while(TRUE){
  tryCatch({
  
    ptm <- proc.time()
  
    # Se podria coger también los MBs gestionados por cada MKT
    
    library("RMySQL")
    radius <- dbConnect(MySQL(), user="root", password="PASSWORD", db="radius", host="X.X.X.X")
    nases <- dbGetQuery(radius, "SELECT nasname, shortname FROM nas ORDER BY shortname")
    query <- dbGetQuery(radius, "SELECT nasipaddress FROM radacct WHERE acctstoptime is NULL")
    servicios <- dbGetQuery(radius, "SELECT calledstationid, framedipaddress FROM radacct WHERE acctstoptime is NULL")
    dbDisconnect(radius)
    detach("package:RMySQL")
    
    write.table(nases, "/home/tecnico/EstadisticasWEB/listaMKT.csv")
    
    ocurrencias <- table(as.factor(query$nasipaddress))
    ocurrencias <- as.data.frame(ocurrencias)
    ocurrencias$DateTime <- as.character(strptime(Sys.time(), "%F %X"))
    names(ocurrencias)[2] <- "PPPoE-Users"
    
    #table(as.factor(servicios$calledstationid))
    telefonos <- sum(servicios$calledstationid == "telefonia")
    ipsInternet <- servicios[servicios$calledstationid == "datos",2]
    ipsInternet <- str_split_fixed(ipsInternet, "\\.", n=3)
    ipsInternet <- paste(ipsInternet[,1], ipsInternet[,2], sep = ".")
    impagosInternet <- sum(ipsInternet == "172.18")
    pagadosInternet <- length(ipsInternet) - impagosInternet
    
    
    library("RMongo")
    mongo <- mongoDbConnect("estadisticas", "localhost", 27017)
    
      for(i in 1:dim(ocurrencias)[1]){
        dbInsertDocument(mongo, as.character(ocurrencias$Var1[i]), sprintf('{\n \"PPPoE-Users\": %s, \n\"DateTime\": \"%s\" \n}', ocurrencias[i,2], ocurrencias[i,3]))
      }
      
    suma <- as.data.frame(sum(ocurrencias$`PPPoE-Users`))
    datos <- cbind(suma, as.character(strptime(Sys.time(), "%F %X")))
    names(datos) <- c("PPPoE-Users", "DateTime")
    dbInsertDocument(mongo, "Total", sprintf("{\n \"PPPoE-Users\": %s,\n\"DateTime\": \"%s\" \n}", datos$`PPPoE-Users`, datos$DateTime))
    
    telefonos <- cbind(as.data.frame(telefonos), as.character(strptime(Sys.time(), "%F %X")))
    names(telefonos) <- c("PPPoE-Users", "DateTime")
    dbInsertDocument(mongo, "Total Telefonia", sprintf("{\n \"PPPoE-Users\": %s,\n\"DateTime\": \"%s\" \n}", telefonos$`PPPoE-Users`, telefonos$DateTime))
    
    impagosInternet <- cbind(as.data.frame(impagosInternet), as.character(strptime(Sys.time(), "%F %X")))
    names(impagosInternet) <- c("PPPoE-Users", "DateTime")
    dbInsertDocument(mongo, "Total Impagos y Bajas", sprintf("{\n \"PPPoE-Users\": %s,\n\"DateTime\": \"%s\" \n}", impagosInternet$`PPPoE-Users`, impagosInternet$DateTime))
    
    pagadosInternet <- cbind(as.data.frame(pagadosInternet), as.character(strptime(Sys.time(), "%F %X")))
    names(pagadosInternet) <- c("PPPoE-Users", "DateTime")
    dbInsertDocument(mongo, "Total Activos", sprintf("{\n \"PPPoE-Users\": %s,\n\"DateTime\": \"%s\" \n}", pagadosInternet$`PPPoE-Users`, pagadosInternet$DateTime))
    
    RMongo::dbDisconnect(mongo)
    detach("package:RMongo")
    
    
    delay <- proc.time() - ptm
    Sys.sleep(5*60 - delay[[3]])
    
  }, error=function(e){
      cat("ERROR :",conditionMessage(e), "\n")
    
      tryCatch({
        RMySQL::dbDisconnect(radius)
        detach("package:RMySQL")
        RMongo::dbDisconnect(mongo)
        detach("package:RMongo")
        Sys.sleep(5*60)
      }, error=function(e){
        tryCatch({
          RMongo::dbDisconnect(mongo)
          detach("package:RMongo")
          Sys.sleep(5*60)
        }, error=function(e){
          Sys.sleep(5*60)
        })
          
      })

    })
}
