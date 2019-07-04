.libPaths(c(.libPaths(), "/home/tecnico/R/x86_64-pc-linux-gnu-library/3.2"))
library("rmongodb")
library('stringr')

limpiaSNMP <- function(x) return(str_extract(x, "([^ ]*)$"))

### INICIO ###

ptm <- proc.time()

# Ono #
onoIN0 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
onoOUT0 <-  as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))

# Cabecera 1 #
aire1IN0 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
aire1OUT0 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))

# Cabecera 2 #
aire2IN0 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
aire2OUT0 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))

delay <- proc.time() - ptm
Sys.sleep(30 - delay[[3]])
ptm <- proc.time()

# Ono #
onoIN1 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
onoOUT1 <-  as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))

# Cabecera 1 #
aire1IN1 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
aire1OUT1 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))

# Cabecera 2 #
aire2IN1 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
aire2OUT1 <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))

DateTime <- as.character(strptime(Sys.time(), "%F %X"))

onoINv <- (onoIN1 - onoIN0)*8/(30*1024*1024) # Para ponerlo en Mbit/s
onoOUTv <- (onoOUT1 - onoOUT0)*8/(30*1024*1024) # Para ponerlo en Mbit/s
aire1INv <- (aire1IN1 - aire1IN0)*8/(30*1024*1024) # Para ponerlo en Mbit/s
aire1OUTv <- (aire1OUT1 - aire1OUT0)*8/(30*1024*1024) # Para ponerlo en Mbit/s
aire2INv <- (aire2IN1 - aire2IN0)*8/(30*1024*1024) # Para ponerlo en Mbit/s
aire2OUTv <- (aire2OUT1 - aire2OUT0)*8/(30*1024*1024) # Para ponerlo en Mbit/s

mongo <- mongo.create(host="localhost" , db="Trafico")
mongo.insert(mongo, "Trafico.Ono", 
             mongo.bson.from.JSON(sprintf('{\n "BytesIN": "%s", \n "BytesOUT": "%s", \n "SpeedIN": "%s", \n "SpeedOUT": "%s" ,\n "DateTime": "%s" \n}',
                                          onoIN1, onoOUT1, onoINv, onoOUTv, DateTime)))
mongo.insert(mongo, "Trafico.Aire1", 
             mongo.bson.from.JSON(sprintf('{\n "BytesIN": "%s", \n "BytesOUT": "%s", \n "SpeedIN": "%s", \n "SpeedOUT": "%s" ,\n "DateTime": "%s" \n}',
                                          aire1IN1, aire1OUT1, aire1INv, aire1OUTv, DateTime)))
mongo.insert(mongo, "Trafico.Aire2", 
             mongo.bson.from.JSON(sprintf('{\n "BytesIN": "%s", \n "BytesOUT": "%s", \n "SpeedIN": "%s", \n "SpeedOUT": "%s" ,\n "DateTime": "%s" \n}',
                                          aire2IN1, aire2OUT1, aire2INv, aire2OUTv, DateTime)))
mongo.insert(mongo, "Trafico.Total", 
             mongo.bson.from.JSON(sprintf('{\n "SpeedIN": "%s", \n "SpeedOUT": "%s" ,\n "DateTime": "%s" \n}',
                                          onoINv+aire1INv+aire2INv, onoOUTv+aire1OUTv+aire2OUTv, DateTime)))
mongo.destroy(mongo)

delay <- proc.time() - ptm
Sys.sleep(5*60 - delay[[3]])

while(TRUE){
    tryCatch({
  
    ptm <- proc.time()
    print(paste0("Start ", Sys.time()))
  
    # Ono #
    onoIN <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
    onoOUT <-  as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))
    
    # Cabecera 1 #
    aire1IN <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
    aire1OUT <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))
    
    # Cabecera 2 #
    aire2IN <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.6.1", intern = T)))
    aire2OUT <- as.numeric(limpiaSNMP(system("snmpget -v1 -c public X.X.X.X 1.3.6.1.2.1.31.1.1.1.10.1", intern = T)))
    
    DateTime <- as.character(strptime(Sys.time(), "%F %X"))
    
    mongo <- mongo.create(host="localhost" , db="Trafico")
    ono <- mongo.find.all(mongo, "Trafico.Ono", query='{}', fields='{"BytesIN":1, "BytesOUT":1, "DateTime":1, "_id":0}', sort='{"_id":-1}', limit = 1) # Si eso que devuelva tb DateTime para logs
    aire1 <- mongo.find.all(mongo, "Trafico.Aire1", query='{}', fields='{"BytesIN":1, "BytesOUT":1, "_id":0}', sort='{"_id":-1}', limit = 1)
    aire2 <- mongo.find.all(mongo, "Trafico.Aire2", query='{}', fields='{"BytesIN":1, "BytesOUT":1, "_id":0}', sort='{"_id":-1}', limit = 1)
    
    minutosLoop <- as.numeric(as.POSIXct(DateTime) - as.POSIXct(ono[[1]]$DateTime))
    
    onoINv <- (onoIN - as.numeric(ono[[1]]$BytesIN))*8/(minutosLoop*60*1024*1024) # Para ponerlo en Mbit/s
    onoOUTv <- (onoOUT - as.numeric(ono[[1]]$BytesOUT))*8/(minutosLoop*60*1024*1024) # Para ponerlo en Mbit/s
    aire1INv <- (aire1IN - as.numeric(aire1[[1]]$BytesIN))*8/(minutosLoop*60*1024*1024) # Para ponerlo en Mbit/s
    aire1OUTv <- (aire1OUT - as.numeric(aire1[[1]]$BytesOUT))*8/(minutosLoop*60*1024*1024) # Para ponerlo en Mbit/s
    aire2INv <- (aire2IN - as.numeric(aire2[[1]]$BytesIN))*8/(minutosLoop*60*1024*1024) # Para ponerlo en Mbit/s
    aire2OUTv <- (aire2OUT - as.numeric(aire2[[1]]$BytesOUT))*8/(minutosLoop*60*1024*1024) # Para ponerlo en Mbit/s
    
    mongo.insert(mongo, "Trafico.Ono", 
                 mongo.bson.from.JSON(sprintf('{\n "BytesIN": "%s", \n "BytesOUT": "%s", \n "SpeedIN": "%s", \n "SpeedOUT": "%s" ,\n "DateTime": "%s" \n}',
                                              onoIN, onoOUT, onoINv, onoOUTv, DateTime)))
    mongo.insert(mongo, "Trafico.Aire1", 
                 mongo.bson.from.JSON(sprintf('{\n "BytesIN": "%s", \n "BytesOUT": "%s", \n "SpeedIN": "%s", \n "SpeedOUT": "%s" ,\n "DateTime": "%s" \n}',
                                              aire1IN, aire1OUT, aire1INv, aire1OUTv, DateTime)))
    mongo.insert(mongo, "Trafico.Aire2", 
                 mongo.bson.from.JSON(sprintf('{\n "BytesIN": "%s", \n "BytesOUT": "%s", \n "SpeedIN": "%s", \n "SpeedOUT": "%s" ,\n "DateTime": "%s" \n}',
                                              aire2IN, aire2OUT, aire2INv, aire2OUTv, DateTime)))
    mongo.insert(mongo, "Trafico.Total", 
                 mongo.bson.from.JSON(sprintf('{\n "SpeedIN": "%s", \n "SpeedOUT": "%s" ,\n "DateTime": "%s" \n}',
                                              onoINv+aire1INv+aire2INv, onoOUTv+aire1OUTv+aire2OUTv, DateTime)))
    mongo.destroy(mongo)
    
    delay <- proc.time() - ptm
    Sys.sleep(5*60 - delay[[3]])
  
  reboot}, error=function(e){
  cat("ERROR :",conditionMessage(e), "\n")
  Sys.sleep(5*60)
  })
}
