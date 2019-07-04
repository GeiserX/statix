.libPaths(c(.libPaths(), "/home/tecnico/R/x86_64-pc-linux-gnu-library/3.2"))
library("RMongo")
library('stringr')
library('rPython')
Sys.sleep(5*60)

while(TRUE){
  tryCatch({
    
    ptm <- proc.time()
    #print(c("Start ", Sys.time()))
    
    #####################################################################################
    # Conectamos al servidor principal x.x.x.x para extraer MACs de cada cliente #
    #####################################################################################
    
    outputMACs <- system(ignore.stdout = F, ignore.stderr = T, command = 'python3 /home/tecnico/EstadisticasWEB/MACsPrincipal.py', intern = T)
    #print("Sacado MACs")
    
    macs <- outputMACs[grep("=mac-address=", outputMACs)]
    users <- outputMACs[grep("=user=", outputMACs)]
    address <- outputMACs[grep("=address=", outputMACs)]
    loginby <- outputMACs[grep("=login-by=", outputMACs)]
    uptime <- outputMACs[grep("=uptime=", outputMACs)]
    bytesin <- outputMACs[grep("=bytes-in=", outputMACs)]
    bytesout <- outputMACs[grep("=bytes-out=", outputMACs)]
    
    for(i in 1:length(macs)){
      macs[i] <- strsplit(macs, "=")[[i]][3]
      users[i] <- strsplit(users, "=")[[i]][3]
      address[i] <- strsplit(address, "=")[[i]][3]
      loginby[i] <- strsplit(loginby, "=")[[i]][3]
      uptime[i] <- strsplit(uptime, "=")[[i]][3]
      bytesin[i] <- strsplit(bytesin, "=")[[i]][3]
      bytesout[i] <- strsplit(bytesout, "=")[[i]][3]
    }
    usuarios <- as.data.frame(cbind(users, macs, address, loginby, uptime, bytesin, bytesout), stringsAsFactors = F)
    
    ####################################################################################
    # Conectamos al servidor principal 10.255.255.195 para extraer IPs de cada HotSpot #
    ####################################################################################
    
    #print("Sacando IPs")
    outputIPs <- system(ignore.stdout = F, ignore.stderr = T, command = 'python3 /home/tecnico/EstadisticasWEB/IPsPrincipal.py', intern = T)
    names <- outputIPs[grep(">>> =name=", outputIPs)]
    ips <- outputIPs[grep("=remote-address=", outputIPs)]
    
    if(length(names) != 0 && length(ips) != 0){
      
      for(i in 1:length(ips)){
        ips[i] <- strsplit(ips, "=")[[i]][3]
      }
      for(i in 1:length(names)){
        names[i] <- strsplit(names, "=")[[i]][3]
      }
      #print(names)
      #print(ips)
      names <- str_replace_all(names, "-", " ") 
      names <- sapply(str_split(names, " "), function(x) {x[-1]})
      names <- sapply(names, function(x) {paste(x, collapse = " ")})
      nombres <- as.data.frame(names)
      nombres <- cbind(nombres, ips)
    }
    
    ##############################################
    # Conectamos a cada MKT Hotspot... 6 minutos #
    ##############################################
    
    outputUsers <- list()
    for(i in 1:length(ips)){
      #print(i)
      #print(ips[i])
      outputUsers[[i]] <- system(command = sprintf("python3 -u /home/tecnico/EstadisticasWEB/scriptHotSpot.py '%s'", ips[i]), intern = T, ignore.stdout = F, ignore.stderr = T)
    }
    
    ##########################
    # Extraemos MACs y Users #
    ##########################
    
    macslist <- vector("list", length(names))
    names(macslist) <- names
    signallist <- vector("list", length(names))
    names(signallist) <- names
    for(i in 1:length(ips)){
      if(!length(strsplit(outputUsers[[i]][grep("=mac-address=", outputUsers[[i]])], "=")) == 0){
        for(j in 1:length(strsplit(outputUsers[[i]][grep("=mac-address=", outputUsers[[i]])], "="))){
          #print(sprintf("i: %s, j: %s, datos: %s", i, j, strsplit(outputUsers[[i]][grep("=mac-address=", outputUsers[[i]])], "=")))
          macslist[[i]][j] <- strsplit(outputUsers[[i]][grep("=mac-address=", outputUsers[[i]])], "=")[[j]][3]
        }
      }
      if(!length(strsplit(outputUsers[[i]][grep("=signal-strength=", outputUsers[[i]])], "=")) == 0){
        for(j in 1:length(strsplit(outputUsers[[i]][grep("=signal-strength=", outputUsers[[i]])], "="))){
          #print(sprintf("i: %s, j: %s, datos: %s", i, j, strsplit(outputUsers[[i]][grep("=signal-strength=", outputUsers[[i]])], "=")))
          signallist[[i]][j] <- strsplit(outputUsers[[i]][grep("=signal-strength=", outputUsers[[i]])], "=")[[j]][3] ######## subíndice fuera de límites en ocasiones... hace falta sacarlo del for########
        }
      }
    }
    
    ##########################
    # Hacemos la comparación #
    ##########################
    
    coinc <- data.frame()
    for(z in 1:length(ips)){
      coinc[z,1] <- sum(macs %in% macslist[[z]])
    }
    row.names(coinc) <- names
    coinc$DateTime <- as.character(strptime(Sys.time(), "%F %X"))
    names(coinc)[1] <- "HS-Users" 
    
    ########################################################
    # Extraemos localización, signal strength, por usuario #
    ########################################################
    
    usuarios$loc <- NA
    usuarios$signal <- NA
    for(z in 1:dim(usuarios)[1]){
      for(i in 1:length(macslist)){
        if(usuarios$macs[z] %in% macslist[[i]]){
          #print(paste(z, i))
          usuarios$loc[z] <- names(macslist)[i]
          for(j in 1:length(signallist[[i]])){
            if(!is.na(macslist[[i]][j])){
              if(usuarios$macs[z] == macslist[[i]][j]){
                usuarios$signal[z] <- signallist[[i]][j] 
              }
            }
          }
        }
      }
    }
    usuarios$DateTime <- as.character(strptime(Sys.time(), "%F %X"))
    
    ###################################################################################
    # Hacemos lo mismo con Unifi, conectándonos al Unifi Server y obteniendo sus macs #
    ###################################################################################
    
    #print("Start Unifi Controller API")
    
    python.exec(python.code = c("import argparse", 
                                "from unifi.controller import Controller",
                                "c = Controller('UNIFI.IP', 'admin', 'PASSWORD', '8443', 'v4', 'emartinez')",
                                "aps = c.get_aps()",
                                "ap_names = dict([(ap['mac'], ap['name']) for ap in aps])",
                                "clients = c.get_clients()",
                                "clients.sort(key=lambda x: -x['rssi'])"
    ))
    NameAPs1 <- as.data.frame(python.get("ap_names"), stringsAsFactors = F)
    clients1 <- python.get("clients")
    
    python.exec(python.code = c("import argparse", 
                                "from unifi.controller import Controller",
                                "c = Controller('UNIFI2.IP', 'admin', 'PASSWORD', '8443', 'v4', 'em-oficinas-centrales')",
                                "aps = c.get_aps()",
                                "ap_names = dict([(ap['mac'], ap['name']) for ap in aps])",
                                "clients = c.get_clients()",
                                "clients.sort(key=lambda x: -x['rssi'])"
    ))
    NameAPs2 <- as.data.frame(python.get("ap_names"), stringsAsFactors = F)
    clients2 <- python.get("clients")
    
    NameAPs <- rbind(NameAPs1, NameAPs2)
    
    #print("Begin Processing Data of Unifi Controller 1")
    
    usuariosUnifi1 <- data.frame(signal = character(0), mac = character(0), ap = character(0), stringsAsFactors = F)
    if(length(clients1) != 0){
      for(i in 1:length(clients1)){
        usuariosUnifi1[i,] <- c(clients1[[i]][["signal"]], toupper(clients1[[i]][["mac"]]), as.character(NameAPs[clients1[[i]][["ap_mac"]] == rownames(NameAPs),1]))
      }
    }
    
    #print("Begin Processing Data of Unifi Controller 2")
    
    usuariosUnifi2 <- data.frame(signal = character(0), mac = character(0), ap = character(0), stringsAsFactors = F)
    if(length(clients2) != 0){
      for(i in 1:length(clients2)){
        usuariosUnifi2[i,] <- c(clients2[[i]][["signal"]], toupper(clients2[[i]][["mac"]]), as.character(NameAPs[clients2[[i]][["ap_mac"]] == rownames(NameAPs),1]))
      }
    }
    usuariosUnifi <- rbind(usuariosUnifi1, usuariosUnifi2)
    
    #print("Begin Processing Data of Unifi Controller 3")
    
    insert <- as.data.frame(cbind("", NameAPs[,1]), stringsAsFactors = F)
    names(insert) <- c("names", "ips")
    nombres <- rbind(nombres,insert)
    
    #print("Begin Processing Data of Unifi Controller 4")
    
    APs <- as.data.frame(table(usuariosUnifi$ap), stringsAsFactors = F)
    APs$DateTime <- as.character(strptime(Sys.time(), "%F %X"))
    names(APs) <- c("AP", "HS-Users", "DateTime")
    
    write.table(nombres, "/home/tecnico/EstadisticasWEB/listaHS.csv")
    
    #print("Inserting Data of Unifi Controller 1")
    
    # Insertamos en la tabla de usuarios los usuarios encontrados
    for (i in 1:length(usuarios$macs)){
      if(usuarios$macs[i] %in% usuariosUnifi$mac){
        #print(paste0("Mac número ", i))
        usuarios[i,8] <- usuariosUnifi$ap[usuariosUnifi$mac %in% usuarios$macs[i]]
        usuarios[i,9] <- usuariosUnifi$signal[usuariosUnifi$mac %in% usuarios$macs[i]]
      }
    }
    
    ###################################################
    # Insertamos usuarios en la base de datos MongoDB #
    ###################################################
    
    #print("Inserting Data of Unifi Controller 2")
    
    mongo <- mongoDbConnect("usuariosHS", "localhost", 27017)
    for(i in 1:dim(usuarios)[1]){
      dbInsertDocument(mongo, usuarios$users[i],
                       sprintf('{"macs" : "%s", "address" : "%s", "loginby" : "%s", "uptime" : "%s", "bytesin" : "%s",
                               "bytesout" : "%s", "loc" : "%s", "signal" : "%s", "DateTime" : "%s"}',
                               usuarios[i,2], usuarios[i,3], usuarios[i,4], usuarios[i,5], usuarios[i,6], usuarios[i,7],
                               usuarios[i,8], usuarios[i,9], usuarios[i,10]))
    }
    RMongo::dbDisconnect(mongo)
    
    ##############################################################
    # Insertamos en la BBDD los usuarios por MKT/Unifi y totales #
    ##############################################################
    
    # HotSpot Mikrotik
    mongo <- mongoDbConnect("estadisticasHS", "localhost", 27017)
    for(i in 1:dim(coinc)[1]){
      dbInsertDocument(mongo, ips[i], sprintf('{"HS-Users" : "%s", "DateTime" : "%s"}', coinc[i,1], coinc[i,2]))
    }
    
    # Total HotSpot
    datos <- as.data.frame(cbind(dim(usuarios)[1], as.character(strptime(Sys.time(), "%F %X"))), stringsAsFactors = F)
    names(datos) <- c("HS-Users", "DateTime")
    dbInsertDocument(mongo, "Total", sprintf('{"HS-Users" : "%s", "DateTime" : "%s"}', datos[1,1], datos[,2]))
    
    # HotSpot Unifi
    for(i in 1:dim(APs)[1]){
      dbInsertDocument(mongo, APs$AP[i], sprintf('{"HS-Users" : "%s", "DateTime" : "%s"}', APs$`HS-Users`[i], APs$DateTime[i]))
    }
    
    RMongo::dbDisconnect(mongo)
    
    delay <- proc.time() - ptm
    Sys.sleep(10*60 - delay[[3]])
    
  }, error=function(e){
    cat("ERROR :",conditionMessage(e), "\n")
    Sys.sleep(5*60)
  })
}
