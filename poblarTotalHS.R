library(RMongo)
mongo <- mongoDbConnect("estadisticasHS", "localhost", 27017)

listaHS <- read.table("listaHS.csv", header = T, sep = " ", stringsAsFactors = F)
lista <- listaHS$ips

query <- vector("list", length=length(lista))
for(i in 1:length(lista)){
  print(i)
  query[[i]] <- dbGetQueryForKeys(mongo, lista[i], '{}', '{ "DateTime":1, "HS-Users":1, "_id":0 }')
  #query[[i]] <- query[[i]][1:584,]
}

suma <- 0
sumaFinal <- vector(length = dim(query[[1]])[1])
for(i in 1:dim(query[[1]])[1]){
  suma <- 0
  for(j in 1:length(lista)){
    if(!is.null(query[[j]]$HS.Users[i]) && !is.na(query[[j]]$HS.Users[i])){
      suma <- suma + query[[j]]$HS.Users[i]
    }
  }
  if(!length(suma)==0)
    sumaFinal[i] <- suma
}

final <- as.data.frame(cbind(as.data.frame(sumaFinal), as.data.frame(query[[1]]$DateTime)))
names(final) <- c("HS-Users", "DateTime")

for(k in 1:dim(query[[1]])[1]){
  dbInsertDocument(mongo, "Total", toJSON(final[k,]))
}
RMongo::dbDisconnect(mongo)


