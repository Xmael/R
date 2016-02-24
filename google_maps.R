library(RCurl)
library(RJSONIO)
library(plyr)
library(stringdist)
library(stringr)
library(RANN)


cities=read.csv("cities1000.txt",sep="\t",quote="",header=F)
colnames(cities)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                   "feature.code","country.code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                   "population","elevation","dem","timezone","modification.date")
hotelbeds=read.csv("hotelbeds.csv")
restel=read.csv("restel.csv",sep=";",header=T)

colnames(restel)=c("Codigo.hotel","Country","Name","Province","City")

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  
  https://maps.googleapis.com/maps/api/place/textsearch/output?parameters
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng

    nombre = x$results[[1]]$address_components[[1]]$long_name
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(nombre,formatted_address, lat, lng))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA,NA))
  }
}

address <- geoCode(busca)

library(readr)
fallos=read_csv("fallos mapeo.csv")

gmaps=t(sapply(1:nrow(fallos),function(i){
  cat(i, "de", nrow(fallos),"\n")
  address=paste(fallos$name[i],fallos$city[i],sep=", ")
  Sys.sleep(1)
  return(geoCode(address))
}))

correcciones=cbind(fallos$name,fallos$country,fallos$city,gmaps)

correcciones.1=correcciones[!is.na(correcciones[,4]),]


#bolsa=sapply(1:1000,function(i){as.character(unlist(strsplit(str_to_lower(restel$Name[i]),split=" ")))})

prueba=as.character(unlist(strsplit(str_to_lower(correcciones[2,1]),split=" ")))
#prueba=prueba[setdiff(1:length(prueba),match("hotel",prueba))]
indices=NULL
for(i in 1:nrow(correcciones)){
  cat(i,"\n")
  if(fallos$country[i]!=fallos$gcountry[i]){
    if(fallos$prov[i]=="HB"){
      pais=as.character(cities[nn2(query=hotelbeds[hotelbeds$HotelCode==fallos$prov_id[i],8:9],
                                     data=cities[,5:6],k=1,search='radius',radius=100)$nn.idx,9])
      hotelbeds[which(stringdist(str_to_lower(fallos$name[i]),str_to_lower(hotelbeds$Name),method="jaccard")<=0.1),]
    }
    restel[which(stringdist(str_to_lower(fallos$name[45]),str_to_lower(restel$Name),method="jaccard")<=0.1),]

  }
}


head(cities)
geoCode("Hotel Dolce Vita, Tiranë, Albania")
