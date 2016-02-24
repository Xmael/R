library(geonames)
library(geosphere)
library(dbscan)
library(stringdist)
library(stringr)
# library(dplyr)
setwd("~/Trivago")
admincodes=read.csv(file="admin2Codes.txt",sep="\t",header=F,stringsAsFactors = F)
reconstruye=function(admincodes){
  temp=matrix(data=NA,nrow=nrow(admincodes),ncol=6,byrow=T)
  for(i in 1:nrow(admincodes)){
    aux=rbind(unlist(strsplit(admincodes[i,1],split="\\.")))
    for(j in 1:length(aux)){temp[i,j]=aux[j]}
    temp[i,4]=admincodes[i,2]
    temp[i,5]=admincodes[i,3]
    temp[i,6]=admincodes[i,4]
  }
  return(temp)
}

raw_text=function(aux){
  peticiones=paste("http://localhost:5000/es/raw/content/",aux$prov,"/",aux$prov_id,sep="")
  sapply(1:length(peticiones),function(i){
    respuesta=getURL(peticiones[i])
    if(respuesta=="{}"){
      return(NA)
    }else{
      respuesta=fromJSON(respuesta)
      return(respuesta$city)
    }
  })
}

codigo_provincia=function(candidatos,admincodes){
  provincia=unique(na.omit(gsub(" ","",candidatos$province)))
  respuesta=sapply(1:length(provincia),function(i){
    paste(admincodes[which(admincodes[,1]=="ES" & gsub(" ","",str_to_upper(admincodes[,4]))==provincia[i]),c(1:3)],collapse = ".")
  })
  return(respuesta)
}

admincodes=reconstruye(admincodes)

#hoteles=read.csv("hoteles_corregidos.csv",header=T,sep=";",stringsAsFactors = F)
load("hoteles(25.1.16).RData")

j=69
countries=read.csv("countries.csv", sep=";",head=F,na.strings = "NULL",stringsAsFactors = F)
colnames(countries)=c("country_code","country")

codigo=as.character(countries$country_code[j])

#filter(hoteles,country_code==codigo)[1:10,]

ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="")
colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")
cities15k=read.csv("cities15000.txt",sep="\t",quote="",header=F,stringsAsFactors = F,colClasses = "character")
colnames(cities15k)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")

ciudades_matrix=as.matrix(ciudades) #Convertimos el data frame de ciudades en una matriz para que la siguiente función vaya más rápida
codigosciudades=sapply(1:nrow(ciudades),function(i){paste(ciudades_matrix[i,c(9,11,12)],collapse=".")}) #Obtenemos los códigos territoriales de cada ciudad


nombres=str_to_upper(names(table(hoteles$city[which(hoteles$country_code==codigo)])))
incluye=intersect(nombres,nombres[nombres %in% str_to_upper(unlist(strsplit(ciudades$alternate.names,split=",")))])
incluye=unique(c(incluye,intersect(nombres,nombres[nombres %in% str_to_upper(ciudades$name)])))
excluye=setdiff(nombres,incluye)

# excluye=excluye[-match(c("","."),excluye)]

candidatos_nombre=NULL
for(i in 1:length(excluye)){
  cat(i,"de",length(excluye),excluye[i]," => ")
  jw=ciudades[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1],c(2,5,6)]
  cosine=ciudades[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="cosine"),decreasing=F)[1],c(2,5,6)]
  qgram=ciudades[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),q=2,method="qgram"),decreasing=F)[1],c(2,5,6)]
  lv=ciudades[order(stringdist(gsub("-","",excluye[i]),gsub(","," ",str_to_upper(ciudades$name)),q=2,method="lv"),decreasing=F)[1],c(2,5,6)]
  jaccard=ciudades[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jaccard"),decreasing=F)[1],c(2,5,6)]
  
  candidatos=hoteles[which(str_to_upper(iconv(hoteles$city,to="ASCII//TRANSLIT")) %in% iconv(excluye[i],to="ASCII//TRANSLIT") & hoteles$country_code==codigo & !is.na(hoteles$lat)), ]
  codigosp=codigo_provincia(candidatos,admincodes)
  jwR=t(sapply(1:length(codigosp),function(j){
    ciudades[which(codigosciudades==codigosp[j] & ciudades$feature.class=="P"),][
      order(stringdist(gsub(","," ",str_to_upper(ciudades$name[which(codigosciudades==codigosp[j] & ciudades$feature.class=="P")])),
                       gsub("-","",excluye[i]),method="jw",weight = c(0.1,1,1)),decreasing=F)[1],c(2,5,6)]}))
  
  cuenta=data.frame(rbind(jw,jwR,cosine,qgram,lv,jaccard)) #Obtenemos los nombres de las ciudades que sean parecidas y sus coordenadas
  
  dataset=rbind(apply(candidatos[,14:13],2,as.numeric))
  if(nrow(dataset)>1){
    temp=dbscan(dataset, eps=1, minPts = 2, borderPoints = TRUE,
                search = "kdtree", bucketSize = 10,
                splitRule = "suggest", approx = 0)
    dataset=cbind(dataset,(temp$cluster))
  }else{
    dataset=cbind(dataset,1)
  }
  distancias=sapply(unique(dataset[,3]),function(k) distGeo(rbind(apply(rbind(dataset[which(dataset[,3]==k),1:2]),2,mean)),
                                        cbind(apply(cuenta[,c(3,2)],2,as.numeric)),a=6378137, f=1/298.257223563)/1000)
  threshold=30
  
  coeficientes=which(cuenta[,1] %in% cities15k$name)
  if(length(coeficientes!=0)){
    distancias[coeficientes,1]=distancias[coeficientes,1]/20
  }
 
  nombre=str_to_upper(cuenta[distancias<threshold,1][which.min(distancias[distancias<threshold])])
  
  if(length(nombre)==0){#Lo comprobamos en giata
    gcity=names(which.max(table(hoteles$gcity[which(str_to_upper(hoteles$city)==excluye[i] & hoteles$country_code==codigo)])))
    cat(" giata =>",gcity, " => ")
    jw=ciudades[order(stringdist(gsub(","," ",str_to_upper(iconv(ciudades$name,to="ASCII//TRANSLIT"))),gsub("-","",gcity),method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1],c(2,5,6)]
    cosine=ciudades[order(stringdist(gsub(","," ",str_to_upper(iconv(ciudades$name,to="ASCII//TRANSLIT"))),gsub("-","",gcity),method="cosine"),decreasing=F)[1],c(2,5,6)]
    qgram=ciudades[order(stringdist(gsub(","," ",str_to_upper(iconv(ciudades$name,to="ASCII//TRANSLIT"))),gsub("-","",gcity),q=2,method="qgram"),decreasing=F)[1],c(2,5,6)]
    lv=ciudades[order(stringdist(gsub("-","",gcity),gsub(","," ",str_to_upper(iconv(ciudades$name,to="ASCII//TRANSLIT"))),q=2,method="lv"),decreasing=F)[1],c(2,5,6)]
    jaccard=ciudades[order(stringdist(gsub(","," ",str_to_upper(iconv(ciudades$name,to="ASCII//TRANSLIT"))),gsub("-","",gcity),method="jaccard"),decreasing=F)[1],c(2,5,6)]
    
    cuenta=data.frame(rbind(jw,cosine,qgram,lv,jaccard)) #Obtenemos los nombres de las ciudades que sean parecidas y sus coordenadas
    
    candidatos=hoteles[which(str_to_upper(hoteles$city) %in% excluye[i] & hoteles$country_code=="ES" & !is.na(hoteles$lat)), ]
    codigosp=codigo_provincia(candidatos,admincodes)
    jwR=t(sapply(1:length(codigosp),function(j){
      ciudades[which(codigosciudades==codigosp[j] & ciudades$feature.class=="P"),][
        order(stringdist(gsub(","," ",str_to_upper(ciudades$name[which(codigosciudades==codigosp[j] & ciudades$feature.class=="P")])),
                         gsub("-","",gcity),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1],c(2,5,6)]}))
    
    
    dataset=rbind(apply(candidatos[,14:13],2,as.numeric))
    if(nrow(dataset)>1){
      temp=dbscan(dataset, eps=1, minPts = 2, borderPoints = TRUE,
                  search = "kdtree", bucketSize = 10,
                  splitRule = "suggest", approx = 0)
      dataset=cbind(dataset,(temp$cluster))
    }else{
      dataset=cbind(dataset,1)
    }
    distancias=sapply(unique(dataset[,3]),function(k) distGeo(rbind(apply(rbind(dataset[which(dataset[,3]==k),1:2]),2,mean)),
                                                              cbind(apply(cuenta[,c(3,2)],2,as.numeric)),a=6378137, f=1/298.257223563)/1000)
    coeficientes=which(cuenta[,1] %in% cities15k$name)
    if(length(coeficientes!=0)){
      distancias[coeficientes,1]=distancias[coeficientes,1]/20
    }
    
    kayak=str_to_upper(unique(candidatos$city[which(candidatos$prov=="KY")])) #Apaño para que si el nombre del candidato lo proporciona Kayak se quede tal cual
    if(length(kayak)!=0){
      pos1=str_locate(kayak, "\\(")[1]-1
      if(!is.na(pos1)){
        kayak=sub(" +$", "",substr(x = kayak,start = 1,stop = str_locate(kayak, "\\(")[1]-1))
      }
      nombre=kayak
    }else{
      nombre=str_to_upper(cuenta[distancias<threshold,1][which.min(distancias[distancias<threshold])])
    }
  }
  if(length(nombre)!=0){
    candidatos_nombre=rbind(candidatos_nombre,cbind(i,excluye[i],nombre))
    cat(nombre, "\n")
  }else{
    candidatos_nombre=rbind(candidatos_nombre,cbind(i,excluye[i],NA))
    cat(NA,"\n")
  }
}


save(candidatos_nombre,file="candidatos.RData")
save(excluye,file="excluye.RData")
load("candidatos.RData")
ciudadesp=ciudades[which(ciudades$feature.class=="P"),]
candidatos_nombre=cbind(candidatos_nombre,NA)
##########



for(i in 1:nrow(candidatos_nombre)){
  cat(i,candidatos_nombre[i,2]," | ",candidatos_nombre[i,3],"=>")
  candidatos=hoteles[which(str_to_upper(hoteles$city)==str_to_upper(candidatos_nombre[i,2]) & !is.na(hoteles$lat) & hoteles$country_code==codigo),]
  
  dataset=rbind(apply(candidatos[,14:13],2,as.numeric))
  if(nrow(dataset)>1){
    temp=dbscan(dataset, eps=1, minPts = 2, borderPoints = TRUE,
                search = "kdtree", bucketSize = 10,
                splitRule = "suggest", approx = 0)
    dataset=cbind(dataset,(temp$cluster))
  }else{
    dataset=cbind(dataset,1)
  }
  
  distancias=sapply(unique(dataset[,3]),function(k) distGeo(rbind(apply(rbind(dataset[which(dataset[,3]==k),1:2]),2,mean)),
                                                            apply(ciudadesp[,c(6,5)],2,as.numeric),a=6378137, f=1/298.257223563)/1000)
  lista=str_to_upper(ciudadesp[order(distancias,decreasing=F)[1:100],2])
  jw=lista[order(stringdist(iconv(lista,to="ASCII//TRANSLIT"),candidatos_nombre[i,2],method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]
  candidatos_nombre[i,4]=jw
  cat(candidatos_nombre[i,4],"\n")
}

candidatos_nombre=data.frame(cbind(candidatos_nombre,NA),stringsAsFactors = F)
colnames(candidatos_nombre)=c("indice","Original","Propuesta.1","Propuesta.2","Definitiva")


#Corrige a mano la lista de candidatos
for(i in 772:nrow(candidatos_nombre)){
  cat(i,"de",nrow(candidatos_nombre),"\n")
  cat(candidatos_nombre[i,2]," ===> 1. ",candidatos_nombre[i,3], " | 2.", candidatos_nombre[i,4],"\n")
  
  aux=hoteles[which(str_to_upper(hoteles$city)==candidatos_nombre[i,2] & hoteles$country_code==codigo),]
  propuesta=sapply(1:nrow(aux),function(i){ciudadesp$name[order(distGeo(apply(rbind(aux[i,c(14,13)]),2,as.numeric),apply(ciudadesp[,c(6,5)],2,as.numeric),a=6378137, f=1/298.257223563)/1000,decreasing = F)[1]]})
  print(aux[,c(2,3,6,11,20,23)])
  print(propuesta)
#  cat("Proveedor#",raw_text(aux),"\n")
  lee=readline(prompt = "Opcion [1]/[2]/Nombre: ")
  if(lee==1){
    candidatos_nombre[i,5]=candidatos_nombre[i,3]
  }else if(lee==2){
    candidatos_nombre[i,5]=candidatos_nombre[i,4]
  }else{
    candidatos_nombre[i,5]=str_to_upper(lee)
  }
}

save(candidatos_nombre,file="candidatos_nombre.RData")

pb=txtProgressBar(min = 0, max = nrow(candidatos_nombre), initial = 0, char = "=",width = NA, title, label, style = 3)
for(i in 1:nrow(candidatos_nombre)){
  setTxtProgressBar(pb, i)
  hoteles$city[which(hoteles$city==candidatos_nombre[i,2])]=candidatos_nombre[i,5]
}
