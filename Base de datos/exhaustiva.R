library(stringdist)
library(stringr)
library(geosphere)
library(dbscan)

hoteles$country[which(hoteles$country=="RU")]="RUSSIA"
cities=read.csv("cities1000.txt",sep="\t",quote="",header=F,stringsAsFactors = F)
colnames(cities)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                   "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                   "population","elevation","dem","timezone","modification.date")

giata=read.csv(file="giata.csv",sep=";",header=T,stringsAsFactors = F)

provincias=function(codigo){
  if(codigo=="JP"){
    return(gsub(" ","",gsub("GUN","",gsub("-GUN","",gsub("SHI","",gsub("-SHI","",str_to_upper(admincodes[admincodes[,1]=="JP",4])))))))
  }
  if(codigo=="IT"){
    return(gsub(" ","",gsub("PROVINCIA DI","",gsub("CITTÀ METROPOLITANA DI","",str_to_upper(admincodes[admincodes[,1]=="IT",4])))))
  }
  if(codigo=="ES"){
    return(gsub("PROVÍNCIA DE ","",gsub("PROVINCE OF ","",gsub("PROVINCIA DE ","",str_to_upper(admincodes[admincodes[,1]=="ES",4])))))
  }
  return(str_to_upper(admincodes[admincodes[,1]==codigo,4]))
}



search.1=function(i,indices){
  subindex=nn2(data=cities[which(cities$country_code==codigo),c(5,6)],query=rbind(as.numeric(hoteles[indices[i],c(13,14)])),k = 1,radius = 5,searchtype = "radius")$nn.idx
  if(subindex!=0){
    codigo_pais=cities$country_code[which(cities$country_code==codigo)[subindex]]
    pais=str_to_upper(countries$country[countries$country_code==codigo_pais])
    return(cbind(codigo_pais,pais))
  }else if(subindex==0){
    return(NA)
  }
}

search.2=function(i,indices){
  
  subindex=nn2(data=cities[,c(5,6)],query=rbind(as.numeric(hoteles[indices[i],c(13,14)])),k = 1,radius = 5,searchtype = "radius")$nn.idx
  if(subindex!=0){
    codigo_pais=cities$country_code[subindex]
    pais=str_to_upper(countries$country[countries$country_code==codigo_pais])
    return(cbind(codigo_pais,pais))
  }else if(subindex==0){
    return(NA) #Probablemente las coordenadas estén mal
  }
}

search.3=function(i,indices){
  if(!is.na(hoteles$city[indices[i]])){
    if(length(which(str_to_upper(ciudades$name)==hoteles$city[indices[i]]))!=0){
      subindex=nn2(data=ciudades[which(str_to_upper(ciudades$name)==hoteles$city[indices[i]]),c(5,6)],
               query=rbind(as.numeric(hoteles[indices[i],c(13,14)])),k = 1,radius = 5,searchtype = "radius")$nn.idx
    }else{return(NA)}
  
  if(subindex!=0){
    codigo_pais=cities$country_code[subindex]
    pais=str_to_upper(countries$country[countries$country_code==codigo_pais])
    return(cbind(codigo_pais,pais))
  }else if(subindex==0){
    return(NA) 
   }
  }else{
    return(NA)
  }
}

search.4=function(i,indices){ #Busca a partir del nombre de la ciudad en nuestra base de datos de hoteles
  busca=match(str_to_upper(hoteles$city[indices[i]]),str_to_upper(cities$name))
  if(length(busca)==1 & !is.na(busca)){
    codigo_pais=cities$country_code[busca]
    if(codigo_pais==hoteles$country_code[indices[i]]){
      pais=str_to_upper(countries$country[which(str_to_upper(countries$country_code)==str_to_upper(hoteles$country_code[indices[i]]))])
      return(cbind(codigo_pais,pais))
    }
  }else{
    codigo_pais=cities$country_code[busca]
    pais=str_to_upper(countries$country[match(codigo_pais,str_to_upper(countries$country_code))])
    return(cbind(codigo_pais,pais))
  }
}

search.4b=function(i,indices){ #Busca a partir del nombre de la ciudad en nuestra base de datos de hoteles
  busca=match(str_to_upper(hoteles$city[indices[i]]),str_to_upper(cities$name))
  if(length(busca)==1 & !is.na(busca)){
    pais=str_to_upper(countries$country[match(cities$country_code[busca],countries$country_code)])
    if(pais==hoteles$country[indices[i]]){
      codigo_pais=countries$country_code[str_to_upper(countries$country)==pais]
      return(cbind(codigo_pais,pais))
    }
  }else{
    codigo_pais=cities$country_code[busca]
    pais=str_to_upper(countries$country[match(codigo_pais,str_to_upper(countries$country_code))])
    return(cbind(codigo_pais,pais))
  }
}

search.5=function(i,indices){
  if(!is.na(hoteles$giata_id[indices[i]])){
    codigo_pais=str_to_upper(giata$country[which(giata$id==hoteles$giata_id[indices[i]])])
    if(length(codigo_pais)!=0){
      pais=str_to_upper(countries$country[countries$country_code==codigo_pais])
      return(cbind(codigo_pais,pais))
    }else{
      return(NA)
    }
  }else{return(NA)}
}

search.6=function(i,indices){ #Busca por uniqid
  codigo_pais=hoteles$country_code[hoteles$uniqid==hoteles$uniqid[indices[i]]]
  if(length(na.omit(codigo_pais))==1){
    return(NA)
  }else{
    pais=str_to_upper(countries$country[match(codigo_pais,str_to_upper(countries$country_code))])
    return(cbind(codigo_pais,pais))
  }
}

search.7=function(i,indices){
  if(hoteles$province[indices[i]] %in% provincias(codigo)){
    codigo_pais=codigo
    pais=str_to_upper(countries$country[which(countries$country_code==codigo_pais)])
    return(cbind(codigo_pais,pais))
  }
  
  if(hoteles$city[indices[i]] %in% provincias(codigo)){
    codigo_pais=codigo
    pais=str_to_upper(countries$country[which(countries$country_code==codigo_pais)])
    return(cbind(codigo_pais,pais))
  }
  
  if(nrow(ciudades[which(str_to_upper(ciudades$name)==hoteles$city[indices[i]]),])!=0){
    codigo_pais=codigo
    pais=str_to_upper(countries$country[which(countries$country_code==codigo_pais)])
    return(cbind(codigo_pais,pais))
  }
  
  if(!is.na(hoteles$giata_id[indices[i]])){
    candidatos=(hoteles[which(hoteles$giata_id==hoteles$giata_id[indices[i]]),])
    
  }else if(is.na(hoteles$giata_id[indices[i]])){
    if(!is.na(hoteles$city[indices[i]])){
      candidatos=(hoteles[which(hoteles$city==hoteles$city[indices[i]]),])
    }else if(is.na(hoteles$city[indices[i]])){return(NA)}
  }
  
  candidatos=candidatos[which(!is.na(candidatos[,c(13,14)]))[which(!is.na(candidatos[,c(13,14)]))<=nrow(candidatos)],]
  
  
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
                                                            rbind(apply(rbind(dataset[,c(1,2)]),2,as.numeric)),a=6378137, f=1/298.257223563)/1000)
  threshold=30
  pais=names(which.max(table(candidatos$country[which(candidatos$country!="NULL" & distancias<threshold)])))

  if(!is.null(pais)){
  codigo_pais=as.character(countries$country_code[which(str_to_upper(countries$country)==pais)])
  return(cbind(codigo_pais,pais))
  }else{return(NA)}
}

