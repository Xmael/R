busca.nombre.hotel=function(candidatos_nombre){
  for(i in 1:nrow(candidatos_nombre)){ #busca por nombre del hotel
    subindex=nn2(ciudades_no_hoteles[,5:6],,searchtype = 'radius', radius = 0.1,eps = 0.5,k=1)$nn.idx #obtenerlo por distancia más cercana en la lista de ciudades
    candidatos=hoteles[which(str_to_upper(hoteles$city)==candidatos_nombre[i,2] & hoteles$country_code=="ES" & !is.na(hoteles$province)),]
    cat(i,"de",nrow(candidatos_nombre),unique(candidatos$city)," => ")
    #Probamos a buscar entre nuestra lista de hoteles
    candidatos[,13:14]=apply(candidatos[,13:14],2,as.numeric)
      
      dataset=candidatos[,14:13]
      temp=dbscan(dataset, eps=0.5, minPts = 1, borderPoints = TRUE,
                  search = "kdtree", bucketSize = 10,
                  splitRule = "suggest", approx = 0)
      dataset=cbind(dataset,temp$cluster)
      distancias=sapply(1:(length(unique(dataset[,3]))-1),function(i) distGeo(rbind(apply(dataset[which(dataset[,3]==i),1:2],2,mean)),rbind(apply(candidato[,14:13],2,as.numeric)),a=6378137, f=1/298.257223563))
      
      nombres=candidatos[which(!is.na(match(dataset[,3],which.min(distancias)))),] #Debería obtener varios candidatos
      nombres=nombres[which(!is.na(nombres$giata_id)),]
      if (nrow(nombres)!=0){
        cat(nombres$city[which(!is.na(nombres$giata_id))])
        return(nombres$city[which(!is.na(nombres$giata_id))])
        
      }
    }
    candidatos_nombre[i,4]=NA
    cat(canditatos_nombre[i,4])
  }
}


library(maps)

map.where(database = "world", x, y)


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

admincodes=reconstruye(admincodes)

paste(admincodes[which(admincodes[,1]=="ES" & str_to_upper(admincodes[,4])=="Toledo"),c(1:3)],collapse=".")




#######################33

candidatos[,13:14]=apply(candidatos[,13:14],2,as.numeric)

dataset=candidatos[,14:13]
temp=dbscan(dataset, eps=0.5, minPts = 1, borderPoints = TRUE,
            search = "kdtree", bucketSize = 10,
            splitRule = "suggest", approx = 0)
plot(dataset,col=temp$cluster+1,pch=16)
dataset=cbind(dataset,temp$cluster)
distancias=sapply(1:(length(unique(dataset[,3]))-1),function(i) distGeo(apply(dataset[which(dataset[,3]==i),1:2],2,mean),apply(candidato[,14:13],2,as.numeric),a=6378137, f=1/298.257223563))

candidatos[which(!is.na(match(dataset[,3],which.min(distancias)))),]


cosine=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="cosine"),decreasing=F)[1]]
jw=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1]]
jaccard=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jaccard"),decreasing=F)[1]]


jw=sort(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1]
cosine=sort(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="cosine"),decreasing=F)[1]
jaccard=sort(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jaccard"),decreasing=F)[1]

metodo=c("jw","cosine","jaccard")[which.min(c(jw,cosine,jaccard))]

if(metodo=="jw"){
  nombre=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1]]
}else if(metodo=="cosine"){
  nombre=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="cosine"),decreasing=F)[1]]
}else if(metodo="jaccard"){
  nombre=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="cosine"),decreasing=F)[1]]
}




######################3



codigo_provincia=function(candidatos,admincodes){
  provincia=unique(na.omit(gsub(" ","",candidatos$province)))
  respuesta=sapply(1:length(provincia),function(i){
    paste(admincodes[which(admincodes[,1]=="ES" & gsub(" ","",str_to_upper(admincodes[,4]))==provincia[i]),c(1:3)],collapse = ".")
  })
  return(respuesta)
}

codigosp=codigo_provincia(candidatos,admincodes)
ciudades_matrix=as.matrix(ciudades)
codigosciudades=sapply(1:nrow(ciudades),function(i){paste(ciudades_matrix[i,c(9,11,12)],collapse=".")})

jw=sapply(1:length(codigosp),function(j){
  ciudades[which(codigosciudades==codigosp[j] & ciudades$feature.class=="P"),][order(stringdist(gsub(","," ",str_to_upper(ciudades$name[which(codigosciudades==codigosp[j] & ciudades$feature.class=="P")])),
                                                                                                gsub("-","",excluye[i]),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1],c(2,5,6)]})

jwR=t(sapply(1:length(codigosp),function(j){
  ciudades[which(codigosciudades==codigosp[j] & ciudades$feature.class=="P"),][
    order(stringdist(gsub(","," ",str_to_upper(ciudades$name[which(codigosciudades==codigosp[j] & ciudades$feature.class=="P")])),
                     gsub("-","",gcity),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1],c(2,5,6)]}))


gsub("(\\{).*(\\})", "", "ALGAIDA (MALLORCA)", perl=TRUE)

x <- c("ALGAIDA (MALLORCA)")
gsub("([^)]+)","",x)

pos1=str_locate(kayak, "\\(")[1]-1
sub(" +$", "",substr(x = kayak,start = 1,stop = pos1)
    
    
    
##########################################3

candidatos_coordenadas=hoteles[which(str_to_upper(hoteles$city)==excluye[i] & hoteles$country_code==codigo),]
candidatos_coordenadas=candidatos_coordenadas[which(!is.na(candidatos_coordenadas[,c(13,14)]))[which(!is.na(candidatos_coordenadas[,c(13,14)]))<=nrow(candidatos_coordenadas)],]

prueba=nn2(candidatos_coordenadas[,13:14],candidatos_coordenadas[,13:14],searchtype = 'radius', radius = 0.1,eps = 0.5)$nn.idx

if(nrow(prueba)==1){ #Solo existe una única entrada en nuestra base de datos de hoteles que tenga las mismas coordenadas
  nombres_aproximados=ciudades$name[nn2(query=candidatos_coordenadas[,13:14],data=ciudades[,5:6],k = 10,radius = 10)$nn.idx]
  nombres_aproximados[order(stringdist(str_to_upper(nombres_aproximados),excluye[5],method="jaccard"),decreasing=F)]
}else if{
  
  quita=NULL
  
  for(m in 1:nrow(prueba)){
    if(all(prueba[m,-1]==0)){
      quita=c(quita,m) #Quitamos aquellos hoteles cuyas coordenadas estén muy lejos del resto
    }
  }
  
  if(!is.null(quita)){
    candidatos_coordenadas=candidatos_coordenadas[-quita,]
  }
  
  city=names(which.max(table(candidatos_coordenadas$city[candidatos_coordenadas$city!="NA"]))) #Nos quedamos con el nombre que nos da la mayoría 
  
  if(str_to_upper(city)==str_to_upper(excluye[i])){ #Si el nombre que obtenemos mediante coordenadas similares en nuestra base de datos es el mismo que teníamos pasamos totalmente de él
    subindex=nn2(data=cities[,c(5,6)],query=rbind(as.numeric(hoteles[indices[i],c(13,14)])),k = 1,radius = 5,searchtype = "radius")$nn.idx
  }
  
  
}

}
