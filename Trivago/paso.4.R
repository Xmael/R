library(geonames)
library(geosphere)

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

busca_ciudad=function(codificado,admincodes){
  temp=admincodes[admincodes[,1]==codificado[,1],]
  if(codificado[,2]!=""){
    temp=temp[which(temp[,2]==codificado[,2]),]
  }else{
    temp=rbind(temp[which(temp[,2]==codificado[,3]),])
  }
  return(temp[4])
}

admincodes=read.csv(file="admin1CodesASCII.txt",sep="\t",header=F,stringsAsFactors = F)

admincodes=reconstruye(admincodes)

hoteles=read.csv("hoteles_corregidos.csv",header=T,sep=";",stringsAsFactors = F)

j=69

codigo=as.character(countries$country_code[j])

filter(hoteles,country_code==codigo)[1:10,]

ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="")
colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")

nombres=str_to_upper(names(table(filter(hoteles,country_code==codigo)$city)))
incluye=intersect(nombres,nombres[nombres %in% str_to_upper(unlist(strsplit(ciudades$alternate.names,split=",")))])
incluye=unique(c(incluye,intersect(nombres,nombres[nombres %in% str_to_upper(ciudades$name)])))
excluye=setdiff(nombres,incluye)
ciudades_no_hoteles=ciudades[which(ciudades$feature.class=="P"),]
#Corrige nombres
#hoteles$city[which(str_to_upper(hoteles$city)==excluye[i] & hoteles$country_code==codigo)]
#hoteles$city[which(str_to_upper(hoteles$city)==excluye[i] & hoteles$country_code==codigo)]="CANILLO"


candidatos_nombre=NULL
ambiguos=NULL
for(i in 1:length(excluye)){
  cat(i,"de",length(excluye),excluye[i]," => ")
  
  jw=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1]]
  cosine=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="cosine"),decreasing=F)[1]]
  qgram=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),q=2,method="qgram"),decreasing=F)[1]]
  lv=ciudades$name[order(stringdist(gsub("-","",excluye[i]),gsub(","," ",str_to_upper(ciudades$name)),q=2,method="lv"),decreasing=F)[1]]
  jaccard=ciudades$name[order(stringdist(gsub(","," ",str_to_upper(ciudades$name)),gsub("-","",excluye[i]),method="jaccard"),decreasing=F)[1]]
  
  
  cuenta=as.data.frame(table(c(jw,cosine,soundex,qgram,lv,jaccard)))
  
  indices=which(cuenta[,2] == max(cuenta[,2]))
  if(length(indices)==1){
    candidatos_nombre=rbind(candidatos_nombre,cbind(i,excluye[i],str_to_upper(cuenta[indices,1])))
    cat(str_to_upper(cuenta[indices,1]), "\n")
  }else{
    ambiguos=rbind(ambiguos,cbind(i,excluye[i]))
    cat("AMBIGUO!!!!\n")
  }
}

candidatos_nombre=cbind(candidatos_nombre,NA)
hoteles_pais=hoteles[which(hoteles$country_code==codigo),]
for(i in 1:nrow(candidatos_nombre)){ #busca por nombre del hotel
  candidato=hoteles[which(str_to_upper(hoteles$city)==candidatos_nombre[i,2] & hoteles$country_code=="ES" & !is.na(hoteles$province)),]
  if(length(unique(na.omit(candidato$giata_id)))>1){ #Si nº Giatas distintos es mayor que uno
    candidato=candidato[which(!is.na(candidato$province))[1],]
  }
  cat(i,"de",nrow(candidatos_nombre),candidato$name," => ")
  #Probamos a buscar entre nuestra lista de hoteles
  coeficientes=stringdist(str_to_upper(hoteles_pais$name),gsub("-","",str_to_upper(candidato$name)),method="jw",weight = c(1,0.5,0.5)) #Busca por nombre del hotel
  candidatos=hoteles_pais[which(coeficientes<=0.5)[order(coeficientes[which(coeficientes<=0.5)])],][-1,] #Me los ordena por el valor del coeficiente
  if(nrow(candidatos)!=0){
    candidatos[,13:14]=apply(candidatos[,13:14],2,as.numeric)
    
    dataset=candidatos[,14:13]
    temp=dbscan(dataset, eps=0.5, minPts = 1, borderPoints = TRUE,
                search = "kdtree", bucketSize = 10,
                splitRule = "suggest", approx = 0)
    dataset=cbind(dataset,temp$cluster)
    distancias=sapply(1:(length(unique(dataset[,3]))-1),function(i) distGeo(apply(dataset[which(dataset[,3]==i),1:2],2,mean),apply(candidato[,14:13],2,as.numeric),a=6378137, f=1/298.257223563))
    
    nombres=candidatos[which(!is.na(match(dataset[,3],which.min(distancias)))),] #Debería obtener varios candidatos
    nombres=candidatos[which(!is.na(candidatos$giata_id)),]
    if (nrow(nombres)!=0){
    candidatos_nombre[i,4]=nombres$city[which(!is.na(candidatos$giata_id))]
    cat(candidatos_nombre[i,4],"\n")
    }
    
  }else if(!is.na(candidato$lat) & !is.na(candidato$lng)){#Si el número de candidatos es igual a cero, buscamos por coordenadas
    subindex=nn2(ciudades_no_hoteles[,5:6],candidato[,13:14],searchtype = 'radius', radius = 0.1,eps = 0.5,k=1)$nn.idx
    nombre=str_to_upper(ciudades_no_hoteles$name[subindex])
    jw=stringdist(str_to_upper(candidatos_nombre[i,2]),gsub("-","",nombre),method="jw",weight = c(0.1,0.5,1))
    if(jw<0.4){
      candidatos_nombre[i,4]=nombre
      cat(candidatos_nombre[i,4],"\n")
    }else{
      candidatos_nombre[i,4]=candidatos_nombre[i,2]
      cat(candidatos_nombre[i,4],"\n")
    }
  }
  
} 

  
###########################    

    subindex=nn2(ciudades_no_hoteles[,5:6],candidatos[1,13:14],searchtype = 'radius', radius = 0.1,eps = 0.5,k=1)$nn.idx
    candidatos_nombre[i,4]=ciudades_no_hoteles$name[subindex]
    cat(ciudades_no_hoteles$name[subindex],"\n")
  }else{#Si no obtenemos ninguna ciudad candidata probamos por ciudad
    coeficientes=stringdist(str_to_upper(ciudades$name),gsub("-","",candidato$city),method="jw",weight = c(1,0.5,0.5))
    candidatos=ciudades[which(coeficientes<=0.5)[order(coeficientes[which(coeficientes<=0.5)])],] #Me los ordena por el valor del coeficiente
  }
}

hoteles_pais[order(stringdist(str_to_upper(hoteles_pais$name),gsub("-","",candidato$name),method="jw",weight = c(1,0.5,0.5)),decreasing=F)[1:10],]


#Busco por los nombres de los hoteles que aparecen en geonames
hoteles_ciudades=ciudades[which(ciudades$feature.code=="HTL"),]

nombre_hotel=str_to_upper(hoteles$name[which(str_to_upper(hoteles$city)==str_to_upper(ambiguos[3,2]) & hoteles$country_code==codigo)]) #Obtengo nombre del hotel
subindex=match(nombre_hotel,str_to_upper(hoteles_ciudades$name))

if(!is.na(subindex)){ #Si NA, probablemente el hotel se encuentre en otro país
  codificado=hoteles_ciudades[subindex,9:14]
  nombre=str_to_upper(busca_ciudad(codificado,admincodes))
}

jw=hoteles_ciudades[order(stringdist(str_to_upper(hoteles_ciudades$name),gsub("-","",nombre_hotel),method="jw",weight = c(0.1,0.5,1)),decreasing=F)[1],]



subindex=nn2(data=ciudades[,c(5,6)],query=rbind(as.numeric(hoteles[which(str_to_upper(hoteles$city)==ambiguos[2,2] & hoteles$country_code==codigo)
                                                                 ,c(13,14)])),k = 1,radius = 1,searchtype = "radius")$nn.idx

lista_ciudades=unlist(str_split(ciudades$alternate.names,pattern=","))
candidatos_ambiguos=as.data.frame(t(sapply(1:nrow(ambiguos),function(i){
  c(ambiguos[i,2],lista_ciudades[order(stringdist(str_to_upper(lista_ciudades),
                                                 ambiguos[i,2],method="jw",weight = c(0.1,0.5,1)),decreasing = F)][1:5])})))

corregidos=NULL
colnames(candidatos)=paste("V",0:(ncol(candidatos)-1),sep="")
for(i in 1:nrow(candidatos)){
  cat("Candidato:",as.character(candidatos[i,1]),"\n")
  print(candidatos[i,-1],right = T,quote = T)
  columna=readline(prompt = "Columna válida ")
  if(columna==""){
    manual=readline("Nombre ")
    corregidos=rbind(corregidos,data.frame(Candidato=as.character(candidatos[i,1]),Corregido=as.character(manual)))
  }else{
    corregidos=rbind(corregidos,data.frame(Candidato=as.character(candidatos[i,1]),Corregido=as.character(candidatos[i,as.integer(columna)+1])))
  }
}

for(i in 1:nrow(corregidos)){
  hoteles$city[which(str_to_upper(hoteles$city)==corregidos[i,1] & hoteles$country_code==codigo)]=str_to_upper(corregidos[i,2])
}