library(stringr)
library(stringdist)
library(parallel)

setMKLthreads(n=6)

cat("Zonas horarias\n")
timezones=read.csv(file="timeZones.txt",sep="\t",header=T)
# hoteles=read.csv(file="hoteles_full.csv",sep=";",header=T,stringsAsFactors = F)
# hoteles$Time_zone=NA
# hoteles$DST=NA

cat("Hoteles\n")
load("hoteles(12.1.16).RData")

reconstruye.1=function(admincodes){ #para admin1codes
  temp=matrix(data=NA,nrow=nrow(admincodes),ncol=5,byrow=T)
  for(i in 1:nrow(admincodes)){
    aux=rbind(unlist(strsplit(admincodes[i,1],split="\\.")))
    for(j in 1:length(aux)){temp[i,j]=aux[j]}
    temp[i,3]=admincodes[i,2]
    temp[i,4]=admincodes[i,3]
    temp[i,5]=admincodes[i,4]
  }
  return(temp)
}

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

codigo="US"

codigo_provincia=function(candidatos,admincodes){
  provincia=unique(na.omit(gsub(" ","",candidatos$province)))
  respuesta=sapply(1:length(provincia),function(i){
    paste(admincodes[which(admincodes[,1]==codigo & gsub(" ","",str_to_upper(admincodes[,4]))==provincia[i]),c(1:3)],collapse = ".")
  })
  return(respuesta)
}

cat("Admincodes\n")

admincodes=read.csv(file="admin2Codes.txt",sep="\t",header=F,stringsAsFactors = F)
admincodes=reconstruye(admincodes)

cat("Ciudades\n")
ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="",na.strings = NULL)
colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")

candidatos=hoteles[which(hoteles$country_code==codigo),]
tamaño=nrow(candidatos)

#ciudades_matrix=as.matrix(ciudades) #Convertimos el data frame de ciudades en una matriz para que la siguiente función vaya más rápida

subadmincodes=admincodes[which(admincodes[,1]==codigo),]

prueba=matrix(NA,nrow=tamaño,ncol=5,byrow = T)
# for(i in 1:tamaño){
# #  if(i%%100==0){
#     cat(i,"de",tamaño,"\n")
# #  }
#   orden=sort.int(stringdist(gsub(","," ",str_to_upper(iconv(subadmincodes[,2],to="ASCII//TRANSLIT"))),
#                             gsub(".*\\, ","",candidatos[i,11]),method="jw",weight = c(1,1,0.5)),
#                             decreasing=F,index.return = T)
#   
#   codigosp=subadmincodes[orden$ix[which(orden$x<0.5)][1],c(1:2)]
#   
#   if(all(!is.na(codigosp))){
#     codigotz=names(which.max(table(ciudades$timezone[which(ciudades$admin1.code==codigosp[2])])))
#     offset=timezones[which(codigotz==timezones[,2] & codigo==timezones[,1]),c(3,4)]
#     
#     prueba[i,]=as.matrix(cbind(candidatos[i,11],paste(codigosp,collapse="."),cbind(codigotz,offset)))
#     
#     #hoteles[which(hoteles$country_code==codigo)[i],match(c("Time_zone","DST"),colnames(hoteles))]=offset
#     #cat(candidatos$province[i],codigosp,"\n")
#   }
# }


prueba=mclapply(1:tamaño,function(i){
  cat(i,"de",tamaño,"\n")
  orden=sort.int(stringdist(gsub(","," ",str_to_upper(iconv(subadmincodes[,2],to="ASCII//TRANSLIT"))),
                            gsub(".*\\, ","",candidatos[i,11]),method="jw",weight = c(1,1,0.5)),
                 decreasing=F,index.return = T)
  
  codigosp=subadmincodes[orden$ix[which(orden$x<0.5)][1],c(1:2)]
  
  if(all(!is.na(codigosp))){
    codigotz=names(which.max(table(ciudades$timezone[which(ciudades$admin1.code==codigosp[2])])))
    offset=timezones[match(codigotz,timezones[,2]),c(3,4)]
    
    return(as.matrix(cbind(candidatos[i,11],paste(codigosp,collapse="."),cbind(codigotz,offset))))
  }else{
    return(as.matrix(cbind(candidatos[i,11],NA,NA,NA)))
  }
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 6L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)

save(prueba,file="prueba.RData")

temp=unlist(prueba)
resultados=matrix(unlist(prueba),nrow=length(temp)/5,ncol = 5,byrow=T)

