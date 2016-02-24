library(stringr)
library(stringdist)
library(parallel)

setwd("~/Trivago")

countries=read.csv("countries.csv", sep=";",head=F,na.strings = "NULL",stringsAsFactors = F)
colnames(countries)=c("country_code","country")


cat("Zonas horarias\n")
timezones=read.csv(file="timeZones.txt",sep="\t",header=T)
# hoteles=read.csv(file="hoteles_full.csv",sep=";",header=T,stringsAsFactors = F)
# hoteles$Time_zone=NA
# hoteles$DST=NA

cat("Hoteles\n")
#load("hoteles(12.1.16).RData")

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

reconstruye.2=function(admincodes){
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

cat("Admincodes\n")

admincodes.1=read.csv(file="admin1CodesASCII.txt",sep="\t",header=F,stringsAsFactors = F)
admincodes.2=read.csv(file="admin2Codes.txt",sep="\t",header=F,stringsAsFactors = F)


admincodes=reconstruye.1(admincodes.1)
#admincodes=reconstruye.2(admincodes.2)


horarios=function(i){
  codigo=countries$country_code[i]
  cat(codigo,countries$country[i],"\n")
  
  cat("Ciudades\n")
  ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="",na.strings = NULL)
  colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                       "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                       "population","elevation","dem","timezone","modification.date")
  
  candidatos=hoteles[which(hoteles$country_code==codigo),]
  tamaño=nrow(candidatos)
  cat(tamaño,"\n")
  
  if(tamaño==0){
    cat("No hay hoteles en este país\n")
    return("No hay hoteles en este país")
  }
  
  subadmincodes=rbind(admincodes[which(admincodes[,1]==codigo),])
  
  zonas=unique(ciudades$timezone)
  usos=timezones[which(timezones[,2]%in%zonas),c(3,4)]
  
  if(length(unique(usos[,1]))==1 & length(unique(usos[,2]))==1){
    cat("Un único uso horario\n")
    return(c(unique(usos[,1]),unique(usos[,2])))
  }else{
    resultado=dos(codigo,candidatos,ciudades,subadmincodes,tamaño)
    return(resultado)
  }
} 


######
dos=function(codigo,candidatos,ciudades,subadmincodes,tamaño){
  pb=txtProgressBar(min = 0, max = tamaño, initial = 0, char = "=",
                 width = NA, title, label, style = 3)
  prueba=mclapply(1:tamaño,function(i){
    setTxtProgressBar(pb, i)
    orden=sort.int(stringdist(gsub(","," ",str_to_upper(iconv(subadmincodes[,3],to="ASCII//TRANSLIT"))),
                              gsub(".*\\, ","",candidatos[i,11]),method="jw",weight = c(1,1,0.5)),
                   decreasing=F,index.return = T)
    
    codigosp=subadmincodes[orden$ix[which(orden$x<0.5)][1],c(1:2)]
    
    if(all(!is.na(codigosp))){
      codigotz=names(which.max(table(ciudades$timezone[which(ciudades$admin1.code==codigosp[2])])))
      offset=timezones[match(codigotz,timezones[,2]),c(3,4)]
      
      return(as.matrix(cbind(candidatos[i,11],paste(codigosp,collapse="."),cbind(codigotz,offset))))
    }else{
      return(as.matrix(cbind(candidatos[i,11],NA,NA,NA,NA)))
    }
  },mc.preschedule = TRUE, mc.set.seed = TRUE,
  mc.silent = FALSE, mc.cores = getOption("mc.cores", 1L),
  mc.cleanup = TRUE, mc.allow.recursive = TRUE)
  temp=unlist(prueba)
  resultados=matrix(unlist(prueba),nrow=length(prueba),ncol = 5,byrow=T)
  cat("\n")
  return(cbind(unique(resultados[,4]),unique(resultados[,5])))
}

resumen=sapply((1:nrow(countries))[-c(8,234)],horarios)
names(resumen)=countries$country_code[(1:nrow(countries))[-c(8,234)]]
  
save(prueba,file="prueba.RData")

# for(i in 1:length(prueba)){
#   if(ncol(prueba[[i]])==4){
#     prueba[[i]]=cbind(prueba[[i]],NA)
#   }
# }

tabla=NULL
for(i in 1:length(resumen)){
  if(nrow(rbind(na.omit(resumen[[i]])))==1){
    if(all(na.omit(resumen[[i]]!="No hay hoteles en este país"))){
      tabla=rbind(tabla,cbind(names(resumen)[[i]],rbind(na.omit(resumen[[i]]))))
    }
  }
}

indices=(na.omit(sapply(1:length(resumen),function(i){
  if(nrow(rbind(na.omit(resumen[[i]])))>1){
    return(i)
  }else{
    return(NA)
  }
},simplify = T)))
names(resumen)[indices]
   
   
modifica1=function(i){
  hoteles[which(hoteles$country_code==tabla[i,1]),35]=tabla[i,2]
  hoteles[which(hoteles$country_code==tabla[i,1]),36]=tabla[i,3]
  assign("hoteles",hoteles,.GlobalEnv)
}

for(i in 1:nrow(tabla)){
  modifica1(i)
}


multizona=function(codigo){
  cat(codigo,"\n")
  
  cat("Ciudades\n")
  ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="",na.strings = NULL)
  colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                       "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                       "population","elevation","dem","timezone","modification.date")
  
  candidatos=hoteles[which(hoteles$country_code==codigo),]
  tamaño=nrow(candidatos)
  cat(tamaño,"\n")
  
  if(tamaño==0){
    cat("No hay hoteles en este país\n")
    return("No hay hoteles en este país")
  }
  
  subadmincodes=admincodes[which(admincodes[,1]==codigo),]
  
  pb=txtProgressBar(min = 0, max = tamaño, initial = 0, char = "=",
                    width = NA, title, label, style = 3)
  prueba=mclapply(1:tamaño,function(i){
    setTxtProgressBar(pb, i)
    orden=sort.int(stringdist(gsub(","," ",str_to_upper(iconv(subadmincodes[,4],to="ASCII//TRANSLIT"))),
                              gsub("\\,.*","",candidatos[i,11]),method="jw",weight = c(1,1,0.5)),
                   decreasing=F,index.return = T)
    
    codigosp=subadmincodes[orden$ix[which(orden$x<0.5)][1],c(1:2)]
    subadmincodes[orden$ix[which(orden$x<0.5)][1],]
    
    if(all(!is.na(codigosp))){
      codigotz=names(which.max(table(ciudades$timezone[which(ciudades$admin1.code==codigosp[2])])))
      offset=timezones[match(codigotz,timezones[,2]),c(3,4)]
      
      return(as.matrix(cbind(candidatos[i,11],paste(codigosp,collapse="."),cbind(codigotz,offset))))
    }else{
      return(as.matrix(cbind(candidatos[i,11],NA,NA,NA,NA)))
    }
  },mc.preschedule = TRUE, mc.set.seed = TRUE,
  mc.silent = FALSE, mc.cores = getOption("mc.cores", 1L),
  mc.cleanup = TRUE, mc.allow.recursive = TRUE)
  temp=unlist(prueba)
  resultados=matrix(unlist(prueba),nrow=length(prueba),ncol = 5,byrow=T)
  cat("\n")
  return(resultados)
}

prueba=multizona(names(resumen)[indices][1])


hoteles[which(hoteles$country_code==codigo),35]=1
hoteles[which(hoteles$country_code==codigo),36]=2

hoteles[which(hoteles$country_code==codigo),]

hoteles[which(hoteles$country_code==codigo & hoteles$province==provincias_canarias[7]),35]=0
hoteles[which(hoteles$country_code==codigo & hoteles$province==provincias_canarias[7]),36]=1
