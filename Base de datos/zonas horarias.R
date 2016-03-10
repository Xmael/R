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
                              gsub(".*\\, ","",str_to_upper(iconv(candidatos[i,11],to="ASCII//TRANSLIT"))),method="jw",weight = c(1,1,0.5)),
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
  
#save(prueba,file="prueba.RData")

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

pb=txtProgressBar(min = 0, max = nrow(tabla), initial = 0, char = "=",width = NA, title, label, style = 3)
for(i in 1:nrow(tabla)){
  setTxtProgressBar(pb, i)
  hoteles[which(hoteles$country_code==tabla[i,1]),35]=tabla[i,2]
  hoteles[which(hoteles$country_code==tabla[i,1]),36]=tabla[i,3]
}


admincodes=reconstruye.1(admincodes.1)
#admincodes=reconstruye.2(admincodes.2)

multizona=function(codigo){
  cat(codigo,"\n")
  
  cat("Ciudades\n")
  ciudades=read.csv(file=paste("/home/ismael/Trivago/Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="",na.strings = NULL)
  colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                       "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                       "population","elevation","dem","timezone","modification.date")
  
  candidatos=hoteles[which(hoteles$country_code==codigo),]
  tamaño=nrow(candidatos)
  cat(tamaño,"\n")
  ciudades$latitude=as.numeric(ciudades$latitude)
  ciudades$longitude=as.numeric(ciudades$longitude)
  
  if(tamaño==0){
    cat("No hay hoteles en este país\n")
    return("No hay hoteles en este país")
  }
  
  subadmincodes=admincodes[which(admincodes[,1]==codigo),]
  
   pb=txtProgressBar(min = 0, max = tamaño, initial = 0, char = "=",
                     width = NA, title, label, style = 3)
  prueba=mclapply(1:nrow(candidatos),function(i,ciudades,candidatos){
      setTxtProgressBar(pb, i)
     write(paste(i,"de",tamaño),file="progreso",append = T)
    if(as.numeric(candidatos[i,c(13)]) > -90 & as.numeric(candidatos[i,c(14)]) > -360 & all(!is.na(candidatos[i,c(14,13)])) & as.numeric(candidatos[i,c(13)]) > -180){
      distancias=distGeo(as.matrix(ciudades[,c(6,5)]),rbind(apply(candidatos[i,c(14,13)],2,as.numeric))
                         ,a=6378137, f=1/298.257223563)
      
      indice=order(distancias,decreasing=F)[1]
      if(distancias[indice]/1000<5){
        codigotz=ciudades$timezone[indice]
      }else{
        codigotz=NA
      }
      offset=timezones[match(codigotz,timezones[,2]),c(3,4)]
      return(as.matrix(cbind(candidatos[i,11],cbind(codigotz,offset))))
    }else{
      return(as.matrix(cbind(candidatos[i,11],NA,NA,NA)))
    }
  },ciudades,candidatos,mc.preschedule = TRUE, mc.set.seed = TRUE,
  mc.silent = FALSE, mc.cores = getOption("mc.cores", 6L),
  mc.cleanup = TRUE, mc.allow.recursive = TRUE)
  
  temp=unlist(prueba)
  resultados=matrix(unlist(prueba),nrow=length(prueba),ncol = 4,byrow=T)
  cat("\n")
  return(resultados)
}

for(j in 1:length(indices)){
  zonas=multizona(names(resumen)[indices][j])
  hoteles[which(hoteles$country_code==names(resumen)[indices][j]),c(35,36)]=zonas[,c(3,4)]
}

####Búsqueda para USA####

####Esto está por desarrollar aun y sería buscando por condados######
ciudades=read.csv(file=paste("Countries/","US",".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="",na.strings = NULL)
colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")
ciudades$name=str_to_upper(ciudades$name)
candidatos=hoteles[which(hoteles$country_code=="US"),]
resumen=cbind(candidatos$city,candidatos$province,admincodes1[match(candidatos$province,admincodes1[,4]),2:3])
estados=unique(ciudades$admin1.code)
mapeo_ciudades=lapply(1:length(estados),function(i){ciudades[which(ciudades$admin1.code==estados[i]),]})
names(mapeo_ciudades)=estados
resumen=cbind(resumen,timezone=NA)
for(i in 1:nrow(resumen)){
  cat(i,"de",nrow(resumen),"\n")
  if(!is.na(resumen[i,3])){
    zona=mapeo_ciudades[[match(resumen[i,3],estados)]][match(str_to_upper(resumen[[i,1]]),mapeo_ciudades[[match(resumen[i,3],estados)]]$name),"timezone"]
    if(!is.null(zona)){
    resumen[i,"timezone"]=zona
    }
  }
}
###########

zonas=multizona("US")
resumen2=unique(zonas[,c(1,2)])
#Completa las zonas horarios de los condados que sí han aparecido alguna vez en resumen2
resumen2[which(is.na(resumen2[,2])),2]=resumen2[which(!is.na(resumen2[,2])),][match(resumen2[which(is.na(resumen2[,2])),1],resumen2[which(!is.na(resumen2[,2])),1]),2] 
resumen2[which(is.na(resumen2[,2])),2]="America/New_York" #Las que no hemos encontrado las igualamos a la hora de la costa este

pb=txtProgressBar(min = 0, max = nrow(zonas), initial = 0, char = "=",width = NA, title, label, style = 3)
for(i in 1:nrow(zonas)){
  setTxtProgressBar(pb, i)
  if(is.na(zonas[i,2])){
    candidatos=rbind(resumen2[match(zonas[i,1],resumen2[,1]),])
    if(nrow(candidatos)==1){
      zonas[i,2]=candidatos[,2]
      zonas[i,c(3,4)]=unlist(timezones[match(zonas[i,2],timezones[,2]),c(3,4)],use.names = F)
    }
  }
}

hoteles[which(hoteles$country_code=="US"),c(35,36)]=zonas[,c(3,4)]

#Añadimos el estado al final del nombre de la ciudad en el campo city
subadmincodes=admincodes1[admincodes1[,1]=="US",]
pb=txtProgressBar(min = 0, max = nrow(hoteles[which(hoteles$country_code=="US"),]), initial = 0, char = "=",width = NA, title, label, style = 3)
hoteles$city=str_to_upper(hoteles$city)
for(i in 1:nrow(hoteles[which(hoteles$country_code=="US"),])){
  setTxtProgressBar(pb, i)
  ciudad=hoteles$city[which(hoteles$country_code=="US")][i]
  provincia=hoteles$province[which(hoteles$country_code=="US")][i]
  estado=subadmincodes[match(provincia,subadmincodes[,4]),2]
  if(!is.na(estado)){
    hoteles$city[which(hoteles$country_code=="US")][i]=paste(ciudad,estado,sep=", ")
  }
}

#Completamos primero Nepal

hoteles$timezone[which(is.na(hoteles$timezone) & hoteles$country_code=="NP")]=5.75
hoteles$dst[which(is.na(hoteles$dst) & hoteles$country_code=="NP")]=5.75

hoteles$timezone[which(is.na(hoteles$timezone) & hoteles$country_code=="XK")]=1
hoteles$dst[which(is.na(hoteles$dst) & hoteles$country_code=="XK")]=2
  
paises=unique(hoteles$country_code[which(is.na(hoteles$timezone))])
for(i in 1:length(paises)){
  hoteles$timezone[which(is.na(hoteles$timezone) & hoteles$country_code==paises[i])]=names(which.max(table(hoteles$timezone[which(hoteles$country_code==paises[i])])))
  hoteles$dst[which(is.na(hoteles$dst) & hoteles$country_code==paises[i])]=names(which.max(table(hoteles$dst[which(hoteles$country_code==paises[i])])))
}