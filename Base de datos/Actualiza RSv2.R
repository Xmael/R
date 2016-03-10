library(XML)
library(parallel)
library(RJSONIO)
library(RCurl)
library(RMySQL)
library(stringr)
library(knitr)
library(geosphere)



# Conecta base de datos ---------------------------------------------------
con <- dbConnect(MySQL(), user="root", password="papidorl", 
                 dbname="hoteles", host="localhost",client.flag=CLIENT_MULTI_STATEMENTS)


# Carga descriptores ------------------------------------------------------
mapped_countries=dbGetQuery(conn = con,statement = "select * from mapped_countries where locale='en'")
mapped_countries=mapped_countries[order(mapped_countries$code),]
mapped_countries=mapped_countries[,-2]
load("~/Trivago/Datos/hoteles(25.1.16).RData")
countries=read.csv("/home/ismael/Trivago/countries.csv",sep=";",stringsAsFactors = F,header=F,na="NULL")
colnames(countries)=c("country_code","country")

codigos_mal=cbind(mapped_countries,cambio=countries$country_code[match(mapped_countries$code,countries$country_code)])
codigos_mal$cambio=as.character(codigos_mal$cambio)
codigos_mal$cambio[which(is.na(codigos_mal$cambio))]=c("SA","MO","AN","IE","IC","KZ","LV","UA","GB","XK")  #Hay que mapear de nuevo las Islas del canal. Y sí, son los únicos gilipollas que le han asignado un código ISO a Kosovo


# Parsea XML ---------------------------------------------------------------
doc=xmlParse("/home/ismael/Trabajo/Errores/restel-2016-01-28.xml", useInternalNodes = TRUE)
doc=xmlRoot(doc)
doc=xmlRoot(doc)

codigos_restel=mclapply(1:xmlSize(doc[[4]][[1]]),function(i){ 
  cat(i,"de",xmlSize(doc[[4]][[1]]),"\n")
  c1=xmlValue(doc[[4]][[1]][[i]][[3]])
  c2=xmlValue(doc[[4]][[1]][[i]][[2]])
  c3=xmlValue(doc[[4]][[1]][[i]][[1]])
  c4=xmlValue(doc[[4]][[1]][[i]][[4]])
  return(c(c1,c2,c3,c4))
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 7L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)

save(codigos_restel,file="codigos_restel.RData")


load(file="codigos_restel.RData")
load("~/Trivago/Datos/hoteles(25.1.16).RData")
codigos_restel=data.frame(matrix(unlist(codigos_restel),nrow=length(codigos_restel),ncol=4,byrow = T),stringsAsFactors = F)
colnames(codigos_restel)=c("npi","gprovince_id","province_code","prov_id")
#Metemos primero los códigos de provincia de RESTEL. Las provincias las obtenemos como siempre.
restel=hoteles[hoteles$prov=="RS",] #Seleccionamos de la base de datos los hoteles de restel
incluidos=restel$prov_id[which(restel$prov_id %in% codigos_restel$prov_id)] #obtengo los códigos de restel que aun son válidos
nuevos=codigos_restel$prov_id[-which(codigos_restel$prov_id %in% restel$prov_id)]

#  save(nuevos,file="nuevos.RData")

indices_nuevos=c(1:nrow(codigos_restel))[-which(codigos_restel$prov_id %in% restel$prov_id)]

restel=restel[which(restel$prov_id %in% incluidos),] #Nos quedamos solamente con los que tienen código válido


# Crea lista de hoteles nuevos --------------------------------------------

restel_nuevos=data.frame(matrix(NA,nrow=length(nuevos),ncol=18,byrow = T))
colnames(restel_nuevos)=c("province","city","name","category","country","hotel_checkin","lon","hotel_id","hotel_checkout",
                          "address","lat","cp","provider_tlf","hotelService","roomServices","shortdesc","desc","fotos")

load("nuevos.RData")

for(i in 1:length(nuevos)){
  cat(i,"de",length(nuevos),"\n")
  #  peticion=paste("curl http://localhost:5000/es/raw/content/RS/",nuevos[i]," --max-time 10",sep="")
  #  temp=system(peticion,ignore.stderr = F,intern = T)
  temp=getURI(paste("http://localhost:5000/es/raw/content/RS/",nuevos[i],sep=""),
              .opts = list(timeout = 10))
  retry=1
  
  while(length(temp)==0){
    cat("Retry",retry,"\n")
    temp=system(peticion,ignore.stderr = T,intern = T)
    retry=retry+1
    Sys.sleep(2)		
  }
  
  if(isValidJSON(temp,asText = T)){
    temp=fromJSON(temp,simplify = F)
    feature=c(1,2,4,5,6,7,8,9,10,12,13,15,16)
    
    if(class(temp$fotos)=="AsIs"){
      fotos=NA
    }else{
      fotos=paste(unlist(temp$fotos),collapse="|")
    }
    
    if(class(temp$hotelServices)=="AsIs"){
      hs=NA
    }else{
      hs=paste(sapply(1:length(temp$hotelServices),function(i) temp$hotelServices[[i]]$service),collapse="|")
    }
    
    if(class(temp$roomServices)=="AsIs"){
      rs=NA
    }else{
      rs=paste(sapply(1:length(temp$roomServices),function(i) temp$roomServices[[i]]$service),collapse="|")
    }
    
    if(class(temp$desc$shortDescription)=="AsIs"){
      sdesc=NA
    }else{
      sdesc=temp$desc$shortDescription
    }
    
    if(class(temp$desc$desc)=="AsIs"){
      desc=NA
    }else{
      desc=temp$desc$desc
    }
    
    final=rbind(unlist(c(temp[feature],hs,rs,sdesc,desc,fotos),use.names = F))
    final[which(final=="")]=NA
    #    final=cbind(i,final,deparse.level=0)
    restel_nuevos[i,]=final
  }else{
    restel_nuevos[i,]=c(NA,NA,NA,NA,NA,NA,NA,nuevos[i],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  }
}

load("/home/ismael/Trivago/Datos/hoteles(25.1.16).RData")


# Inserta directamente algunos campos -------------------------------------
codigos_paises_restel=unique(restel_nuevos$country)
cbind(codigos_paises_restel,countries_int[match(codigos_paises_restel,countries_int$country_code),])
restel_nuevos$country=codigos_mal[match(restel_nuevos$country,codigos_mal[,1]),"cambio"]
giata=dbGetQuery(con, "select * from giataproviders")
giatahotels=dbGetQuery(con, "select * from giatahotels")
giata=giata[which(giata$providerCode=="restel"),]
restel=data.frame(matrix(NA,nrow=nrow(restel_nuevos),ncol=ncol(hoteles)))
colnames(restel)=colnames(hoteles)
restel$prov="RS"
restel$city=restel_nuevos$city
restel$province=restel_nuevos$province
restel$name=restel_nuevos$name
restel$lat=restel_nuevos$lat
restel$lng=restel_nuevos$lon
restel$cp=restel_nuevos$cp
restel$phone=restel_nuevos$provider_tlf

restel$prov_id=nuevos
restel$giata_id=giata$giata_id[match(restel$prov_id,giata$value)]
restel[,c(20,17)]=giatahotels[match(restel$giata_id,giatahotels$id),c(5,6)] #Insertas gcity y gcountry_code
restel$gcountry=countryid$country[match(restel$gcountry_code,countryid$country_code)] #Inserta gcountry a partir del gcountry_code usando nuestra lista de países
restel$country_code=restel_nuevos$country
restel$country=countries$country[match(restel$country_code,countries$country_code)]
restel$stars=restel_nuevos$category
restel$gprovince=restel_nuevos$province
#########

#Vamos a corregir unicamente los nombres de las ciudades españolas
codigo="ES"
ciudades=read.csv(file=paste("~/Trivago/Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="")
colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")
ciudadesp1=ciudades[which(ciudades$feature.code=="ADM3"),]
ciudadesp2=ciudades[which(ciudades$feature.class=="P"),]
#ciudadestemp=cbind(restel$city,hoteles$city[which(hoteles$prov=="RS")][match(restel$prov_id,hoteles$prov_id[which(hoteles$prov=="RS")])]) #Reutilizamos los nombres de ciudades que tenemos ya en la base de datos
indices=which(restel$country_code=="ES") #Se corresponde con aquellos indices de hoteles en España que no tenemos en la base de datos para buscarlos a continuación
candidatos_nombre=NULL
for(i in 1:nrow(restel[indices,])){
  cat(i,"de",length(indices),restel$city[indices[i]]," => ")
  coordenadas=rbind(apply(restel[indices[i],c(14,13)],2,as.numeric))
  if(any(!is.na(coordenadas))){
    distancia=distGeo(rbind(apply(ciudadesp1[,c(6,5)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
    lista=ciudadesp1$name[order(distancia)[1:10]]
    nombre=gsub("/.*|\\(.*|-.*","",iconv(restel$city[indices[i]],to="ASCII//TRANSLIT"))
    candidatos_nombre=rbind(candidatos_nombre,cbind(restel$city[indices[i]],
                                                    lista[order(stringdist(iconv(str_to_upper(lista),to="ASCII//TRANSLIT"),nombre,method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]))
  }else{
    jw=ciudadesp1$name[order(stringdist(iconv(str_to_upper(ciudadesp1$name),to="ASCII//TRANSLIT"),iconv(str_to_upper(restel$city[indices[i]]),to="ASCII//TRANSLIT"),method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]
    candidatos_nombre=rbind(candidatos_nombre,cbind(restel$city[indices[i]],jw))
  }
  cat(candidatos_nombre[i,2],"\n")
}

candidatos_nombre=cbind(candidatos_nombre,NA)
for(i in 1:nrow(restel[indices,])){
  cat(i,"de",length(indices),restel$city[indices[i]]," => ")
  coordenadas=rbind(apply(restel[indices[i],c(14,13)],2,as.numeric))
  if(any(!is.na(coordenadas))){
    distancia=distGeo(rbind(apply(ciudadesp2[,c(6,5)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
    lista=ciudadesp2$name[order(distancia)[1:10]]
    nombre=gsub("/.*|\\(.*|-.*","",iconv(restel$city[indices[i]],to="ASCII//TRANSLIT"))
    candidatos_nombre[i,3]=lista[order(stringdist(iconv(str_to_upper(lista),to="ASCII//TRANSLIT"),nombre,method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]
  }else{
    jw=ciudadesp2$name[order(stringdist(iconv(str_to_upper(ciudadesp2$name),to="ASCII//TRANSLIT"),iconv(str_to_upper(restel$city[indices[i]]),to="ASCII//TRANSLIT"),method="jw",weight = c(0.5,0.5,1)),decreasing=F)[1]]
    candidatos_nombre[i,3]=jw
  }
  cat(candidatos_nombre[i,3],"\n")
}


sugerencias=NULL
confirma=function(k){
  for(i in k:nrow(candidatos_nombre)){
    if(ncol(candidatos_nombre)!=4){
      candidatos_nombre=cbind(candidatos_nombre,NA)
    }
    cat("\014")
    cat(i,"de",nrow(candidatos_nombre),"\n")
    cat(candidatos_nombre[i,1]," => 1. ",candidatos_nombre[i,2], " | 2.", candidatos_nombre[i,3],"\n")
    
    if(all(str_to_upper(iconv(candidatos_nombre[i,1],to="ASCII//TRANSLIT"))!=iconv(str_to_upper(candidatos_nombre[i,2]),to="ASCII//TRANSLIT"),
           str_to_upper(iconv(candidatos_nombre[i,1],to="ASCII//TRANSLIT"))!=iconv(str_to_upper(candidatos_nombre[i,3]),to="ASCII//TRANSLIT"))){
      if(candidatos_nombre[i,2]==restel[indices[i],20] & !is.na(restel[indices[i],20])){
        candidatos_nombre[i,4]=candidatos_nombre[i,2]
      }else if(candidatos_nombre[i,3]==restel[indices[i],20] & !is.na(restel[indices[i],20])){
        candidatos_nombre[i,4]=candidatos_nombre[i,3]
      }else if(length(which(candidatos_nombre[i,1]==sugerencias[,1] & candidatos_nombre[i,2]==sugerencias[,2] & candidatos_nombre[i,3]==sugerencias[,3]))!=0){
        candidatos_nombre[i,4]=sugerencias[which(candidatos_nombre[i,1]==sugerencias[,1] & candidatos_nombre[i,2]==sugerencias[,2] & candidatos_nombre[i,3]==sugerencias[,3]),4]
        cat("APLICA SUGERENCIA\n")
      }else{
        coordenadas=rbind(apply(ciudadesp2[which(ciudadesp2$name==candidatos_nombre[i,3]),c(6,5)],2,as.numeric))
        distancia=distGeo(rbind(apply(ciudadesp1[,c(6,5)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
        propuesta=cbind(ciudadesp1$name[order(distancia)[1:10]],sort(distancia)[1:10])
        colnames(propuesta)=c("Ciudades Cercanas","Distancia")
        print(propuesta)
        print(kable(restel[indices[i],c(3,5,6,12,23,20)]))
        #  cat("Proveedor#",raw_text(aux),"\n")
        lee=readline(prompt = "Opcion [1]/[2]/Nombre: ")
        if(lee==1){
          candidatos_nombre[i,4]=candidatos_nombre[i,2]
          sugerencias=rbind(sugerencias,candidatos_nombre[i,])
        }else if(lee==2){
          candidatos_nombre[i,4]=candidatos_nombre[i,3]
          sugerencias=rbind(sugerencias,candidatos_nombre[i,])
        }else{
          candidatos_nombre[i,4]=str_to_upper(lee)
          sugerencias=rbind(sugerencias,candidatos_nombre[i,])
        }
      }
    }else if(str_to_upper(iconv(candidatos_nombre[i,1],to="ASCII//TRANSLIT"))==iconv(str_to_upper(candidatos_nombre[i,2]),to="ASCII//TRANSLIT")){
      candidatos_nombre[i,4]=str_to_upper(candidatos_nombre[i,2])
      cat("\n\n")
    }else if(str_to_upper(iconv(candidatos_nombre[i,1],to="ASCII//TRANSLIT"))==iconv(str_to_upper(candidatos_nombre[i,3]),to="ASCII//TRANSLIT")){
      candidatos_nombre[i,4]=str_to_upper(candidatos_nombre[i,3])
      cat("\n\n")
    }
    assign('candidatos_nombre',candidatos_nombre,envir=.GlobalEnv)
    assign('sugerencias',sugerencias,envir=.GlobalEnv)
  }
}

confirma(i)
restel$city[indices]=str_to_upper(candidatos_nombre[,4]) #Introducimos los nombres que hemos corregido en el paso anterior
restel$country=str_to_upper(restel$country)
#restel$city[-indices]=str_to_upper(ciudadestemp[-indices,2]) #Introducimos los que teníamos en la base de datos sin pisar los que hemos buscado antes
#restel$city[which(is.na(restel$city))]=contacts[match(restel$prov_id,contacts$prov_id),4][which(is.na(restel$city))] #Metemos el resto de lo que nos ha facilitado el proveedor


#####Buscamos códigos giata######
candidatos=restel[which(is.na(restel$giata_id)),]
resumen=data.frame(matrix(NA,nrow = nrow(candidatos),ncol=7))
colnames(resumen)=c("Candidato","city","name","gcity","giata_id","prov","distancia")

pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=", width = NA, title, label, style = 3)
for(i in 1:nrow(resumen)){
  setTxtProgressBar(pb, i)
  subcandidatos=hoteles[which(hoteles$country_code==candidatos$country_code[i] & hoteles$city==candidatos$city[i] & hoteles$prov!="RS"),]
  
  resultado.1=sort.int(stringdist(str_to_upper(gsub(" \\(.*| - NON REFUNDABLE ROOM","",candidatos$name[i])),str_to_upper(subcandidatos$name),method="jw",weight = c(0.1,0.5,1)),index.return = T)
  if(candidatos$lng[i]>-360 & !is.na(candidatos$lng[i])){
    distancia=distGeo(rbind(apply(subcandidatos[resultado.1$ix[which(resultado.1$x<=0.5)][1],c(14,13)],2,as.numeric)),rbind(apply(candidatos[i,c(14,13)],2,as.numeric)),a=6378137, f=1/298.257223563)/1000
  }else{
    distancia=NA
  }
  resumen[i,]=c(Candidatos=candidatos$name[i],candidatos$city[i],subcandidatos[resultado.1$ix[which(resultado.1$x<=0.5)][1],c(6,12,5,2)],distancia)
  # subcandidatos[resultado.1$ix[1],]
  direccion=subcandidatos$address[resultado.1$ix[which(resultado.1$x<=0.5)]][1]
  score=stringdist(str_to_upper(gsub(" \\(.*| - NON REFUNDABLE ROOM","",resumen[i,1])),str_to_upper(resumen[i,3]),method="cosine")
  score2=sort.int(stringdist(str_to_upper(candidatos$address[i]),str_to_upper(direccion),method="cosine",weight = c(0.1,0.5,1)),index.return=T)$x[1]
  if(score>0.1 & !is.na(score) & str_to_upper(resumen$city[i])!=str_to_upper(resumen$gcity[i])){
    resumen$giata_id[i]=NA
  }else if(score>0.3 & !is.na(score) & str_to_upper(resumen$city[i])==str_to_upper(resumen$gcity[i]) & score2>0.1 & !is.na(score2)){
    resumen$giata_id[i]=NA
  }
}


resumen.2=NULL
#colnames(resumen.2)=c("Candidato","city","name","gcity","giata_id","prov")
pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=", width = NA, title, label, style = 3)
hoteles2=hoteles[which(hoteles$prov!="RS" & !is.na(hoteles$giata_id)),]
for(i in 1:nrow(candidatos)){
  cat(i,"\n")
  temp=hoteles2[which(str_to_upper(hoteles2$name) %in% str_to_upper(candidatos$name[i]) & hoteles2$city==candidatos$city[i]),c("name","city","giata_id","prov")][1,]
  if(all(!is.na(temp))){
    resumen.2=rbind(resumen.2,cbind(i,candidatos$name[i],candidatos$city[i],temp))
  }
}
resumen=resumen[-which(row.names(resumen) %in% resumen.2[,1]),]
candidatos[as.integer(rownames(resumen)),"giata_id"]=resumen$giata_id
candidatos[as.integer(rownames(resumen)),"gcountry_code"]=giatahotels$country[match(candidatos[as.integer(rownames(resumen)),"giata_id"],giatahotels$id)]
candidatos[as.integer(rownames(resumen)),"gcountry"]=str_to_upper(countries_int$country[match(candidatos[as.integer(rownames(resumen)),"gcountry_code"],countries_int$country_code)])

candidatos[resumen.2[,1],"giata_id"]=resumen.2$giata_id
candidatos[resumen.2[,1],"gcountry_code"]=giatahotels$country[match(candidatos[resumen.2[,1],"giata_id"],giatahotels$id)]
candidatos[resumen.2[,1],"gcountry"]=str_to_upper(countries_int$country[match(candidatos[resumen.2[,1],"gcountry_code"],countries_int$country_code)])

restel[match(row.names(candidatos),row.names(restel)),]=candidatos
########



####Utilizamos las provincias proporcionadas por Restel para meter los gprovince_id. Luego cambiamos los nombres de las provincias para que sean homogéneos.

restel$gprovince_id=codigos_restel$gprovince_id[match(restel$prov_id,codigos_restel$prov_id)]


##################
####Provincias####
##################

##Buscamos primero los que tienen coordenadas

candidatos=restel[which(!is.na(restel$lat)),]
ciudades1000=read.csv2(file="/home/ismael/Trivago/cities1000.txt",sep="\t",header=F,stringsAsFactors = F,quote="",na="NULL")
colnames(ciudades1000)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                         "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                         "population","elevation","dem","timezone","modification.date")
admincodes1=read.csv(file="/home/ismael/Trivago/admin2Codes.txt",sep="\t",header=F,stringsAsFactors = F)
admincodes2=read.csv(file="/home/ismael/Trivago/admin1CodesASCII.txt",sep="\t",header=F,stringsAsFactors = F)
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
admincodes1=reconstruye(admincodes1)
admincodes2=reconstruye(admincodes2)

#Busco primero las provincias por coincidencia directa en la base de datos de ciudades con más de 1000 habitantes
for(i in 1:nrow(restel)){
  cat(i,"de",nrow(restel),restel$city[i],"\n")
  codigo<-ciudades1000[which(ciudades1000$country_code==restel$country_code[i] & 
                               str_to_upper(iconv(ciudades1000$name,to="ASCII//TRANSLIT"))==str_to_upper(iconv(restel$city[i],to="ASCII//TRANSLIT"))),
                       c("country_code","admin1.code","admin2.code","admin3.code")]
  if(nrow(codigo)!=1){
    cat("No se encuentra",restel$city[i],"\n")
  }else if(nrow(codigo)==1){
    temp=admincodes1[which(codigo[,1]==admincodes1[,1] & codigo[,2]==admincodes1[,2] & codigo[,3]==admincodes1[,3]),4]
    if(length(temp)==1){
      restel$province[i]=temp
      cat(restel$province[i],"\n")
    }else if(length(temp)!=1){
      temp=admincodes2[which(codigo[,1]==admincodes2[,1] & codigo[,2]==admincodes2[,2]),4]
      if(length(temp)==1){
        restel$province[i]=temp
        cat(restel$province[i],"\n")
      }else if(length(temp)!=1){
        cat("No se encuentra",restel$city[i],"\n")
      }
    }
  }
}

#Busco el resto mediante coordenadas y por proximidad a ciudades con más de 1000 habitantes
#En regiones con poca densidad puede ubicar las poblaciones en provincias equivocadas
restel$lat=as.numeric(restel$lat)
restel$lng=as.numeric(restel$lng)
indices=which(is.na(restel$province) & !is.na(restel$lat) & restel$lng>-360 & restel$lat>=-90)

busca=function(i,flag){
  if(flag){
    candidatos=ciudades1000[which(ciudades1000$country_code==restel$country_code[i]),]
  }else{
    candidatos=ciudades1000
  }
  if(nrow(candidatos)!=0){
    coordenadas=restel[i,c(14,13)]
    distancias=distGeo(rbind(apply(coordenadas,2,as.numeric)),rbind(apply(candidatos[,c("longitude","latitude")],2,as.numeric)),a=6378137, f=1/298.257223563)
    codigo=candidatos[order(distancias)[1],c("country_code","admin1.code","admin2.code","admin3.code")]
    if(codigo[,2]==""){
      codigo=candidatos[order(distancias)[2],c("country_code","admin1.code","admin2.code")]
    }
    #  codigo=cbind(names(which.max(table(codigo[,1]))),names(which.max(table(codigo[,2]))),names(which.max(table(codigo[,3]))))
    provincia=admincodes1[which(codigo[,1]==admincodes1[,1] & codigo[,2]==admincodes1[,2]),4]
    if(length(provincia)==1){
      restel$province[i]=provincia
    }else if(length(provincia)!=1){
      provincia=admincodes1[which(codigo[,1]==admincodes1[,1] & codigo[,2]==admincodes1[,2] & codigo[,3]==admincodes1[,3]),4]
      if(length(provincia)==1){
        restel$province[i]=provincia
      }else{
        provincia=admincodes2[which(codigo[,1]==admincodes2[,1] & codigo[,2]==admincodes2[,2]),4]
        if(length(provincia)==1){
          restel$province[i]=provincia
        }else{
          restel$province[i]=NA
        }
      }
    }
    if(!is.na(restel$province[i])){
      restel$country_code[i]=codigo[,1]
    }
  }else{
    restel$province[i]=NA
  }
  if(flag==FALSE & is.na(restel$province[i])){
    restel$country_code[i]=codigo[,1]
  }
  assign("restel",restel,envir=.GlobalEnv)
  cat(match(i,indices),"de",length(indices),restel$city[i],restel$province[i],restel$country_code[i],"\n")
}

for (i in 1:length(indices)){
#  busca(indices[i],flag=T)
#  if(is.na(restel$province[indices[i]])){
    busca(indices[i],flag=F)
#  }
}

#Actualizamos las id de los hoteles nuevos de restel
id=739236 #Habría que buscar este valor máximo en la base de datos

#En esta versión del script estamos trabajando solamente con los nuevos hoteles
for(i in 1:nrow(restel)){
  id=id+1
  restel$id[i]=id
}

######Codigo postal######
cp=read.csv2(file="codigos_postales.txt",header=F,sep = "\t",stringsAsFactors = F)
colnames(cp)=c("country_code","postal_code","admin_name1","admin_code1","admin_name2","admin_code2","admin_name3",
               "admin_code3","lat","lng","accuracy")

codigos=mclapply(1:nrow(restel),function(i){
  cat(i,"de",nrow(restel),restel$country_code[i],"  ")
  if(!is.na(restel$lat[i]) & restel$lng[i]>-360 & restel$lat[i]>=-90){
    coordenadas=rbind(apply(restel[i,c(14,13)],2,as.numeric))
    subcp=cp[which(cp[,1]==restel$country_code[i]),]
    if(nrow(subcp)==0){
      cat(NA,"\n")
      return(c(restel$country_code[i],NA))
    }else{
      distancia=distGeo(rbind(apply(subcp[,c(11,10)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
      if((distancia[order(distancia,decreasing=F)][1]/1000)<5){
        cat(subcp$postal_code[order(distancia,decreasing=F)[1]],"\n")
        return(c(restel$country_code[i],subcp$postal_code[order(distancia,decreasing=F)[1]]))
      }else{
        cat(NA,"\n")
        return(c(restel$country_code[i],NA))
      }
    }
    
  }else{
    cat(NA)
    return(c(restel$country_code[i],NA))
  }
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 7L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)

codigos=matrix(unlist(codigos),nrow=length(codigos),ncol=2,byrow = T)
restel$cp=codigos[,2]
#####Buscamos los que faltan en la base de datos de los hoteles mapeados
candidatos=restel[which(is.na(restel$cp) & !is.na(restel$lat)),]
for(i in 7674:nrow(candidatos)){
  cat(i,"de",nrow(candidatos),candidatos$country_code[i],"  ")
  if(!is.na(candidatos$lat[i]) & candidatos$lng[i]>-360 & candidatos$lat[i]>=-90 & candidatos$lat[i]<=90){
    coordenadas=rbind(apply(candidatos[i,c(14,13)],2,as.numeric))
    subcp=hoteles[which(hoteles$country_code==candidatos$country_code[i] & hoteles$cp!="" & hoteles$prov=="HB" & as.numeric(hoteles$lat)<=90),]
    if(nrow(subcp)==0){
      cat(NA,"\n")
      candidatos$cp[i]=NA
    }else{
      distancia=distGeo(rbind(apply(subcp[,c(14,13)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
      if((distancia[order(distancia,decreasing=F)][1]/1000)<5){
        cat(subcp$cp[order(distancia,decreasing=F)[1]],"\n")
        candidatos$cp[i]=subcp$cp[order(distancia,decreasing=F)[1]]
      }else{
        cat(NA,"\n")
        candidatos$cp[i]=NA
      }
    }
    
  }else{
    cat(NA)
    candidatos$cp[i]=NA
  }
}

restel$cp[which(is.na(restel$cp) & !is.na(restel$lat))]=candidatos$cp

#Completamos el content_id
restel$content_id=restel$id

#Introducimos zonas horarias

hoteles=restel #El data.frame con los hoteles del proveedor tiene que llamarse hoteles

#Ejectuamos el script por pasos

restel=hoteles

#Pasamos el data frame de restel a la base de datos para actualizar 
dbWriteTable(conn=conn,name="restel",restel)
