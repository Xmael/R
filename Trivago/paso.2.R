library(stringr)
library(RANN)
library(stringdist)
library(geosphere)
countries=read.csv("countries.csv", sep=";",head=F,na.strings = "NULL")
countries[,2]=str_to_upper(countries[,2])
colnames(countries)=c("country_code","country")
cities=read.csv("cities1000.txt",sep="\t",quote="",header=F,stringsAsFactors = F)
colnames(cities)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                   "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                   "population","elevation","dem","timezone","modification.date")



######CUIDADO, NO SE ESTÁ MODIFICANDO EL CONJUNTO DE HOTELES. HAY QUE DESCOMENTAR UN PAR DE LÍNEAS

countries=countries[-na.omit(match(c("NA","AN","IO"),countries$country_code)),]

mal=NULL
for(k in 1:nrow(countries)){
  codigo=str_to_upper(as.character(countries$country_code[k]))
  pais=str_to_upper(as.character(countries$country[k]))
  
  ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character")
  colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                       "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                       "population","elevation","dem","timezone","modification.date")
  
  #Si falta la latitud o la longitud no tocamos nada por ahroa
  indices=which(is.na(hoteles$country_code) & hoteles$country==pais & !is.na(hoteles$lat) & !is.na(hoteles$lng))

  if(length(indices)!=0){
    pb=txtProgressBar(min = 0, max = length(indices), initial = 0, char = "=",
                      width = NA, title, label, style = 3)
    for(i in 1:length(indices)){
      setTxtProgressBar(pb, i)
      cat(i,pais,hoteles$city[indices[i]],countries$country[which(countries$country_code=="codigo")],"=> ")
      log=paste(i,codigo,hoteles$city[indices[i]],as.character(candidatos[j,1]),"=> ",sep=" ")
      
      busquedas=data.frame(matrix(NA,nrow=5,ncol=2,byrow = T))
      
      busquedas[1,]=search.1(i,indices)  #Si NA, indica que no existe ninguna ciudad con ese nombre en el pais dado por el codigo del pais
      busquedas[2,]=search.2(i,indices)  #Si NA, probablemente las coordenadas estén equivocadas y lo hayan colocado en el oceáno
      busquedas[3,]=search.3(i,indices)  #Si NA, no existe una ciudad con ese nombre en esas coordenadas
      if(is.na(busquedas[3,1])){busquedas[2,]=busquedas[3,]}
      busquedas[4,]=search.5(i,indices)  #Si NA, giata está mal, bien porque no existe o se metió mal
      busquedas[5,]=search.7(i,indices)  #Comprueba por Giata y por coordenadas próximas en nuestra base de datos
      
      
      colnames(busquedas)=c("codigo_pais","pais")
      
      if(busquedas[4,1] %in% search.6(i,indices)){
        
        busquedas=rbind(busquedas,busquedas[4,])  #Si NA, el uniqID es único y no se puede obtener información a partir de él
      }
      
      if(all(!is.na(search.4b(i,indices)))){
        codigo_pais=na.omit((unique(search.4b(i,indices)[,1])[match(unique(na.omit(busquedas[,1])),unique(search.4b(i,indices)[,1]))]))
      }else{ 
        codigo_pais=names(which.max(table(busquedas[,1])))
      }
      
      if(any(!is.na(codigo_pais))){
        pais=str_to_upper(countries$country[countries$country_code==(na.omit(codigo_pais))])
        hoteles$country_code[indices[i]]=na.omit(codigo_pais)
        hoteles$country[indices[i]]=pais
        cat(codigo_pais,pais,"\n")
        log2=paste(codigo_pais,pais,sep=" ")
      }else{
        mal=rbind(mal,cbind(indices[i],hoteles[indices[i],c(1,2,3,4,5,6,9,10,11,12,13,14)]))
        cat("MAL","MAL","\n")
      }
      write(paste(log,log2,sep=" "),file="log",append=T)
    }
  }else{cat("NO HAY HOTELES\n")}
}

for(i in 1:nrow(hoteles)){
  cat(i,"\n")
  if(is.na(hoteles$country[i]) & !is.na(hoteles$country_code[i])){
    hoteles$country[i]=str_to_upper(countries$country[match(hoteles$country_code[i],as.character(countries$country_code))])
    hoteles$gcountry[i]=hoteles$country[i]
  }
  if(!is.na(hoteles$country[i]) & is.na(hoteles$country_code[i])){
    hoteles$country_code[i]=str_to_upper(countries$country_code[match(hoteles$country[i],str_to_upper(as.character(countries$country)))])
    hoteles$gcountry_code[i]=hoteles$country_code[i]
  }
}

