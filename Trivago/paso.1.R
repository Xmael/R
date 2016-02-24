library(stringr)
library(RANN)
library(stringdist)
library(geosphere)

cities=read.csv("cities1000.txt",sep="\t",quote="",header=F,stringsAsFactors = F)
colnames(cities)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                   "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                   "population","elevation","dem","timezone","modification.date")
hoteles=read.csv(file="hoteles_corregidos_20.01.16.csv",header=T,sep = ";",quote = "",stringsAsFactors = F,na.strings = "NULL")
tourico=read.csv(file="tourico.csv",header=T,sep="|",quote=NULL,stringsAsFactors = F,na.strings = "NULL")
countries=read.csv("countries.csv", sep=";",head=F,na.strings = "NULL",stringsAsFactors = F)
colnames(countries)=c("country_code","country")

#Normaliza los nombres de los paises
paisesmal=NULL
for(i in 1:nrow(hoteles)){
  if(!is.na(hoteles$country[i]) & all(str_to_upper(hoteles$country[i])!=str_to_upper(countries$country))){
    cat(i,"\n")
    paisesmal=rbind(paisesmal,cbind(i,hoteles$country[i]))
  }
}

lista=as.data.frame(unique(paisesmal[,2]))
lista=cbind(lista,NA)
lista=lista[-2,]
lista[,1]=as.character(lista[,1])
for(i in 93:nrow(lista)){
  cat(lista[i,1],"\n")
  lista[i,2]=str_to_upper(readline(prompt = "País: "))
}

correcciones=c("UNITED KINGDOM","UKRAINE","NETHERLANDS ANTILLES","ALGERIA","JORDAN","LEBANON","SOUTH KOREA","ICELAND","IRELAND",
               "MYANMAR","LATVIA","BELIZE","SPAIN","SAINT LUCIA","GUADELOUPE","CAMBODIA","ANTIGUA AND BARBUDA","SINGAPORE","PHILIPPINES",
               "TURKS AND CAICOS ISLANDS","VIRGIN ISLANDS","MAURITIUS","MALDIVES","BELARUS","CAMEROON","MACAO","FIJI","GRENADA",
               "SAINT KITTS AND NEVIS","FRENCH POLYNESIA","TRINIDAD AND TOBAGO","SAUDI ARABIA","CAYMAN ISLANDS","KAZAKHSTAN","ETHIOPIA","NA",
               "UNITED KINGDOM","SAINT BARTHELEMEY","SPAIN","CYPRUS","UNITED ARAB EMIRATES","UNITED KINGDOM","IRELAND","FRANCE","ITALY",
               "GREECE","SWITZERLAND","LATVIA","CZECH REPUBLIC","GERMANY","TURKEY","UNITED STATES","CROATIA","POLAND","HUNGARY","FINLAND",
               "NETHERLANDS","ICELAND","SWEDEN","BELGIUM","MONACO","OMAN","ROMANIA","SAUDI ARABIA","LUXEMBOURG","EGYPT","MOROCCO","DENMARK",
               "NORWAY","JORDAN","LITHUANIA","UKRAINE","RUSSIA","SLOVENIA","LEBANON","BOSNIA AND HERZEGOVINA","TUNISIA","SLOVAKIA","BELARUS",
               "BRAZIL","MOLDOVA","BAHRAIN","BRUNEI","KENYA","UNITED STATES","CUBA","BRITISH INDIAN OCEAN TERRITORY","PORTUGAL","UNITED STATES",
               "MACAO","South Pacific","CYPRUS","FRANCE","PORTUGAL","VIRGIN ISLANDS","MYANMAR","Western Pacific Ocean","LIBYAN","COTE D'IVOIRE",
               "LIBYAN","SYRIA","NEW CALEDONIA","SWAZILAND","IRAN")

lista[,2]=correcciones

lista=as.data.frame(apply(lista,2,as.character),stringsAsFactors = F)
colnames(lista)=c("country","country fix")
colnames(paisesmal)=c("index","country")
paisesmal=as.data.frame(paisesmal,stringsAsFactors = F)
paisescorregidos=merge(paisesmal,lista)
hoteles$country[as.integer(paisescorregidos[,2])]=as.character(paisescorregidos[,3])
hoteles$country=str_to_upper(hoteles$country)

#Corregimos códigos de países que puedan estar mal

#Primero completamos con los que proporciona Giata
hoteles$country_code[which(is.na(hoteles$country_code) & !is.na(hoteles$gcountry_code))]=hoteles$gcountry_code[which(is.na(hoteles$country_code) & !is.na(hoteles$gcountry_code))]

######CUIDADO, NO SE ESTÁ MODIFICANDO EL CONJUNTO DE HOTELES. HAY QUE DESCOMENTAR UN PAR DE LÍNEAS

mal=NULL
for(k in 1:nrow(countries)){
  codigo=as.character(countries$country_code[k])
  
  ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character")
  colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                       "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                       "population","elevation","dem","timezone","modification.date")
  
  candidatos=as.data.frame(table(hoteles$country[which(hoteles$country_code==codigo & !is.na(hoteles$lat) & !is.na(hoteles$lng))])) #Si falta la latitud o la longitud no tocamos nada por ahroa
  
  if(nrow(candidatos)!=0){
    candidatos=candidatos[-match(str_to_upper(countries$country[countries$country_code==codigo]),candidatos[,1]),] #Nos centramos únicamente en aquellos candidatos cuyo país no coincida con el código
  }
  
  if(nrow(candidatos)!=0 & all(!is.na(candidatos))){
    
    for(j in 1:nrow(candidatos)){

      indices=which(hoteles$country_code==codigo & !is.na(hoteles$lat) & !is.na(hoteles$lng) & hoteles$country==str_to_upper(candidatos[j,1]))
      pb=txtProgressBar(min = 0, max = length(indices), initial = 0, char = "=",
                        width = NA, title, label, style = 3)
      for(i in 1:length(indices)){
        setTxtProgressBar(pb, i)
        cat("\n",i,codigo,hoteles$city[indices[i]],as.character(candidatos[j,1]),"=> ")
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
        
        if(all(!is.na(search.4(i,indices)))){
          codigo_pais=na.omit((unique(search.4(i,indices)[,1])[match(unique(na.omit(busquedas[,1])),unique(search.4(i,indices)[,1]))]))
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
      
    }
  }else{cat("NO HAY NINGÚN CÓDIGO MAL\n")}
}

write.table(mal,file="mal",sep=";",row.names=F)

# mal[2,]
# compara=as.character(countries$country_code[which(str_to_upper(countries$country)==mal$country[2])])
# subindex=nn2(data=cities[which(cities$country_code==compara),c(5,6)],query=rbind(as.numeric(mal[2,c(12,13)])),k = 1,radius = 5,searchtype = "radius")$nn.idx
# codigo_pais=cities[which(cities$country_code==compara)[subindex],]
# pais=str_to_upper(countries$country[countries$country_code==codigo_pais])
