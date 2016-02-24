library(stringr)

hoteles=read.csv("hoteles_full.csv",sep=";",header=T,na.string="NULL",stringsAsFactors = F)
countries=read.csv("countries.csv", sep=";",head=F,na.strings = "NULL",stringsAsFactors = F)
colnames(countries)=c("country_code","country")

paisesmal=NULL
for(i in 1:nrow(hoteles)){
  if(!is.na(hoteles$country[i]) & all(str_to_upper(hoteles$country[i])!=str_to_upper(countries$country))){
    cat(i,"\n")
    paisesmal=rbind(paisesmal,cbind(i,hoteles$country[i]))
  }
}

lista=as.data.frame(unique(paisesmal[,2]))
lista=cbind(lista,NA)
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

#Obtenemos los índices de los países que tienen el campo country perdido
indicesperdidos=which(is.na(hoteles$country))            
codigos=as.data.frame(unique(hoteles$country_code[indicesperdidos]),stringsAsFactors = F)
colnames(codigos)="country_code"

diferencia=setdiff(as.character(codigos[,1]),as.character(countries$country_code))
diferencia[which(is.na(diferencia))]="NA"
correcciones=c("TR","BG","CZ","DO","PT","SE","NL","RO","DK","UA","SA","CY","KW","IE","LV","MO","AN","KZ","","NA","GB","CY","AN","AN","AN")
for(i in 1:nrow(codigos)){
  if(codigos[i,] %in% diferencia){
    codigos[i,]=correcciones[codigos[i,]==diferencia]
  }
}


indices_codigos_mal=NULL
for(i in indicesperdidos){
  if(as.character(hoteles$country_code[i]) %in% diferencia){
    indices_codigos_mal=rbind(indices_codigos_mal,cbind(i,hoteles$country_code[i]))
  }
}

indices_codigos_mal=data.frame(index=as.integer(indices_codigos_mal[,1]),countrycode=as.character(indices_codigos_mal[,2]))
indices_codigos_mal=merge(indices_codigos_mal,data.frame(countrycode=diferencia,fix=correcciones))

codigos=merge(codigos,countries)

hoteles$country_code[indices_codigos_mal$index]=as.character(indices_codigos_mal$fix)

#Completa codigo de pais o pais a partir del otro
pb=txtProgressBar(min = 0, max = nrow(hoteles), initial = 0, char = "=",
                  width = NA, title, label, style = 3)
for(i in 1:nrow(hoteles)){
  setTxtProgressBar(pb, i)
  if(is.na(hoteles$country[i]) & !is.na(hoteles$country_code[i])){
    hoteles$country[i]=str_to_upper(countries$country[match(hoteles$country_code[i],as.character(countries$country_code))])
    hoteles$gcountry[i]=hoteles$country[i]
  }
  if(!is.na(hoteles$country[i]) & is.na(hoteles$country_code[i])){
    hoteles$country_code[i]=str_to_upper(countries$country_code[match(hoteles$country[i],str_to_upper(as.character(countries$country)))])
    hoteles$gcountry_code[i]=hoteles$country_code[i]
  }
}

for(i in 1:nrow(hoteles)){
  cat(i,"\n")
  if(hoteles$country[i]=="NA" & hoteles$country_code[i]!="NA" & !is.na(hoteles$country[i]=="NA" & hoteles$country_code[i]!="NA")){
    hoteles$country[i]=str_to_upper(countries$country[match(hoteles$country_code[i],as.character(countries$country_code))])
    hoteles$gcountry[i]=hoteles$country[i]
  }
  if(hoteles$country[i]!="NA" & hoteles$country_code[i]=="NA" & !is.na(hoteles$country[i]!="NA" & hoteles$country_code[i]=="NA")){
    hoteles$country_code[i]=str_to_upper(countries$country_code[match(hoteles$country[i],str_to_upper(as.character(countries$country)))])
    hoteles$gcountry_code[i]=hoteles$country_code[i]
  }
}

#Convertimos todos los nombres de hoteles a mayúsculas
hoteles$country=str_to_upper(hoteles$country)
#Corrige códigos de paises para que coincidan con el país
for(i in i:nrow(hoteles)){
  cat(i,"\n")
  if(!is.na(hoteles[i,10]) & hoteles[i,10]!="VIRGIN ISLANDS" & hoteles[i,10]!="NA" & hoteles[i,10]!="SOUTH PACIFIC" &
     hoteles[i,10]!="WESTERN PACIFIC OCEAN"){
    indice=which(str_to_upper(countries$country)==hoteles[i,10])
    pais=str_to_upper(as.character(countries$country[indice]))
    codigo=as.character(hoteles[i,9])
    if(codigo!=as.character(countries$country_code[indice]) | is.na(codigo)){
      hoteles[i,9]=as.character(countries$country_code[indice])
    }
  }
}

save(hoteles,file="hoteles.RData")

hoteles$country[is.na(hoteles$country)]="NA"
c_hoteles=unique(hoteles$city)

#Corrige países que no tienen ni código de país ni nombre de país a partir de las coordenadas espaciales

indices=which(hoteles$country=="NA")
for(i in indices){
  coordenadas=hoteles[i,c(13,14)]
  codigos=cities[nn2(query=coordenadas,data=cities[,7:8],k = 1,searchtype = "radius",radius=10)$nn.idx,c(1,2)]
  hoteles[i,9]=(as.character(levels(codigos[,1]))[codigos[,1]])
  hoteles[i,10]=str_to_upper(as.character(levels(codigos[,2]))[codigos[,2]])
}

indices=which(is.na(hoteles$country))
for(i in indices){
  coordenadas=hoteles[i,c(13,14)]
  codigos=cities[nn2(query=coordenadas,data=cities[,7:8],k = 1,searchtype = "radius",radius=10)$nn.idx,c(1,2)]
  hoteles[i,9]=(as.character(levels(codigos[,1]))[codigos[,1]])
  hoteles[i,10]=str_to_upper(as.character(levels(codigos[,2]))[codigos[,2]])
}


#########################
########PENDIENTE########
#########################

#Corrige la asociación ciudad-país mediante búsqueda de coordenadas
hoteles$country[is.na(hoteles$country)]="NA"
for(i in 1:nrow(countries)){
  cat(i,str_to_upper(countries$country[i]),"\n")
  candidatos=which(hoteles$country==str_to_upper(countries$country[i]))
  lista=na.omit(unique(hoteles$city[candidatos]))
  coordenadas=cities[str_to_upper(cities$country)==str_to_upper(as.character(countries$country[i])),c(2,4,7,8)] #Obtengo las coordenadas de todas las ciudades de ese país
  if(length(candidatos)!=0){
    for(j in 1:length(lista)){
      paises=as.data.frame(table(hoteles$country[hoteles$city==lista[j]]))
      if(nrow(paises)!=1){
        coord_hotel=hoteles[lista[]]
        
        
        
        indices=nn2(coordenadas[,2:3],query=hoteles[which(str_to_upper(hoteles$city)==str_to_upper(candidatos[j])),c(13,14)],k = 3,searchtype = "radius",radius=1,eps=0.1)
        cambia=str_to_upper(names(which.max(table(as.character(coordenadas[indices$nn.idx,1])))))
        hoteles$country[which(hoteles$city==candidatos[j])]=str_to_upper(as.character(countries$country[i]))
        hoteles$country_code[which(hoteles$city==candidatos[j])]=as.character(countries$country_code[i])
      }
      else if(nrow(paises)=1){
        count_vigila=count_vigila+1
        vigila[[count_vigila]]=which(hoteles$city==candidatos[j])
      }
    }
  }
}


