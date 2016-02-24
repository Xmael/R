library(stringdist)
library(stringr)

codigo="ES"
candidatos=hoteles[which(hoteles$country_code==codigo),]

ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="",na.strings = NULL)
colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")

ciudades_candidatas=unique(candidatos[,12])

nombres=sapply(1:length(ciudades_candidatas),function(i){
  
  orden=sort.int(stringdist(str_to_upper(iconv(ciudades$name,to="ASCII//TRANSLIT")),str_to_upper(gsub("\\,.*","",gsub(" \\(.*","",ciudades_candidatas[i]))),
                      method="jw",weight=c(1,1,0.5)),decreasing = F,index.return = T)
  cat(i,"de",length(ciudades_candidatas),c(ciudades_candidatas[i],as.matrix(ciudades[orden$ix[which(orden$x<0.5)][1],c(3,18)])),"\n")
  return(c(ciudades_candidatas[i],as.matrix(ciudades[orden$ix[which(orden$x<0.5)][1],c(3,18)])))
})

nombres=t(nombres)

#load("nombres.RData")

# nombres=read.csv(file="nombres.csv",sep=";",header=F,stringsAsFactors = F)
# nombres_temp=rbind(nombres.bak,nombres[match(setdiff(nombres[,1],nombres.bak[,1]),nombres[,1]),])
# nombres=nombres_temp


for(i in 1:nrow(nombres)){
  
  if(str_to_upper(gsub("\\,.*","",gsub(" \\(.*","",nombres[i,1])))!=str_to_upper(nombres[i,2])){
    cat(i,"de",nrow(nombres),as.matrix(nombres[i,]),"\n")
    cat(unique(candidatos[which(str_to_upper(iconv(candidatos$city,to="ASCII//TRANSLIT"))==nombres[i,1]),c(11)]))
    lee=readline(prompt = "Correcto/Nombre nuevo ")
    if(lee!=""){
      nombres[i,2]=str_to_upper(lee)
      lee2=readline(prompt = "Uso horario correcto?/Nuevo: ")
      if(lee2!="")
        nombres[i,3]=lee2
    }
  }
}

####Corrige los nombres
pb=txtProgressBar(min = 0, max = nrow(nombres), initial = 0, char = "=",
                  width = NA, title, label, style = 3)
for(i in 1:nrow(nombres)){
  setTxtProgressBar(pb, i)
  hoteles$city[which(hoteles$country_code==codigo & hoteles$city==nombres[i,1])]=str_to_upper(nombres[i,2])
}

#Añade las zonas horarias
pb=txtProgressBar(min = 0, max = nrow(nombres), initial = 0, char = "=",
                  width = NA, title, label, style = 3)
for(i in 1:nrow(nombres)){
  setTxtProgressBar(pb, i)
  hoteles$timezone[which(hoteles$country_code==codigo & hoteles$city==str_to_upper(nombres[i,2]))]=timezones[which(timezones[,2]==nombres[i,3]),3]
  hoteles$dst[which(hoteles$country_code==codigo & hoteles$city==str_to_upper(nombres[i,2]))]=timezones[which(timezones[,2]==nombres[i,3]),4]
}




###########################
paises=unique(hoteles$city[which(is.na(hoteles$country_code) & hoteles$country=="VIRGIN ISLANDS")])
for(i in 4:length(paises)){
  indice=match(str_to_upper(paises[i]),str_to_upper(countries$country))
  if(!is.na(temp)){
    pais=str_to_upper(countries$country[indice])
    codigo=countries$country_code[indice]
    hoteles$country_code[which(hoteles$country=="WESTERN PACIFIC OCEAN" & hoteles$city==paises[i])]=codigo
    hoteles$country[which(hoteles$country=="WESTERN PACIFIC OCEAN" & hoteles$city==paises[i])]=pais
  }
}


provincias_canarias=c("FUERTEVENTURA","LANZAROTE","TENERIFE","LA GOMERA","EL HIERRO","GRAN CANARIA","LAS PALMAS")

table(hoteles$country[(which(is.na(hoteles$Time_zone)))])
hoteles$country_code[which(hoteles$country=="THAILAND")]
filter(hoteles,country=="MEXICO")


# hoteles$country[which(hoteles$country=="ALGERIA")]=str_to_upper("UKRAINE")
hoteles$country_code[which(hoteles$country=="THAILAND")]="TH"

hoteles$timezone[which(hoteles$country=="SPAIN")]
hoteles$dst[which(hoteles$country=="SPAIN")]

hoteles$timezone[which(hoteles$country==str_to_upper("SPAIN")  )]=7
hoteles$dst[which(hoteles$country==str_to_upper("SPAIN")  )]=7
