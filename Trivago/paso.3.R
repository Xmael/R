#Obtenemos los índices de los países que tienen el campo country perdido
indicesperdidos=which(is.na(hoteles$country_code))            
paises=as.data.frame(unique(hoteles$country[indicesperdidos]),stringsAsFactors = F)
colnames(paises)="country"

diferencia=setdiff(as.character(codigos[,1]),as.character(countries$country_code))
diferencia[which(is.na(diferencia))]="NA"

#Repasamos a mano cada uno de los códigos
j=30
diferencia[j]

filter(hoteles,country_code==diferencia[j])[1:10,]

codigo=""

ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character")
colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")

#correcciones=c("TR","BG","CZ","DO","PT","SE","NL","RO","DK","UA","SA","CY","KW","IE","LV","MO","AN","KZ","","NA","GB","CY","AN","AN","AN")

nombres=str_to_upper(names(table(filter(hoteles,country_code==diferencia[j])$city)))
incluye=intersect(nombres,nombres[nombres %in% str_to_upper(unlist(strsplit(ciudades$alternate.names,split=",")))])
incluye=unique(c(incluye,intersect(nombres,nombres[nombres %in% str_to_upper(ciudades$name)])))
excluye=setdiff(nombres,incluye)

#Corrige nombres
hoteles$city[which(hoteles$city=="Madinah, NO USAR")]=str_to_upper("Madinah")

#Obtenemos de nuevo los nombres después de la corrección
nombres=names(table(filter(hoteles,country_code==diferencia[j])$city))
incluye=intersect(nombres,nombres[nombres %in% str_to_upper(unlist(strsplit(ciudades$alternate.names,split=",")))])
incluye=unique(c(incluye,intersect(nombres,nombres[nombres %in% str_to_upper(ciudades$name)])))
excluye=setdiff(nombres,incluye)

hoteles$country_code[which(hoteles$country_code==diferencia[j] & hoteles$city %in% incluye)]=codigo
hoteles$country[which(hoteles$country_code==diferencia[j] & hoteles$city %in% incluye)]=countries$country[which(countries$country_code==codigo)]

#k es el numero de elementos de excluye

#Si todos los excluye pertenecen al mismo sitio
fix=codigo
hoteles$country_code[which(hoteles$country_code==diferencia[j] & hoteles$city %in% excluye)]=fix
hoteles$country[which(hoteles$country_code==diferencia[j] & hoteles$city %in% excluye)]=countries$country[which(countries$country_code==fix)]

#Si no es así
k=6
hoteles[which(hoteles$country_code==diferencia[j] & hoteles$city %in% excluye[k]),][1:20,]

fix="DO"
hoteles$country_code[which(hoteles$country_code==diferencia[j] & hoteles$city %in% excluye[k])]=fix
hoteles$country[which(hoteles$country_code==diferencia[j] & hoteles$city %in% excluye[k])]=countries$country[which(countries$country_code==fix)]
