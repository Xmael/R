library(RANN)
library(geosphere)
library(stringdist)
library(stringr)

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

admincodes.1=read.csv(file="admin1CodesASCII.txt",sep="\t",header=F,stringsAsFactors = F,na.strings = "NULL")
admincodes.2=read.csv(file="admin2Codes.txt",sep="\t",header=F,stringsAsFactors = F,na.strings="NULL")


admincodes.1=reconstruye.1(admincodes.1)
admincodes.2=reconstruye.2(admincodes.2)

timezones=read.csv(file="timeZones.txt",sep="\t",header=T)

cities15k=read.csv("cities15000.txt",sep="\t",quote="",header=F,stringsAsFactors = F,colClasses = "character",na.strings="NULL")
colnames(cities15k)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                      "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                      "population","elevation","dem","timezone","modification.date")

candidatos=hoteles[which(hoteles$country=="NETHERLANDS ANTILLES"),]# & !is.na(hoteles$lat)),]

#candidatos$city=str_to_upper(giata$cityName[match(candidatos$giata_id,giata$id)])
#candidatos$country_code=""

pb=txtProgressBar(min = 0, max = nrow(candidatos), initial = 0, char = "=",width = NA, title, label, style = 3)
prueba=t(sapply(1:nrow(candidatos),function(i){
  setTxtProgressBar(pb, i)
  cities15k[order(distGeo(apply(rbind(candidatos[i,c(14,13)]),2,as.numeric),
        apply(rbind(cities15k[,c(6,5)]),2,as.numeric),a=6378137, f=1/298.257223563)/1000,decreasing = F)[1],c(3,5,6,9,11,12,18)]}))
prueba=matrix(unlist(prueba),nrow = nrow(prueba),ncol=ncol(prueba),dimnames = list(1:nrow(prueba),colnames(prueba)))

for(i in 1:nrow(candidatos)){
  cat(i,rbind(prueba[i,]),"\n")
  candidatos$country_code[i]=prueba[i,4]
  candidatos$country[i]=str_to_upper(countries$country[match(prueba[i,4],countries$country_code)])
#  candidatos$province[i]=str_to_upper(admincodes.1[which(admincodes.1[,1]==prueba[i,4] & admincodes.1[,2]==prueba[i,5]),3])
  candidatos$timezone[i]=timezones[match(prueba[i,7],timezones[,2]),3]
  candidatos$dst[i]=timezones[match(prueba[i,7],timezones[,2]),4]
}

pb=txtProgressBar(min = 0, max = nrow(candidatos), initial = 0, char = "=",width = NA, title, label, style = 3)
for(i in 1:nrow(candidatos)){
  setTxtProgressBar(pb, i)
  hoteles[which(hoteles$id==candidatos$id[i]),]=candidatos[i,]
}


#candidatos=hoteles[which(hoteles$country=="NULL" & hoteles$country_code=="" & !is.na(hoteles$lat)),]
table(hoteles$country[(which(is.na(hoteles$timezone)))])

candidatos=unique(tourico$province)
