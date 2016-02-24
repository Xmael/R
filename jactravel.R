library(stringr)
library(stringdist)
hoteles=read.csv2(file = "/home/ismael/Trivago/mapped_hotels.csv",sep = ";",header=T,stringsAsFactors = F,na.strings = "NULL")
admincodes=read.csv(file="admin2Codes.txt",sep="\t",header=F,stringsAsFactors = F)
candidatos=jactravel[which(is.na(jactravel$giata_id) & jactravel$prov=="JA"),]
#candidatos=hoteles[which(hoteles$prov=="JA"),]
countries=read.csv("countries.csv", sep=";",head=F,na.strings = "NULL",stringsAsFactors = F)
colnames(countries)=c("country_code","country")

codigo=as.character(countries$country_code[j])

paises=unique(candidatos$country)
paises=paises[-which(str_to_upper(paises) %in% str_to_upper(countries$country))]
corrige=c("United Kingdom","United Kingdom","United Kingdom","United Kingdom","Jersey",
          "United States","United Kingdom","","United States","Thailand","Thailand","Bosnia and Herzegovina",
          "Ireland")
paises=cbind(paises=paises,correccion=corrige)

for(i in 1:nrow(paises)){
  candidatos$country[which(candidatos$country==paises[i,1])]=paises[i,2]
}
candidatos$country=str_to_upper(candidatos$country)
candidatos$country_code=countries$country_code[match(candidatos$country,str_to_upper(countries$country))]

jactravel=candidatos
save(jactravel,file="jactravel.RData")

resumen=data.frame(matrix(NA,nrow = nrow(candidatos),ncol=6))
colnames(resumen)=c("Candidato","name","giata_id","prov","lat","lng")

pb=txtProgressBar(min = 0, max = nrow(resumen), initial = 0, char = "=", width = NA, title, label, style = 3)
for(i in 1:nrow(resumen)){
  setTxtProgressBar(pb, i)
  subcandidatos=hoteles[which(hoteles$country_code==candidatos$country_code[i]),]
  
  resultado.1=sort.int(stringdist(str_to_upper(gsub(" \\(.*| - NON REFUNDABLE ROOM","",candidatos$name[i])),str_to_upper(subcandidatos$name),method="jw",weight = c(0.1,1,1)),index.return = T)
  resumen[i,]=c(Candidatos=candidatos$name[i],subcandidatos[resultado.1$ix[which(resultado.1$x<=0.35)][1],c(6,5,2,13,14)])
  score=stringdist(str_to_upper(gsub(" \\(.*| - NON REFUNDABLE ROOM","",resumen[i,1])),str_to_upper(resumen[i,2]),method="cosine")
  if(score>0.1 & !is.na(score)){
    resumen[i,c(2:6)]=rep(NA,5)
  }
}
