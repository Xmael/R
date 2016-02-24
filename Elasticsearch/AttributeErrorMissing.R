library(elastic)
library(RJSONIO)
library(XML)
library(lubridate)

today=Sys.Date()-1
periodo=seq(today, length.out=30, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d")
library(data.table)
load("/home/ismael/Trivago/hoteles(25.1.16).RData")

#load("~/Trivago/hoteles(20.1.16).RData")

parsea=function(mensaje){
  r=xmlTreeParse(mensaje)
  cadena=xmlValue(xmlRoot(r)[[2]])
  cadena=strsplit(cadena,split = "#")
  return(c(length(cadena[[1]])-1,cadena[[1]][length(cadena[[1]])]))
}

errores=NULL
for(j in 1:length(periodo)){
  connect(es_base="http://52.28.187.57")
  cat("#####",periodo[j],"#####\n")
  db2=Search(index = paste("logstash-",periodo[j],sep=""),type = "has",size=1e6,
             q="@message: \"'AttributeErrorMissing info\"")$hits$hits
  
  
  pb=txtProgressBar(min = 0, max = length(db2), initial = 0, char = "=",width = NA, title, label, style = 3)
  errores.1=matrix(NA,nrow=length(db2),ncol=1,byrow = T)
  for(i in 1:length(db2)){
    setTxtProgressBar(pb, i)
    r=xmlTreeParse(db2[[i]]$`_source`$'@fields'$request)
    r=xmlApply(xmlRoot(r)[[2]],xmlValue)[[1]]
    errores.1[i,]=r
  }
  errores=rbind(errores,errores.1)
}


hs=unique(errores)
resumen=sapply(1:nrow(hs),function(i){
  setTxtProgressBar(txtProgressBar(min = 0, max = nrow(hs), initial = 0, char = "=",width = NA, title, label, style = 3), i)
  candidatos=hoteles[which(hoteles$giata==hoteles$giata_id[which(hoteles$prov_id==hs[i] & hoteles$prov=="RS")]),]
  temp=data.frame(rbind(table(candidatos$prov)))
  columnas=data.frame(GT=NA,HB=NA,RS=NA,SH=NA,ST=NA,LC=NA,KY=NA,TD=NA)
  columnas[match(colnames(temp),colnames(columnas))]=temp
  return(c(Prov_id=hs[i],Name=hoteles$name[which(hoteles$prov_id==hs[i] & hoteles$prov=="RS")],
              columnas,Actualizado=any(hs[i]<candidatos$prov_id[candidatos$prov=="RS"])))
})
resumen=t(resumen)


temp=xmlParseDoc("restel-2016-01-28.xml",asText=F)
temp=xmlRoot(temp)
#prueba=xmlToDataFrame(nodes = getNodeSet(temp, "//*/hot_codcobol"), stringsAsFactors = FALSE)
nodos=getNodeSet(temp, "//*/hot_codcobol")
lista=xmlApply(nodos,xmlValue)
lista=matrix(unlist(lista),nrow=length(lista),ncol=1)

# pb=txtProgressBar(min = 0, max = length(nodos), initial = 0, char = "=",width = NA, title, label, style = 3)
# for(i in 1:length(nodos)){
#   setTxtProgressBar(pb,i)
#   lista[i,1]=xmlValue(nodos[[i]])
# }

setdiff(lista,lista[which(hoteles$prov_id[which(hoteles$prov=="RS")] %in% lista)])

quitar=hs[-which(hs %in% lista)]
write.table(quitar,file="quitaRS.csv",row.names = F,col.names = F,sep=",")
