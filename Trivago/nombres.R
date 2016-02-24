library(parallel)
library(stringdist)
library(stringr)

cat("Cargando hoteles\n")
load("tourico.RData")

codigo="US"
candidatos=hoteles[which(hoteles$country_code==codigo),]

cat("Cargando ciudades\n")
ciudades=read.csv(file=paste("Countries/",codigo,".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character",quote="",na.strings = NULL)
colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                     "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                     "population","elevation","dem","timezone","modification.date")

ciudades_candidatas=unique(candidatos[,12])

nombres=mclapply(1:length(ciudades_candidatas),function(i){
  
  orden=sort.int(stringdist(str_to_upper(iconv(ciudades$name,to="ASCII//TRANSLIT")),str_to_upper(gsub("\\,.*","",gsub(" \\(.*","",ciudades_candidatas[i]))),
                            method="jw",weight=c(1,1,0.5),nthread = 1),decreasing = F,index.return = T)
  cat(i,"de",length(ciudades_candidatas),c(ciudades_candidatas[i],as.matrix(ciudades[orden$ix[which(orden$x<0.5)][1],c(3,18)])),"\n")
  write.table(cbind(ciudades_candidatas[i],as.matrix(ciudades[orden$ix[which(orden$x<0.5)][1],c(3,18)])),file="nombres.csv",sep=";",row.names=F,append = T,col.names = F)
  return(c(ciudades_candidatas[i],as.matrix(ciudades[orden$ix[which(orden$x<0.5)][1],c(3,18)])))
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 7L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)

nombres=matrix(unlist(nombres),nrow=length(nombres),ncol=3,byrow=T)

save(nombres,file="nombres.RData")
