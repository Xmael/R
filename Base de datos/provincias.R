library(parallel)
library(stringr)
library(geosphere)

load("restel2.RData")

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
provincias=mclapply(1:nrow(restel),function(i){
  cat(i,"de",nrow(restel),restel$city[i],"\n")
  codigo<-ciudades1000[which(ciudades1000$country_code==restel$country_code[i] & 
                               str_to_upper(iconv(ciudades1000$name,to="ASCII//TRANSLIT"))==str_to_upper(iconv(restel$city[i],to="ASCII//TRANSLIT"))),
                       c("country_code","admin1.code","admin2.code","admin3.code")]
  if(nrow(codigo)!=1){
    cat("No se encuentra",restel$city[i],"\n")
    return(NA)
  }else if(nrow(codigo)==1){
    temp=admincodes1[which(codigo[,1]==admincodes1[,1] & codigo[,2]==admincodes1[,2] & codigo[,3]==admincodes1[,3]),4]
    if(length(temp)==1){
      cat(restel$province[i],"\n")
      return(temp)
    }else if(length(temp)!=1){
      temp=admincodes2[which(codigo[,1]==admincodes2[,1] & codigo[,2]==admincodes2[,2]),4]
      if(length(temp)==1){
        cat(restel$province[i],"\n")
        return(temp)
      }else if(length(temp)!=1){
        cat("No se encuentra",restel$city[i],"\n")
        return(NA)
      }
    }
  }
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 6L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)
save(provincias,file="provincias.RData")