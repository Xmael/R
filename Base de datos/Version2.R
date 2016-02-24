library(RCurl)
library(RJSONIO)
library(rjson)
library(XML)
library(parallel)

load("/home/ismael/Trivago/hoteles(25.1.16).RData")

#################
 indices.GT=hoteles$prov_id[which(hoteles$prov=="SH")]

############
resumen.GT=matrix(NA,nrow=length(indices.GT),ncol=6)
colnames(resumen.GT)=c("Prov","Prov_id","UniqID","Services","desc","fotos")
 
for(i in 1:length(indices.GT)){
  # peticion=paste("curl http://localhost:5000/es/raw/content/RS/",indices.GT[i], " --retry 50",sep="")
   temp=getURL(paste("http://localhost:5000/es/raw/content/SH/",indices.GT[i],sep=""),
               .opts = list(timeout = 60, maxredirs = 10))
#  temp=system(peticion,ignore.stderr = T,intern = T)
  if(isValidJSON(temp,asText = T)){
#    write(temp,file="descripciones",append=T)
    temp=fromJSON(temp,simplify=T)
    if(class(temp$services)!="AsIs" & class(temp$services)!="NULL"){
      HServ=1
    }else if(class(temp$services)=="AsIs" | class(temp$services)=="NULL"){
      HServ=0
    }
#     if(class(temp$roomServices)!="AsIs" & class(temp$roomServices)!="NULL"){
#       RServ=1
#     }else if(class(temp$roomServices)=="AsIs" | class(temp$roomServices)=="NULL"){
#       RServ=0
#     }
    if(class(temp$desc)!="AsIs" & class(temp$desc)!="NULL"){
      desc=1
    }else if(class(temp$desc)=="AsIs" | class(temp$desc)=="NULL"){
      desc=0
    }
    if(class(temp$fotos)!="AsIs" & class(temp$fotos)!="NULL"){
      fotos=1
    }else if(class(temp$fotos)=="AsIs" | class(temp$fotos)=="NULL"){
      fotos=0
    }
#     if(class(temp$service_html)!="AsIs" & class(temp$service_html)!="NULL"){
#       Servhtml=1
#     }else if(class(temp$service_html)=="AsIs" | class(temp$service_html)=="NULL"){
#       Servhtml=0
#     }
    Prov=hoteles$prov[which(hoteles$prov_id==indices.GT[i] & hoteles$prov=="SH")]
    Prov_id=hoteles$prov_id[which(hoteles$prov_id==indices.GT[i] & hoteles$prov=="SH")]
    UniqID=hoteles$uniqid[which(hoteles$prov_id==indices.GT[i] & hoteles$prov=="SH")]
    resumen.GT[i,]=c(Prov,Prov_id,UniqID,HServ,desc,fotos)
  }else{
  Prov=hoteles$prov[which(hoteles$prov_id==indices.GT[i] & hoteles$prov=="SH")]
  Prov_id=hoteles$prov_id[which(hoteles$prov_id==indices.GT[i] & hoteles$prov=="SH")]
  UniqID=hoteles$uniqid[which(hoteles$prov_id==indices.GT[i] & hoteles$prov=="SH")]
  resumen.GT[i,]=c(Prov,Prov_id,UniqID,NA,NA,NA)
  
  }
  cat(resumen.GT[i,])
  cat(" |  ",i,"de",length(indices.GT),"\n")
}

save(resumen.RS,file="resumen.RS.RData")
save(resumen.RS2,file="resumen.RS2.RData")


which(is.na(resumen.GT[,4] & is.na(resumen.GT[,5]) & is.na(resumen.GT[,6])))
