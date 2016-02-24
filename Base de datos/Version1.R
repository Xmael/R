library(RCurl)
library(RJSONIO)
library(XML)
library(parallel)

load("hoteles(25.1.16).RData")

resumen=matrix(NA,nrow=400,nco=8)
colnames(resumen)=c("Prov","Prov_id","UniqID","Serv-html","RServ","HServ","desc","fotos")

lapply(1:400,function(i){
  cat(i,"\n")
  temp=getURL(paste("http://localhost:5000/es/raw/content/",hoteles$prov[i],"/",hoteles$prov_id[i],sep=""))
  if(isValidJSON(temp,asText = T)){
    temp=fromJSON(temp,simplify=T)
    if(class(temp$hotelServices)!="AsIs" & class(temp$hotelServices)!="NULL"){
      resumen[i,"HServ"]=1
    }
    if(class(temp$roomServices)!="AsIs" & class(temp$roomServices)!="NULL"){
      resumen[i,"RServ"]=1
    }
    if(class(temp$desc)!="AsIs" & class(temp$desc)!="NULL"){
      resumen[i,"desc"]=1
    }
    if(class(temp$fotos)!="AsIs" & class(temp$fotos)!="NULL"){
      resumen[i,"fotos"]=1
    }
    if(class(temp$service_html)!="AsIs" & class(temp$service_html)!="NULL"){
      resumen[i,"Serv-html"]=1
    }
    resumen[i,"Prov"]=hoteles$prov[i]
    resumen[i,"Prov_id"]=hoteles$prov_id[i]
    resumen[i,"UniqID"]=hoteles$uniqid[i]
    
  }
  
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 1L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)
  