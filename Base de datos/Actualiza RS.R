library(XML)
library(parallel)
library(RJSONIO)
library(RCurl)
load("~/Trivago/Datos/hoteles(25.1.16).RData")
doc=xmlParse("/home/ismael/Trabajo/Errores/restel-2016-01-28.xml", useInternalNodes = TRUE)
doc=xmlRoot(doc)
doc=xmlRoot(doc)

codigos_restel=mclapply(1:xmlSize(doc[[4]][[1]]),function(i){ 
  cat(i,"de",xmlSize(doc[[4]][[1]]),"\n")
  c1=xmlValue(doc[[4]][[1]][[i]][[3]])
  c2=xmlValue(doc[[4]][[1]][[i]][[2]])
  c3=xmlValue(doc[[4]][[1]][[i]][[1]])
  c4=xmlValue(doc[[4]][[1]][[i]][[4]])
  return(c(c1,c2,c3,c4))
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 7L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)

save(codigos_restel,file="codigos_restel.RData")

load(file="codigos_restel.RData")
load("~/Trivago/Datos/hoteles(25.1.16).RData")
codigos_restel=data.frame(matrix(unlist(codigos_restel),nrow=length(codigos_restel),ncol=4,byrow = T),stringsAsFactors = F)
colnames(codigos_restel)=c("npi","gprovince_id","province_code","prov_id")
#Metemos primero los códigos de provincia de RESTEL. Las provincias las obtenemos como siempre.
restel=hoteles[hoteles$prov=="RS",] #Seleccionamos de la base de datos los hoteles de restel
incluidos=restel$prov_id[which(restel$prov_id %in% codigos_restel$prov_id)] #obtengo los códigos de restel que aun son válidos
nuevos=codigos_restel$prov_id[-which(codigos_restel$prov_id %in% restel$prov_id)]

#  save(nuevos,file="nuevos.RData")

indices_nuevos=c(1:nrow(codigos_restel))[-which(codigos_restel$prov_id %in% restel$prov_id)]

restel=restel[which(restel$prov_id %in% incluidos),] #Nos quedamos solamente con los que tienen código válido

restel_nuevos=data.frame(matrix(NA,nrow=length(nuevos),ncol=18,byrow = T))
colnames(restel_nuevos)=c("province","city","name","category","country","hotel_checkin","lon","hotel_id","hotel_checkout",
                          "address","lat","cp","provider_tlf","hotelService","roomServices","shortdesc","desc","fotos")

load("nuevos.RData")
library(RJSONIO)
library(RCurl)
library(curl)
for(i in 1:length(nuevos)){
  cat(i,"de",length(nuevos),"\n")
  #   h<-new_handle()
  #   handle_setopt(handle=h,.list=list(timeout=10))
  #   temp=(curl_fetch_memory(paste("http://localhost:5000/es/raw/content/RS/",nuevos[i],sep=""),handle = h)$content)
  temp=getURI(paste("http://localhost:5000/es/raw/content/RS/",nuevos[i],sep=""),.opts = list(timeout = 60))
  retry=1
  
  while(length(temp)==0){
    cat("Retry",retry,"\n")
    temp=system(peticion,ignore.stderr = T,intern = T)
    retry=retry+1
    Sys.sleep(2)		
  }
  
  if(isValidJSON(temp,asText = T)){
    write(temp,file="xmlrestel.csv",append=T)
  }
}
restel_nuevos=to(file="xmlrestel.csv",header=F,sep = ";",stringsAsFactors = F,blank.lines.skip = T)
restel_nuevos=fromJSON("xmlrestel.csv")

