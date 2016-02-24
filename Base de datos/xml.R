library(XML)

idiomas=c("ING","ALE","DAN","FRA","HOL","ITA","POR")
grupos=read.csv("/media/HD/Hotelbeds Servicios/FacilityGroupsDescriptions.csv",sep="|",header=F,stringsAsFactors = F)
colnames(grupos)=c("codigo","idioma","servicio")

archivos=list.files("/media/HD/Hotelbeds Servicios/ESP/")
hotelbeds=matrix(NA,nrow=length(archivos)*8,ncol=5)


for(k in 1162:length(archivos)){
  cat(k,"de",length(archivos),"\n")
  xml=xmlToList(paste("/media/HD/Hotelbeds Servicios/ESP/",archivos[k],sep=""))
  #Servicios
  codigos_hoteles=sapply(1:length(xml[[5]]),function(i){
    xml[[5]][[i]]$code
  })
  
  grupos_hoteles=sapply(1:length(xml[[5]]),function(i){
    xml[[5]][[i]]$group
  })
  
  descripciones=sapply(1:length(xml[[5]]),function(i){
    xml[[5]][[i]]$description
  })
  
  resumen=t(sapply(1:length(xml[[5]]),function(i){
    c(descripciones[i],grupos$servicio[which(grupos$codigo==grupos_hoteles[i] & grupos$idioma=="CAS")])
  }))
  
  #Elimina campos con SI o NO
  indices.1=sapply(1:nrow(resumen),function(i){
    if("NO" %in% strsplit(resumen[i,1],split=" ")[[1]]){
      descripcion=resumen[i,1]
      resumen=resumen[-i,]
      return(rbind(i,which(resumen[,1]==gsub("NO ","",descripcion))))
    }
    
    if("SI" %in% strsplit(resumen[i,1],split=" ")[[1]]){
      return(i) #Quitamos el que pone SI
    }
  })
  
  resumen[,1]=gsub(" – ","",gsub("  – ","",resumen[,1]))
  
  if(!is.null(unlist(indices.1))){
    resumen=resumen[-unlist(indices.1),]
  }
  
  resumen=rbind(resumen)
  
  elimina=c("Tipo de hotel","Ubicación","Distancia (en metros) ","Puntos de interés")
  indices.2=which(resumen[,2] %in% elimina)
  
  if(length(indices.2)!=0){
    resumen=paste(resumen[-indices.2,1],collapse="|")
  }else{
    resumen=paste(resumen,collapse="|")
  }
  
  if(resumen==""){
    resumen=NA
  }
  
  fotos=sapply(1:length(xml[[6]]),function(i){
    xml[[6]][[i]]$path
  })
  
  if(is.null(unlist(fotos))){
    fotos=NA
  }else{
    fotos=paste(fotos,collapse="|")
  }
  
  descripcion=xml[[3]]$descriptionHotel$description
  descripcion=ifelse(is.null(descripcion),NA,descripcion)
  
  hotelbeds[8*(k-1)+1,]=c(xml[[1]]$hotelId,"ESP",resumen,descripcion,fotos)
  
  
  
  for(j in 1:length(idiomas)){
    xml=xmlToList(paste("/media/HD/Hotelbeds Servicios/",idiomas[j],"/",archivos[k],sep=""))
    
    descripciones=sapply(1:length(xml[[5]]),function(i){
      xml[[5]][[i]]$description
    })
    
    descripciones=gsub(" – ","",gsub("  – ","",descripciones))
    if(!is.null(unlist(indices.1))){
      descripciones=descripciones[-unlist(indices.1)]
    }
    if(length(indices.2)!=0){
      descripciones=descripciones[-indices.2]
    }
    
    if(length(descripciones)==0){
      descripciones=NA
    }else{
      descripciones=paste(descripciones,collapse="|")
    }
    
    descripcion_hotel=xml[[3]]$descriptionHotel$description
    descripcion_hotel=ifelse(is.null(descripcion_hotel),NA,descripcion_hotel)
    
    hotelbeds[8*(k-1)+(j+1),]=c(xml[[1]]$hotelId,idiomas[j],descripciones,descripcion_hotel,fotos)
  }
}

colnames(hotelbeds)=c("prov_id","language","services","description","pictures")
hotelbeds=data.frame(hotelbeds,stringsAsFactors = F)
hotelbeds$language=as.factor(hotelbeds$language)
levels(hotelbeds$language)=c("de","da","es","fr","nl","en","it","pt")

save(hotelbeds,file="hotelbeds_desc_fotos_serv.RData")
write.table(hotelbeds,file="HB_descripciones_y_servicios.csv",sep="\t",row.names = F,col.names = T,na = "NULL",quote = F)


gsub("\\\"","")