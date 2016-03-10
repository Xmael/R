load("/home/ismael/Trabajo/Actualiza/nuevos.RData")
library(RJSONIO)
library(RCurl)

restel_nuevos=data.frame(matrix(NA,nrow=length(nuevos),ncol=19,byrow = T))
colnames(restel_nuevos)=c("province","city","name","category","country","hotel_checkin","lon","hotel_id","hotel_checkout",
                          "address","lat","cp","provider_tlf","hotelService","roomServices","shortdesc","desc","fotos")

for(i in 69424:length(nuevos)){
  cat(i,"de",length(nuevos),"\n")
#  peticion=paste("curl http://localhost:5000/es/raw/content/RS/",nuevos[i]," --max-time 10",sep="")
#  temp=system(peticion,ignore.stderr = F,intern = T)
  temp=getURI(paste("http://localhost:5000/es/raw/content/RS/",nuevos[i],sep=""),
             .opts = list(timeout = 10))
  retry=1

  while(length(temp)==0){
    cat("Retry",retry,"\n")
    temp=system(peticion,ignore.stderr = T,intern = T)
    retry=retry+1
    Sys.sleep(2)		
  }
  
  if(isValidJSON(temp,asText = T)){
    temp=fromJSON(temp,simplify = F)
    feature=c(1,2,4,5,6,7,8,9,10,12,13,15,16)
    
    if(class(temp$fotos)=="AsIs"){
      fotos=NA
    }else{
      fotos=paste(unlist(temp$fotos),collapse="|")
    }
    
    if(class(temp$hotelServices)=="AsIs"){
      hs=NA
    }else{
      hs=paste(sapply(1:length(temp$hotelServices),function(i) temp$hotelServices[[i]]$service),collapse="|")
    }
    
    if(class(temp$roomServices)=="AsIs"){
      rs=NA
    }else{
      rs=paste(sapply(1:length(temp$roomServices),function(i) temp$roomServices[[i]]$service),collapse="|")
    }
    
    if(class(temp$desc$shortDescription)=="AsIs"){
      sdesc=NA
    }else{
      sdesc=temp$desc$shortDescription
    }
    
    if(class(temp$desc$desc)=="AsIs"){
      desc=NA
    }else{
      desc=temp$desc$desc
    }
    
    final=rbind(unlist(c(temp[feature],hs,rs,sdesc,desc,fotos),use.names = F))
    final[which(final=="")]=NA
#    final=cbind(i,final,deparse.level=0)
    restel_nuevos[i,]=final
  }else{
    restel_nuevos[i,]=c(NA,NA,NA,NA,NA,NA,NA,nuevos[i],NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  }
}
