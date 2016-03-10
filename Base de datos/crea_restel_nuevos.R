load("/home/ismael/Trabajo/Actualiza/nuevos.RData")
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
    write(temp,file="xmlrestel.csv",append = T)
  }
}
