library(geosphere)
library(parallel)
load(file="restel3.RData")
cp=read.csv2(file="codigos_postales.txt",header=F,sep = "\t",stringsAsFactors = F)
colnames(cp)=c("country_code","postal_code","admin_name1","admin_code1","admin_name2","admin_code2","admin_name3",
               "admin_code3","lat","lng","accuracy")

codigos=mclapply(1:nrow(restel),function(i){
  cat(i,"de",nrow(restel),restel$country_code[i],"  ")
  if(!is.na(restel$lat[i]) & restel$lng[i]>-360 & restel$lat[i]>=-90){
    coordenadas=rbind(apply(restel[i,c(14,13)],2,as.numeric))
    subcp=cp[which(cp[,1]==restel$country_code[i]),]
    if(nrow(subcp)==0){
      cat(NA,"\n")
      return(c(restel$country_code[i],NA))
    }else{
      distancia=distGeo(rbind(apply(subcp[,c(11,10)],2,as.numeric)),coordenadas,a=6378137, f=1/298.257223563)/1000
      if((distancia[order(distancia,decreasing=F)][1]/1000)<5){
        cat(subcp$postal_code[order(distancia,decreasing=F)[1]],"\n")
        return(c(restel$country_code[i],subcp$postal_code[order(distancia,decreasing=F)[1]]))
      }else{
        cat(NA,"\n")
        return(c(restel$country_code[i],NA))
      }
    }
    
  }else{
    cat(NA)
    return(c(restel$country_code[i],NA))
  }
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 7L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)

codigos=matrix(unlist(codigos),nrow=length(codigos),ncol=2,byrow = T)

save(codigos,file="codigos.RData")