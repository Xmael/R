library(readr)
library(stringr)
library(RANN)

#Crea una lista dividida por países de las ciudades

length(hoteles$country_code[which(is.na(hoteles$province) & !is.na(hoteles$city))])

paises_perdidos=unique(hoteles$country_code[which(is.na(hoteles$province) & !is.na(hoteles$city))])
paises_perdidos=paises_perdidos[-match(c("","AN"),paises_perdidos)] #Quito las Antillas Holandesas por problemas con ISO 3166-1 alpha 1

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

busca_codigos=function(codigo,perdido,admincodes){
  temp=admincodes[admincodes[,1]==perdido,] 
  temp=rbind(temp[as.character(temp[,2])==names(which.max(table(as.character(codigo[,1])))),])
  if(nrow(temp)==1){
    return(temp[,4])
    }else{
      temp=temp[which(temp[,3]==names(which.max(table(codigo[codigo[,2]!="",2])))),4]
    }
  return(temp)
}

admincodes=read.csv(file="admin2Codes.txt",sep="\t",header=F,stringsAsFactors = F)

admincodes=reconstruye(admincodes)


perdidos=NULL
mal=NULL

for(k in 4:length(paises_perdidos)){
  
  indices=which(is.na(hoteles$province) & !is.na(hoteles$city) & hoteles$country_code==paises_perdidos[k])
  
  ciudades=read.csv(file=paste("Countries/",paises_perdidos[k],".txt",sep=""),sep="\t",header=F,stringsAsFactors = F,colClasses = "character")
  colnames(ciudades)=c("geonameid","name","asciiname","alternate.names","latitude","longitude","feature.class",
                       "feature.code","country_code","cc2","admin1.code","admin2.code","admin3.code","admin4.code",
                       "population","elevation","dem","timezone","modification.date")
  
  #Para evitar que aparezcan valores perdidos y el nn2 dé error hacemos lo siguiente
  ciudades[,c(5,6)]=apply(ciudades[,c(5,6)],2,as.numeric)
  valoresperdidos=which(apply(ciudades[,c(5,6)],1,function(ciudades){all(!is.na(ciudades))}))
  ciudades=ciudades[valoresperdidos,]
  
  #Genera candidatos por país
  for(i in 1:length(indices)){
    cat(hoteles$country[indices[i]],paste(i,"/",length(indices),sep=""),hoteles$city[indices[i]],"\n")
    nombre=names(which.max(table(
      gsub(" ","",hoteles$province[which(hoteles$city==hoteles$city[indices[i]] & hoteles$country==hoteles$country[indices[i]])]))))
    if(!is.null(nombre)){ #Si el nombre de la provincia aparece en nuestra base de datos lo tomamos de ahí
      hoteles$province[indices[i]]=nombre
    }else{#En caso contrario lo buscamos en la lista de ciudades y lo colocamos en una lista aparte para procesarlos después
      if(any(is.na(hoteles[indices[i],c(13,14)]))){#Si falta la latitud y la longitud, no se puede hacer nada por ahora y lo colocamos en la lista "mal"
        mal=rbind(indices[i],hoteles[indices[i],c(1,2,3,4,5,6,9,10,11,12,13,14)])
      }else{
        subindex=nn2(data=ciudades[,c(5,6)],query=rbind(as.numeric(hoteles[indices[i],c(13,14)])),k = 5,radius = 10,searchtype = "radius")$nn.idx
        if(all(subindex!=0)){
          codigo=busca_codigos(ciudades[subindex,11:14],paises_perdidos[k],admincodes)
          
          if(length(codigo)!=0)
            perdidos=rbind(perdidos,cbind(indices[i],busca_codigos(ciudades[subindex,11:14],paises_perdidos[k],admincodes)))
          else{#El codigo no se encuentra y se utiliza como provincia el nombre de la ciudad más cercana en la lista de perdidos
            perdidos=rbind(perdidos,cbind(indices[i],ciudades$asciiname[subindex[1]]))
          }
          
        }else{#Si por cualquier motivo no se encuentra en la lista de ciudades lo colocamos también en la lista mal
          mal=rbind(mal,indices[i],hoteles[indices[i],c(1,2,3,4,5,6,9,10,11,12,13,14)])
        }
      }
    }
  }
  
  cat("Han quedado sin procesar",length(which(is.na(hoteles$province) & !is.na(hoteles$city) & hoteles$country_code==paises_perdidos[k])),"\n")
  Sys.sleep(3)
  
}

