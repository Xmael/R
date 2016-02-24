library(parallel)

snn.dbscan=function(data,i,threshold){
  # cat(i,"\n")
  snnmatrix=NULL
  knni=data[i,]
  snnmatrix=sapply(1:nrow(data),function(j){
    if(i==j){
      snnmatrix=c(snnmatrix,NA)
    }else{
      knnj=data[j,]
      if(i %in% knnj & j %in% knni){
        snnmatrix=c(snnmatrix,ifelse(length(intersect(knni,knnj))>threshold,length(intersect(knni,knnj)),0))
      }else{snnmatrix=c(snnmatrix,0)}
    }
  })
  return(snnmatrix)
}

sparsesnn.dbscan=function(dataset,k,threshold){
#  cat("Calculando distancias\n")
  distances=as.matrix(dist(dataset))
#  cat("Ordenando distancias\n")
  orderknn=t(apply(distances,1,order))
  orderknn=orderknn[,1:k]
#  cat("Calculando matriz de similitud")
  snnmatrix=mclapply(1:nrow(dataset),snn.dbscan,data=orderknn,threshold,
                     mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = getOption("mc.cores", 1L),
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE)
  snnmatrix=matrix(unlist(snnmatrix),nrow=nrow(orderknn),ncol=nrow(orderknn))
  rownames(snnmatrix)=rownames(dataset)
  colnames(snnmatrix)=rownames(dataset)
  return(snnmatrix) #Me devuelve una matriz de similitud dispersa (con muchos ceros)
}

DBSCAN<-function(dataset,snnmatrix,eps,minpts){
  setofobjects=as.matrix(cbind(names=as.integer(rownames(dataset)),dataset,processed=0,cluster=1))
  print(head(setofobjects))
  idC=1
  for (i in 1:nrow(setofobjects)){ #sustituimos número de vecinos de p por la densidad del propio punto
    if(setofobjects[i,"processed"]==0){
      object=setofobjects[i,]
      setofobjects[i,"processed"]=1
      dist.ord=snnmatrix[-i,i]
      neighbors=dist.ord>=eps
      neighbors=as.integer(names(dist.ord[neighbors]))
      if(length(neighbors)<minpts){ #Los que tengan una densidad menor que minpts los etiquetamos como ruido
        setofobjects[i,"cluster"]=1
      }else{
        idC=idC+1
        setofobjects=expandcluster(setofobjects,i,neighbors,snnmatrix,idC,eps,minpts)
      }
    }
  }
  return(setofobjects)
}

expandcluster=function(setofobjects,i,neighbors,snnmatrix,idC,eps,minpts){
  setofobjects[i,"cluster"]=idC
  while (length(neighbors)!=0){
    # cat(i,length(neighbors),"\n")
    if(setofobjects[match(neighbors[1],setofobjects[,"names"]),"processed"]==0){ #Comprobamos que no ha sido procesado
      instance=match(neighbors[1],setofobjects[,"names"])
      setofobjects[instance,"processed"]=1 #Marcamos el punto contenido en la lista de vecinos como procesado
      print(instance)
      dist.ord.2=snnmatrix[-instance,instance]
      neighbors.2=dist.ord.2>=eps
      neighbors.2=as.integer(names(dist.ord.2[neighbors.2]))  #Vecinos del elemento neighbors[i]
      if(length(neighbors.2)>=minpts){
        neighbors=unique(c(neighbors,neighbors.2)) #Unimos las dos listas de vecinos
      }
    }
    if(setofobjects[match(neighbors[1],setofobjects[,"names"]),"cluster"]==1){
      setofobjects[match(neighbors[1],setofobjects[,"names"]),"cluster"]=idC
    }
    neighbors=neighbors[-1]
  }
  return(setofobjects)
}

SNNDBSCAN=function(dataset,k,threshold,eps,minpts,coeficiente){
  snnmatrix=sparsesnn.dbscan(pos,k,threshold)
  clusters=DBSCAN(pos,snnmatrix,eps,minpts) 
  clusters=as.data.frame(clusters[,-match("processed",colnames(clusters))])
  #Preprocesamos aquellas instancias que etiquetemos como SD
  condicion.sd=round(nrow(dataset)*0.05)
  sd.clusters=NULL
  if(max(unique(clusters[,"cluster"]))!=1){
    for(i in 2:max(unique(clusters[,"cluster"]))){
      if(nrow(clusters[clusters[,"cluster"]==i,])<condicion.sd){
        sd.clusters=rbind(sd.clusters,clusters[clusters[,"cluster"]==i,])
      }
    }
    if(!is.null(nrow(sd.clusters))){
      sd.clusters=roversd(pos,neg,sd.clusters[,-ncol(sd.clusters)],coeficiente)
      return(rbind(dataset,cbind(sd.clusters[,-1],Class=1)))
    }else{
      dataset=rover(dataset,coeficiente)
      return(dataset)
    }
  }else{return(dataset)}
}


#eps es valor umbral de similaridad y minpts es el umbral de densidad
 dataset=candidatos[,13:14]
 snnmatrix=sparsesnn.dbscan(dataset,k=2,threshold=1)
 clusters=DBSCAN(dataset,snnmatrix,eps=10,minpts=17) 
 plot(clusters[,2:3],col=clusters[,"cluster"],pch=16)
