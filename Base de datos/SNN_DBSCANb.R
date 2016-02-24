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
  cat("Calculando distancias\n")
  distances=as.matrix(dist(dataset))
  cat("Ordenando distancias\n")
  orderknn=t(apply(distances,1,order))
  orderknn=orderknn[,1:k]
  cat("Calculando matriz de similitud")
  snnmatrix=mclapply(1:nrow(dataset),snn.dbscan,data=orderknn,threshold,
                     mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = getOption("mc.cores", 4L),
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE)
  snnmatrix=matrix(unlist(snnmatrix),nrow=nrow(orderknn),ncol=nrow(orderknn))
  rownames(snnmatrix)=rownames(dataset)
  colnames(snnmatrix)=rownames(dataset)
  return(snnmatrix) #Me devuelve una matriz de similitud dispersa (con muchos ceros)
}

DBSCAN<-function(dataset,snnmatrix,eps,minpts){
  setofobjects=as.matrix(cbind(names=as.integer(rownames(dataset)),dataset,processed=0,cluster=1))
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
#     cat(i,length(neighbors),"\n")
    if(setofobjects[match(neighbors[1],setofobjects[,"names"]),"processed"]==0){ #Comprobamos que no ha sido procesado
      instance=match(neighbors[1],setofobjects[,"names"])
      setofobjects[instance,"processed"]=1 #Marcamos el punto contenido en la lista de vecinos como procesado
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

SNNDBSCAN=function(dataset,threshold,version){
  sd.clusters.pos=NULL
  sd.clusters.neg=NULL
  pos=dataset[dataset[,ncol(dataset)]==1,-ncol(dataset)]
  neg=dataset[dataset[,ncol(dataset)]==0,-ncol(dataset)]
#  if(nrow(pos)>200){
#    k=200
#  }else{
    k=ifelse(round(nrow(pos)/3)<5,3,round(nrow(pos)/3))
#  }
  cat("Calculando matriz minoritaria\n")
  snnmatrix=sparsesnn.dbscan(pos,k,threshold)
  cat("Calculando desviacion\n")
  desviacion.tipica=median(apply(snnmatrix,1,function(x){sd(x[x!=0],na.rm=T)}),na.rm=T)
  if(is.na(desviacion.tipica)){desviacion.tipica=0}
  cat("Calculando mediana\n")
  eps=median(apply(snnmatrix,1,function(x){median(x[x!=0],na.rm=T)}),na.rm=T)-2*desviacion.tipica
  minpts=ceiling(k/5)
  cat("Creando los clusters\n")
  clusters.pos=DBSCAN(pos,snnmatrix,eps,minpts)
  clusters.pos=as.data.frame(clusters.pos[,-match("processed",colnames(clusters.pos))])
  cat("Número de clusters para la clase minoritaria",max(unique(clusters.pos$cluster)-1),"\n")
  if(version=="pos"){
    return(list(clusters.pos,NULL))
  }
  if(version=="both"){
    cat("Calculando matriz mayoritaria\n")
    k=ifelse(round(nrow(neg)/3)<5,3,round(2*sqrt(round(nrow(neg)/3))))
    snnmatrix=sparsesnn.dbscan(neg,k,threshold)  
    desviacion.tipica=median(apply(snnmatrix,1,function(x){sd(x[x!=0],na.rm=T)}),na.rm=T)
    if(is.na(desviacion.tipica)){desviacion.tipica=0}
    eps=median(apply(snnmatrix,1,function(x){median(x[x!=0],na.rm=T)}),na.rm=T)-2*desviacion.tipica #Valor medio de la similitud para aquellos dtaos que son distintos de cero
    minpts=ceiling(nrow(neg)*0.05)
    clusters.neg=DBSCAN(neg,snnmatrix,eps=10,minpts)
    clusters.neg=as.data.frame(clusters.neg[,-match("processed",colnames(clusters.neg))])
    cat("Número de clusters para la clase mayoritaria",max(unique(clusters.neg$cluster)-1),"\n")
    return(list(clusters.pos,clusters.neg))
  }
}


#eps es valor umbral de similaridad y minpts es el umbral de densidad
# dataset=neg
 snnmatrix=sparsesnn.dbscan(dataset,k=3,threshold=2)
 clusters=DBSCAN(dataset,snnmatrix,eps=2,minpts=2) 
 plot(clusters[,1:2],col=clusters[,"cluster"],pch=16)
