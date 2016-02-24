library(parallel)

db=read.csv("bookings",header=T,sep=";")

db=db[,c(3,4,5,6,9,10,11,12,13,15,17,18,14)]
db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana= factor(db$Diasemana,levels(db$Diasemana)[c(3,4,5,2,7,1,6)]) #Ordena los niveles de los días por su orden en la semana
db$Mes=factor(db$Mes,levels(db$Mes)[c(4,5,8,1,9,7,6,2,12,11,10,3)])

#Quito valores perdidos en el source
mdb=as.matrix(db)
indices=unlist(sapply(1:nrow(db),function(i){if(any(is.na(mdb[i,]))){return(i)}}))
db=db[-indices,]

db$Proveedor=droplevels(db$Proveedor)
niveles=levels(db$Proveedor)
levels(db$Proveedor)=c(1:6)

for(i in 1:(ncol(db)-1)){
  if(class(db[,i])=="factor"){
    db[,i]=as.integer(db[,i])
  }
}


getNeighbors<-function(i,distancia){
  order(distancia[,i])[2:6]
}

synthetic.instance <- function(instance, neighbors, data){
  
  # random neighbor
  n <- sample(1:5,1)
  neighbor <- data[neighbors[n],]
  
  # construct the synthetic instance
  pos <- runif(1)
  new.instance <- NULL
  nAtts <- dim(data)[2]
  for (f in 1:(nAtts)){
    if(class(instance[1,f])=="integer" | class(instance[1,f])=="factor"){ # nominal feature
      random <- runif(1)
      if(random < 0.5){
        value <- instance[f]
      } else {
        value <- neighbor[f]
      }
      new.instance <- c(new.instance, value)
    } else { # numeric feature
      value <- (1 - pos) * instance[f] + pos * neighbor[f]
      new.instance <- c(new.instance, value)
    }
  }
  
  return(new.instance)
}

f.sinteticos<-function(i,data,minoritaria,distancia,nreplicas){  
  cat(i,"    Clase minoritaria: ",minoritaria,"    Número de réplicas: ",nreplicas,"    \n")
  rownumber <- sample(1:dim(data)[1], 1)
  
  neighbors <- getNeighbors(rownumber,distancia)
  return(synthetic.instance(data[rownumber,], neighbors,data))
}


GLOBALCS<-function(df){
  sinteticos=NULL
  classes <-as.integer(sort(unique( as.character(df[,dim(df)[2]]))))
  frecuencias<-as.data.frame(table(df[,dim(df)[2]]))
  mayoritaria<-as.integer(frecuencias[which(frecuencias[,2]==max(frecuencias[,2])),1]) #Busca la clase mayoritaria
  minoritarias<-setdiff(classes,mayoritaria)
  for(minoritaria in minoritarias){
    c.sinteticos<-NULL
    environment()
    cat("Balanceando la clase ",minoritaria,"\n")
    nclass<-sum(df[,dim(df)[2]]==mayoritaria)
    pclass<-sum(df[,dim(df)[2]]==minoritaria)
    indices.minoritarios<-which(df[,dim(df)[2]]==minoritaria)
    data<-df[indices.minoritarios,-dim(df)[2]]
    distancia<-as.matrix(dist(data, diag=T, upper=T)) #Calcula distancia solo para minoritarios
    nreplicas=nrow(df[df[,ncol(df)]==mayoritaria,])-nrow(df[df[,ncol(df)]==minoritaria,])
    ninstancias=nreplicas
    while(ninstancias>0){
      tsinteticos<-(mclapply(1:ninstancias,f.sinteticos,data,minoritaria,distancia,nreplicas,
                             mc.preschedule = TRUE, mc.set.seed = TRUE,
                             mc.silent = FALSE, mc.cores = getOption("mc.cores", 6L),
                             mc.cleanup = TRUE, mc.allow.recursive = TRUE))
      tsinteticos<-matrix(unlist(tsinteticos), nrow=length(tsinteticos), byrow=T)
      tsinteticos<-na.omit(tsinteticos)
      c.sinteticos=rbind(c.sinteticos,tsinteticos)
      ninstancias=nreplicas-nrow(c.sinteticos)
    }
    aux<-cbind(c.sinteticos,minoritaria)
    colnames(aux)<-colnames(df)
    sinteticos<-rbind(sinteticos,aux)
    
  }
  ds<-rbind(df,sinteticos)
  rownames(ds)<-c(1:dim(ds)[1])
  return(ds)
}
load("folds.RData")
balanceados=lapply(1:5,function(i){return(GLOBALCS(db[folds[[i]],]))})
save(balanceados,file="balanceados.RData")


roversampling<-function(data){
  environment()
  clases<-sort(unique(as.character(data[,dim(data)[2]])))
  frecuencias<-as.data.frame(table(data[,dim(data)[2]]))
  mayoritaria<-as.character(frecuencias[which(frecuencias[,2]==max(frecuencias[,2])),1])
  minoritarias<-setdiff(clases,mayoritaria)
  nombre=colnames(data)[ncol(data)]
  c.sinteticos<-NULL
  for (minoritaria in minoritarias){
    cat("Sobremuestreando clase",minoritaria,"frente a clase",mayoritaria,"\n")
    #ubOver necesita que la mayoritaria esté etiquetada con la clase 0 y la minoritaria con la 1
    aux0<-data[which(data[,dim(data)[2]]==mayoritaria),]
    aux1<-data[which(data[,dim(data)[2]]==minoritaria),]
    aux1[,dim(aux1)[2]]<-1
    aux0[,dim(aux0)[2]]<-0
    aux<-rbind(aux0,aux1)
    aux<-aux[order(as.integer(rownames(aux))),]
    balanceado<-ubOver(aux[,-ncol(aux)],aux[,ncol(aux)],k=0)
    balanceado=cbind(balanceado$X,balanceado$Y)
    colnames(balanceado)[ncol(balanceado)]<-nombre
    balanceado<-balanceado[balanceado[,ncol(balanceado)]==1,]
    balanceado[,ncol(balanceado)]<-minoritaria
    c.sinteticos<-rbind(c.sinteticos,balanceado)
  }
  c.sinteticos<-rbind(c.sinteticos,data[which(data[,dim(data)[2]]==mayoritaria),])
  rownames(c.sinteticos)<-c(1:dim(c.sinteticos)[1])
  return(c.sinteticos)
}


library(unbalanced)
balanceados=lapply(1:5,function(i){return(roversampling(db[folds[[i]],]))})
save(balanceados,file="balanceados.RData")