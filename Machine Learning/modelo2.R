library(randomForest)
library(caret)
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

load("folds.RData")
load("balanceados_ros.RData")

for (i in c(1,2,3,4,5,6,7,8,9,10)){
  db[,i]=as.integer(db[,i])
}


  colnames(trainknn)[1:(ncol(trainknn)-1)]=paste("X",c(1:(ncol(trainknn)-1)),sep="")
  colnames(testknn)[1:(ncol(testknn)-1)]=paste("X",c(1:(ncol(testknn)-1)),sep="")
  trainknn[,ncol(trainknn)]=as.factor(trainknn[,ncol(trainknn)])
  testknn[,ncol(testknn)]=as.factor(testknn[,ncol(testknn)])
  model=randomForest(Proveedor~.,data=trainknn,ntree=200,do.trace=T)
  pred=predict(model,testknn)
  metrica=confusionMatrix(pred,testknn$Proveedor)

print(apply(as.data.frame(sapply(1:5,function(i) (c(cv[[i]][[3]][1],cv[[i]][[3]][2])))),1,mean))
