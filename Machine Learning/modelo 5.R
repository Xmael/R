library(kknn)
library(xgboost)
library(caret)
db=read.csv("bookings",header=T,sep=";")
load("clusters.RData")
hoteles=read.csv("hoteles.csv",header=T,sep=";")
hoteles$provs=as.character(hoteles$provs)
hoteles=as.data.frame(cbind(hoteles[,2],sapply(1:nrow(hoteles),function(i){length(unique(unlist(strsplit(hoteles$provs[i],","))))})))
colnames(hoteles)=c("Hoteles","Proveedores")
db=db[as.Date(db$Fecha)>as.Date("2014-11-04"),]
db=db[,c(3,4,9,10,11,12,13,15,17,18,14)]
db=merge(db,hoteles)
db[,c(11,12)]=db[,c(12,11)]
colnames(db)[c(11,12)]=c("Proveedores","Proveedor")


db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana= factor(db$Diasemana,levels(db$Diasemana)[c(3,4,5,2,7,1,6)]) #Ordena los niveles de los días por su orden en la semana
db$Mes=factor(db$Mes,levels(db$Mes)[c(4,5,8,1,9,7,6,2,12,11,10,3)])

#Quito valores perdidos en el source
mdb=as.matrix(db)
indices=unlist(sapply(1:nrow(db),function(i){if(any(is.na(mdb[i,]))){return(i)}}))
db=db[-indices,]
db=data.frame(Clusters=clusters[1:nrow(db)],db) #añade los clusters
db=db[,-match(c("Precio","Netprice"),colnames(db))]

indices=which(db$Proveedor==c("EH") | db$Proveedor==c("ST"))

db=db[-indices,]

db$Proveedor=droplevels(db$Proveedor)
niveles=levels(db$Proveedor)
levels(db$Proveedor)=c(1:4)

for(j in 1:(ncol(db)-1)){
  if(class(db[,j])=="factor"){
    db[,j]=as.integer(db[,j])
  }
}

rownames(db)=1:nrow(db)

#Creo las particiones
folds=createDataPartition(db$Proveedor,times=5,p=0.8)

#Parámetros de entrada para el xgboost
mylambda=1
myalpha=0.8
mylambda_bias=0
child_range=c(0.5, 1, 2, 3, 4, 5, 10)
mychild=3
round_count=3
seednum=20150320
set.seed(seednum)
# Parámetros generales
param <- list("objective" = "multi:softprob",
              "eval_metric" = "merror",
              "num_class" = 4,
              "lambda" = mylambda,
              "lambda_bias" = mylambda_bias,
              "alpha" = myalpha,
              "min_child_weight" = mychild,
              "nthread" = 6)
seednum=20150320
set.seed(seednum)
colsample=0.2
subsample=0.4
gamma=1
eta=0.003
depth=5
nround=2400 #Usaremos 180


cv=lapply(1:5,function(i){
  train=db[folds[[i]],]
  test=db[-folds[[i]],]
  
  train[,ncol(train)]=as.factor(train[,ncol(train)])
  model=kknn(Proveedor~.,train=train,test=train,k=3,distance=2,kernel="optimal") #Nos quedamos con los primeros cinco vecinos
  clase.probable.knn=round(apply(model$CL[,-1],1,function(model) 
    as.integer(names(which.max(table(model))))))
  ttrain=cbind(train[,-ncol(train)],clase.probable.knn,Proveedor=train[,ncol(train)])
  
  test[,ncol(test)]=as.factor(test[,ncol(test)])
  model=kknn(Proveedor~.,train=train,test=test,k=3,distance=2,kernel="optimal") #Nos quedamos con los primeros cinco vecinos
  clase.probable.knn=round(apply(model$CL[,-1],1,function(model) 
    as.integer(names(which.max(table(model))))))
  ttest=cbind(test[,-ncol(test)],clase.probable.knn,Proveedor=test[,ncol(test)])
  
  ttrain=train
  ttest=test
  #Construyo la matriz para el xgboost
  y = ttrain[,ncol(ttrain)] #y da la lista de etiquetas para el training
  y = as.integer(y)-1 #xgboost take features in [0,numOfClass)
  
  x = rbind(ttrain[,-ncol(ttrain)],ttest[,-ncol(ttest)])
  #x = rbind(train[,names],test[,names])
  x = as.matrix(x)
  x = matrix(as.numeric(x),nrow(x),ncol(x))
  trind = 1:length(y) #Esto es para el training
  teind = (nrow(ttrain)+1):nrow(x)  #Esto es para el test
  train<-x[trind,]
  test<-x[teind,]
  
  #Creo el modelo
  bst = xgboost(param=param, data = train, label=y, nrounds=nround,max.depth=depth)
  
  #Hago la predicción
  pred = predict(bst,test)
  pred = matrix(pred,4,length(pred)/4)
  pred = t(pred)
  
  #Calculo métricas
  ganadores=apply(pred,1,which.max)
  metrica=confusionMatrix(ganadores,ttest$Proveedor)
  
  ordenes=t(apply(pred,1,function(pred){order(pred,decreasing=T)}))
  
  count=0
  fallos=NULL
  for(i in 1:nrow(pred)){
    if(ordenes[i,1]==ttest$Proveedor[i] | ordenes[i,2]==ttest$Proveedor[i]){
      count=count+1
    }else{
      fallos=c(fallos,ordenes[i,1])
    }
  }
  accu=count/nrow(pred)
  fallos=as.factor(fallos)
  levels(fallos)=niveles[as.integer(levels(fallos))]
  as.integer(table(fallos))/table(db$Proveedor)[match(names(table(fallos)),niveles)]*100
  return(list(metrica,accu))

})

print(apply(as.data.frame(sapply(1:5,function(i) (c(cv[[i]][[1]][[3]][1],cv[[i]][[1]][[3]][2])))),1,mean))
print(mean(sapply(1:5,function(i) (cv[[i]][[2]]))))