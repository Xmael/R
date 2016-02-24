library(xgboost)
library(caret)

db=read.csv("bookings",header=T,sep=";")
clusters=load("clusters.RData")

db=db[,c(3,4,9,10,11,12,13,15,17,18,14)]
db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana= factor(db$Diasemana,levels(db$Diasemana)[c(3,4,5,2,7,1,6)]) #Ordena los niveles de los días por su orden en la semana
db$Mes=factor(db$Mes,levels(db$Mes)[c(4,5,8,1,9,7,6,2,12,11,10,3)])

#Quito valores perdidos en el source
mdb=as.matrix(db)
indices=unlist(sapply(1:nrow(db),function(i){if(any(is.na(mdb[i,]))){return(i)}}))
db=db[-indices,]
db=data.frame(Clusters=clusters,db) #añade los clusters
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

####Parámetros####
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
              "num_class" = 6,
              "lambda" = mylambda,
              "lambda_bias" = mylambda_bias,
              "alpha" = myalpha,
              "min_child_weight" = mychild,
              "nthread" = 6)

####Parámetros de Carlos con el mejor resultado####
seednum=20150320
set.seed(seednum)
colsample=0.2
subsample=0.6
gamma=1
eta=0.005
depth=10
nround=400 #Usaremos 180

folds=createDataPartition(db$Proveedor,times=5,p=0.8)
cv=lapply(1:5,function(i){
  
  ttrain =db[folds[[i]],]
  rownames(ttrain)=1:nrow(ttrain)
  colnames(ttrain)[ncol(ttrain)]="Proveedor"
  ttrain=INFFC(ttrain)
  
  #  ttrain = balanceados[[i]]
  ttest=db[-folds[[i]],]
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
  
  bst = xgboost(param=param, data = train, label=y, nrounds=nround,max.depth=depth)
  pred = predict(bst,test)
  pred = matrix(pred,6,length(pred)/6)
  pred = t(pred)
  
  ganadores=apply(pred,1,which.max)
  metrica=confusionMatrix(ganadores,ttest$Proveedor)
  
  ordenes=t(apply(pred,1,function(pred){order(pred,decreasing=T)}))
  
  count=0
  fallos=NULL
  for(j in 1:nrow(pred)){
    if(ordenes[j,1]==ttest$Proveedor[j] | ordenes[j,2]==ttest$Proveedor[j]){
      count=count+1
    }else{
      fallos=c(fallos,ordenes[j,1])
    }
  }
  accu=count/nrow(pred)
  fallos=as.factor(fallos)
  levels(fallos)=niveles[as.integer(levels(fallos))]
  as.integer(table(fallos))/table(db$Proveedor)[match(names(table(fallos)),niveles)]*100
  return(list(accu,metrica))
})

print(apply(as.data.frame(sapply(1:5,function(i) (c(cv[[i]][[2]][[3]][1],cv[[i]][[2]][[3]][2])))),1,mean))
