library(xgboost)
library(caret)
db=read.csv("bookings",header=T,sep=";")
load("clusters.RData")
db=db[as.Date(db$Fecha)>as.Date("2014-11-04"),]
db=db[,c(2,3,9,10,11,12,13,15,14)]
db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana=factor(db$Diasemana,levels(db$Diasemana)[c(3,4,5,2,7,1,6)])
db$Mes=factor(db$Mes,levels(db$Mes)[c(4,5,8,1,9,7,6,2,12,11,10,3)])


#Añadimos la información de los proveedores
hoteles=read.csv("hoteles.csv",header=T,sep=";")
hoteles$provs=as.character(hoteles$provs)

#Creamos cuatro variables dummy
dummys=t(sapply(1:nrow(hoteles),function(i){
  temp=unique(unlist(strsplit(hoteles$provs[i],",")))
  dummy=rbind(c("RS","HB","SH","GT"))
  apply(t(sapply(1:length(temp),function(i){ifelse(dummy==temp[i],1,0)})),2,sum)
  }))
colnames(dummys)=c("RS","HB","SH","GT")
hoteles=cbind(hoteles$uniqid,dummys)
colnames(hoteles)[1]="Hotel"


#Quito valores perdidos en el source
mdb=as.matrix(db)
indices=unlist(sapply(1:nrow(db),function(i){if(any(is.na(mdb[i,]))){return(i)}}))
db=db[-indices,]

indices=which(db$Proveedor==c("EH") | db$Proveedor==c("ST"))

db=db[-indices,]

#Añado las dummy variables
db=merge(db,hoteles)

#Cambio la posición de la columna proveedor
db=cbind(db[,-match("Proveedor",colnames(db))],Proveedor=db[,match("Proveedor",colnames(db))])

#Elimino los niveles que están vacíos
db$Proveedor=droplevels(db$Proveedor)
niveles=levels(db$Proveedor)
levels(db$Proveedor)=c(1:4) #Los cambio a valores numéricos

for(j in 1:(ncol(db)-1)){
  if(class(db[,j])=="factor"){
    db[,j]=as.integer(db[,j])
  }
}

rownames(db)=1:nrow(db)

#añade los clusters
#db=data.frame(Clusters=clusters[1:nrow(db)],db) 


####Parámetros####
mylambda=1
myalpha=0.8
mylambda_bias=0.5
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

####Parámetros de Carlos con el mejor resultado####
seednum=20150320
set.seed(seednum)
colsample=0.2
subsample=0.6
gamma=1
eta=0.003
depth=5
nround=800 #Usaremos 180

folds=createDataPartition(db$Proveedor,times=5,p=0.8)

#load("balanceados_ros.RData")
#load("folds.RData")

cv=lapply(1:5,function(i){
  ttrain =db[folds[[i]],]
#  ttrain = balanceados[[i]]
  ttest=db[-folds[[i]],]

#  ttrain=cbind(ttrain[,-ncol(ttrain)],clase.probable.knn,Proveedor=ttrain[,ncol(ttrain)])
  
  #ttest=cbind(ttest[,-ncol(ttest)],clase.probable.knn,Proveedor=ttest[,ncol(ttest)])
  
  
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
  
  bst = xgboost(param=param, data = train, label=y, nrounds=nround)
  pred = predict(bst,test)
  pred = matrix(pred,4,length(pred)/4)
  pred = t(pred)
  
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
                          