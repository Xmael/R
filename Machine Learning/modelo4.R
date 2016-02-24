library(xgboost)
library(caret)

#cargamos primero el archivo knn.R para construir el dataset
#source("knn.R")


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
subsample=0.4
gamma=1
eta=0.003
depth=5
nround=2000 #Usaremos 180



#load("balanceados_ros.RData")
#load("folds.RData")

#folds=createDataPartition(db$Proveedor,times=5,p=0.8)

  
ttrain =trainknn
#  ttrain = balanceados[[i]]
ttest=testknn


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
pred = matrix(pred,4,length(pred)/4)
pred = t(pred)

ganadores=apply(pred,1,which.max)
confusionMatrix(ganadores,ttest$Proveedor)

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

                          