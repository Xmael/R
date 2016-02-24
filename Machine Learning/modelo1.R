library(xgboost)
library(caret)
library(Metrics)

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

# for(i in 1:ncol(train)){
#   if(class(train[,i])=="factor"){
#     train[,i]=as.integer(train[,i])
#   }
# }

LogLoss <- function(actual, predicted, eps=1e-15) {
  predicted[predicted < eps] <- eps;
  predicted[predicted > 1 - eps] <- 1 - eps;
  -1/nrow(actual)*(sum(actual*log(predicted)))
}

folds=createDataPartition(db$Proveedor,times=5,p=0.8)
#h=sample(1:nrow(train),5000,replace=F)

cv=lapply(1:5,function(i){
  train=db[folds[[i]],]
  test=db[-folds[[i]],]
  dval<-xgb.DMatrix(data=data.matrix(test),label=as.integer(levels(test$Proveedor))[test$Proveedor]-1)
  dtrain<-xgb.DMatrix(data=data.matrix(train),label=as.integer(levels(train$Proveedor))[train$Proveedor]-1)
  watchlist<-list(val=dval,train=dtrain)
  param <- list(  objective           = "multi:softprob", 
                  booster = "gbtree",
                  eta                 = 0.02, # 0.06, #0.01,
                  max_depth           = 10, #changed from default of 8
                  subsample           = 0.9, # 0.7
                  colsample_bytree    = 0.7, # 0.7
                  num_class = 6
                  #num_parallel_tree   = 2
                  # alpha = 0.0001, 
                  # lambda = 1
  )
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 600, #300, #280, #125, #250, # changed from 300
                      verbose            = 0,
                      early.stop.round    = 100,
                      watchlist           = watchlist,
                      maximize            = FALSE,
                      eval_metric="mlogloss",
                      nthread = 8
  )
  
  pred=predict(clf,data.matrix(test[,-ncol(test)]))
  pred = matrix(pred,6,length(pred)/6)
  pred = t(pred)
  colnames(pred)=niveles
  
  ganadores=apply(pred,1,which.max)
  metrica=confusionMatrix(ganadores,test$Proveedor)
  return(metrica)
})
