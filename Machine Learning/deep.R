library(h2o)
library(caret)
setwd("/home/ismael/Trabajo")
h2o.init(ip = "127.0.0.1",max_mem_size = "24G",nthreads=8)
db=read.csv("bookings",header=T,sep=";")

db=db[,c(3,4,9,10,11,12,13,15,14)]
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

for(j in 1:(ncol(db)-1)){
  if(class(db[,j])=="factor"){
    db[,j]=as.integer(db[,j])
  }
}

colnames(db)=c("Diasemana","Mes","Hotel","Source","Personas","Noches","Pais","Ciudad","Proveedor")
load("folds.RData")

i=1 #No cargar esto
train=db[folds[[i]],]
test=db[-folds[[i]],]

htrain=as.h2o(train, destination_frame = "train")
htest=as.h2o(test, destination_frame = "test")
deep=h2o.deeplearning(x = colnames(train)[1:ncol(train)-1],y ="Proveedor",training_frame=htrain,
                      hidden=c(100,100),seed=1234,
                      epochs = 200,
                      activation="TanhWithDropout",
                      train_samples_per_iteration = 0,
                      initial_weight_distribution = "Uniform",
                      balance_classes = T,
                      use_all_factor_levels = T,
                      rate = 0.5,
                      input_dropout_ratio=0.2,
                      rho=0.9,
                      epsilon=1e-8,
                      adaptive_rate = F,)
pred=h2o.predict(deep,htest[,-ncol(htest)])
confusionMatrix(as.data.frame(pred)[,1],test[,ncol(test)])
