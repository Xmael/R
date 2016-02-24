library(caret)
library(randomForest)
setwd("/home/ismael/Trabajo")
db=read.csv("bookings",header=T,sep=";")

db=db[as.Date(db$Fecha)>as.Date("2014-11-04"),]
db=db[,c(5,6,9,10,11,12,13,15,14)]
db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana.c= factor(db$Diasemana.c,levels(db$Diasemana.c)[c(3,4,5,2,7,1,6)]) #Ordena los niveles de los días por su orden en la semana
db$Mes.c=factor(db$Mes.c,levels(db$Mes.c)[c(4,5,8,1,9,7,6,2,12,11,10,3)])

#Guardamos la información de los niveles para recuperarla más adelante
nivelespais=data.frame(Pais=as.factor(unique(db$País.Hotel)),Factor=as.integer(as.factor(unique(db$País.Hotel))))
nivelesciudad=data.frame(Ciudad=as.factor(unique(db$Ciudad)),Factor=as.integer(as.factor(unique(db$Ciudad))))
nivelessource=data.frame(Source=as.factor(unique(db$Source)),Factor=as.integer(as.factor(unique(db$Source))))

recuento=as.data.frame(table(db$Hotel))
colnames(recuento)=c("Hotel","Codificacion")
db=merge(recuento,db)

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

db$Proveedor=droplevels(db$Proveedor)
niveles=levels(db$Proveedor)
levels(db$Proveedor)=c(1:4)

for(j in 1:(ncol(db)-1)){
  if(class(db[,j])=="factor"){
    db[,j]=as.integer(db[,j])
  }
}

db=cbind(db[,-match("Proveedor",colnames(db))],db[,match("Proveedor",colnames(db))])

recuento=as.data.frame(table(db$País.Hotel))
colnames(recuento)=c("País.Hotel","CodPais")
db=merge(recuento,db)

recuento=as.data.frame(table(db$Ciudad))
colnames(recuento)=c("Ciudad","CodCiudad")
db=merge(recuento,db)

colnames(db)=c("Ciudad","CodCiudad","PaisHotel","CodPais","Hotel","CodHotel","Diasemanac","Mesc","Source","Personas","Noches",
               "RS","HB","SH","GT","Proveedor" )

db[,1]=as.integer(levels(db[,1]))[db[,1]]
db[,3]=as.integer(levels(db[,3]))[db[,3]]
db$Proveedor=as.factor(db$Proveedor)
db$RS=as.factor(db$RS)
db$HB=as.factor(db$HB)
db$SH=as.factor(db$SH)
db$GT=as.factor(db$GT)

niveles=lapply(1:ncol(db),function(i){unique(db[,i])})

folds=createDataPartition(db$Proveedor,times=5,p=0.8)

i=1 #No cargar esto
train=db[folds[[i]],]
test=db[-folds[[i]],]


#####RANDOM FORESTS#####
model=randomForest(Proveedor~.,data = train,ntree=1000,do.trace=T)
pred.random=predict(model,test[,-ncol(test)],type="prob")
ganadores=apply(pred.random,1,which.max)
metrica=confusionMatrix(ganadores,test$Proveedor)

ordenes=t(apply(pred.random,1,function(pred){order(pred,decreasing=T)}))


count=0
fallos=NULL
for(i in 1:nrow(pred.random)){
  if(ordenes[i,1]==test$Proveedor[i] | ordenes[i,2]==test$Proveedor[i]){
    count=count+1
  }else{
    fallos=c(fallos,ordenes[i,1])
  }
}
accu=count/nrow(pred.random)

#####DEEP LEARNING#####
h2o.init(ip = "127.0.0.1",max_mem_size = "24G",nthreads=7)
for(i in 1:ncol(train)){train[,i]=as.factor(train[,i])}
for(i in 1:ncol(test)){test[,i]=as.factor(test[,i])}
train_hex=as.h2o(train, destination_frame = "train")
test_hex=as.h2o(test, destination_frame = "test")

deep=h2o.deeplearning(x = colnames(train)[1:ncol(train)-1],y ="Proveedor",training_frame=train_hex,
                      hidden=c(28), #(80,40,20)
                      seed=1234,
                      epochs = 200, #20
                      adaptive_rate = F,
                      score_validation_samples = 1500,
                      score_training_samples = round(nrow(train)*0.5),
                      train_samples_per_iteration= 0,
                      activation="RectifierWithDropout",
                      momentum_start = 0.2,
                      momentum_stable = 0.99,
                      momentum_ramp = 100,
                      rate =0.001, #0.005
                      l2 = 0.0002, #0.002
                      #l1 = 0.00001,
                      hidden_dropout_ratios = c(0.5), #c(0.5,0.5,0.5)
                      #                        initial_weight_distribution="Normal",
                      distribution="multinomial",
                      loss="CrossEntropy",
                      balance_classes=T,
                      fast_mode = F,
                      stopping_rounds = 10,
                      stopping_metric= "MSE",
                      quiet_mode=F,
                      nesterov_accelerated_gradient=T	
)

pred.deep=h2o.predict(deep,test_hex[1,])
cat("Ronda número",i," Accuracy",confusionMatrix(as.data.frame(pred.deep)[,1],test[,ncol(test)])$overall[1],"\n")
table(as.data.frame(pred.deep)[,1])
pred.deep=as.data.frame(pred.deep[,-1])

count=0
fallos=NULL
prediccion=t(apply(pred.deep,1,function(t){order(t,decreasing = T)}))
for(i in 1:nrow(pred.deep)){
  if(prediccion[i,1]==test$Proveedor[i] | prediccion[i,2]==test$Proveedor[i]){
    count=count+1
  }else{
    fallos=rbind(fallos,cbind(i,prediccion[i,1]))
  }
}
accu=count/nrow(pred.deep)

####XGBOOST#####
mychild=3
round_count=3
seednum=20150320
set.seed(seednum)
# Parámetros generales
param <- list("objective" = "multi:softprob",
              "eval_metric" = "merror",
              "num_class" = 4,
              "lambda" = 0.1,
              "lambda_bias" = 0,
              "alpha" = 0.8,
              "min_child_weight" = 3,
              "nthread" = 6,
              "eta"=0.1,
              "depth"=5,
              "gamma"=1,
              "colsample"=0.2,
              "subsample"=0.4)

####Parámetros de Carlos con el mejor resultado####
seednum=20150320
set.seed(seednum)
depth=5
nround=3000 #Usaremos 2300

ttrain =db[folds[[i]],]
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

bst = xgboost(param=param, data = train, label=y, nrounds=nround,verbose = 1)
pred = predict(bst,test)
pred = matrix(pred,4,length(pred)/4)
pred.xgboost = t(pred)

ganadores=apply(pred.xgboost,1,which.max)
metrica=confusionMatrix(ganadores,ttest$Proveedor)

ordenes=t(apply(pred.xgboost,1,function(pred){order(pred,decreasing=T)}))

count=0
fallos=NULL
for(i in 1:nrow(pred.xgboost)){
  if(ordenes[i,1]==ttest$Proveedor[i] | ordenes[i,2]==ttest$Proveedor[i]){
    count=count+1
  }else{
    fallos=c(fallos,ordenes[i,1])
  }
}
accu=count/nrow(pred.xgboost)


#Ensemble
pred.total=(pred.deep+pred.random.ensemble+pred.xgboost)/3
ganadores=apply(pred.total,1,which.max)
metrica=confusionMatrix(ganadores,test$Proveedor)

count=0
fallos=NULL
prediccion=t(apply(as.data.frame(pred.total),1,function(t){order(t,decreasing = T)}))
for(i in 1:nrow(prediccion)){
  if(prediccion[i,1]==test$Proveedor[i] | prediccion[i,2]==test$Proveedor[i]){
    count=count+1
  }else{
    fallos=rbind(fallos,cbind(i,prediccion[i,1]))
  }
}
accu=count/nrow(test)
