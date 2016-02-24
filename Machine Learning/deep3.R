library(h2o)
library(caret)
setwd("/home/ismael/Trabajo")
h2o.init(ip = "127.0.0.1",max_mem_size = "24G",nthreads=7)
db=read.csv("bookings",header=T,sep=";")

db=db[,c(5,6,9,10,11,12,13,15,14)]
db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana.c= factor(db$Diasemana.c,levels(db$Diasemana.c)[c(3,4,5,2,7,1,6)]) #Ordena los niveles de los días por su orden en la semana
db$Mes.c=factor(db$Mes.c,levels(db$Mes.c)[c(4,5,8,1,9,7,6,2,12,11,10,3)])

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

colnames(db)=c("Hotel","Codificacion","Diasemana","Mes","Source","NPers","Noches","PaisHotel","Ciudad","RS","HB","SH","GT","Proveedor" )

db[,1]=as.factor(db[,1])
db[,2]=as.factor(db[,2])
db[,ncol(db)]=as.factor(db[,ncol(db)])

#folds=createDataPartition(db$Proveedor,times=5,p=0.8)

i=1 #No cargar esto
train=db[folds[[i]],]
test=db[-folds[[i]],]

train_hex=as.h2o(train, destination_frame = "train")
test_hex=as.h2o(test, destination_frame = "test")

rondas=10
pred_e=as.data.frame(matrix(rep(0,4*nrow(test)),nrow=nrow(test),ncol=4))
for(i in 1:rondas){
  deep=h2o.deeplearning(x = colnames(train)[1:ncol(train)-1],y ="Proveedor",training_frame=train_hex,
                        hidden=c(28), #(80,40,20)
                        seed=1234,
                        epochs = 0, #20
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
  
  pred=h2o.predict(deep,test_hex)
  cat("Ronda número",i," Accuracy",confusionMatrix(as.data.frame(pred)[,1],test[,ncol(test)])$overall[1],"\n")
  table(as.data.frame(pred)[,1])
  
  pred_e=as.data.frame(pred[,2:5])+pred_e
}


count=0
fallos=NULL
prediccion=t(apply(as.data.frame(pred[,-1]),1,function(t){order(t,decreasing = T)}))
for(i in 1:nrow(pred)){
  if(prediccion[i,1]==test$Proveedor[i] | prediccion[i,2]==test$Proveedor[i]){
    count=count+1
  }else{
    fallos=rbind(fallos,cbind(i,prediccion[i,1]))
  }
}
accu=count/nrow(test)

pred_e=pred_e/rondas
pred_e=apply(pred_e,1,which.max)
metrica=confusionMatrix(pred_e,test[,ncol(test)])
print(metrica)
