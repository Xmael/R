library(caret)
library(randomForest)
setwd("/home/ismael/Trabajo")
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

recuento=as.data.frame(table(db$Hotel))
colnames(recuento)=c("Hotel","Codificacion")
db=merge(recuento,db)

colnames(db)=c("Hotel","Codificacion","Diasemana","Mes","Source","NPers","Noches","PaisHotel","Ciudad","RS","HB","SH","GT","Proveedor" )
folds=createDataPartition(db$Proveedor,times=5,p=0.8)

i=1 #No cargar esto
train=db[folds[[i]],]
test=db[-folds[[i]],]

model=randomForest(Proveedor~.,data = train,ntree=100)
pred=predict(model,test[,-ncol(test)])
confusionMatrix(pred,test[,ncol(test)])
