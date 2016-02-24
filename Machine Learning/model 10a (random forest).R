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

folds=createDataPartition(db$Proveedor,times=5,p=0.8)

i=1 #No cargar esto
train=db[folds[[i]],]
test=db[-folds[[i]],]

model=randomForest(Proveedor~.,data = train,ntree=1000,do.trace=T)
pred.random=predict(model,test[,-ncol(test)],type="prob")
ganadores=apply(pred.random,1,which.max)
metrica=confusionMatrix(ganadores,test$Proveedor)

ordenes=t(apply(pred.random,1,function(pred){order(pred,decreasing=T)}))


count=0
fallos=NULL
for(i in 1:nrow(pred)){
  if(ordenes[i,1]==test$Proveedor[i] | ordenes[i,2]==test$Proveedor[i]){
    count=count+1
  }else{
    fallos=c(fallos,ordenes[i,1])
  }
}
accu=count/nrow(pred.random)
