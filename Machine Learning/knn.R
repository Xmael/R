library(kknn)

#Train
train=read.csv("train.csv",header=T)
for(i in c(1,2,3,4,7,8,9)){train[,i]=as.factor(train[,i])}

vecinos=function(i,train){
  distancia=NULL
  for(j in 1:nrow(train)){
    cat(j,"\n")
    if(i==j){
      distancia=c(distancia,NA)
    }else{
      distancia=c(distancia,daisy(train,train,metric = "gower"))
    }
  }
}

model=kknn(Proveedor~.,train=train,test=train,k=3,distance=2,kernel="optimal") #Nos quedamos con los primeros cinco vecinos

clase.probable.knn=round(apply(model$CL[,-1],1,function(model) 
   as.integer(names(which.max(table(model)))))) #Creamos variable con la clase más probable del knn

#mdb=as.matrix(cbind(train[,-ncol(train)],Proveedor=as.integer(levels(train[,ncol(train)]))[train[,ncol(train)]]))

# train.2=as.matrix(train)
# aumentado=matrix(nrow=nrow(train),ncol=(ncol(train.2))*5)
# for(i in 1:nrow(train.2)){
#   temp=NULL
#   cat(i,"\n")
#   for(j in 2:ncol(model$C)){
#     temp=c(temp,train.2[model$C[i,j],])
#   }
#   aumentado[i,]=temp
# }


trainknn=cbind(train[,-ncol(train)],clase.probable.knn,Proveedor=train[,ncol(train)])


#Test
test=read.csv("test.csv",header=T)
test[,ncol(test)]=as.factor(test[,ncol(test)])
model=kknn(Proveedor~.,train=train,test=test,k=3,distance=2,kernel="optimal") #Nos quedamos con los primeros cinco vecinos
clase.probable.knn=round(apply(model$CL[,-1],1,function(model) 
  as.integer(names(which.max(table(model))))))


train.2=as.matrix(train)
aumentado=matrix(nrow=nrow(test),ncol=4)
for(i in 1:nrow(test)){
  temp=NULL
  for(j in 1:ncol(model$C)){
    temp=c(temp,train.2[model$C[i,j],3])
  }
  aumentado[i,]=c(temp,model$fitted.values[i])
}


#mdb=as.matrix(cbind(train[,-ncol(train)],Proveedor=as.integer(levels(train[,ncol(train)]))[train[,ncol(train)]]))

# test.2=as.matrix(test)
# aumentado=matrix(nrow=nrow(test.2),ncol=(ncol(test.2))*5)
# for(i in 1:nrow(test.2)){
#   temp=NULL
#   cat(i,"\n")
#   for(j in 2:ncol(model$C)){
#     temp=c(temp,train.2[model$C[i,j],])
#   }
#   aumentado[i,]=temp
# }


testknn=cbind(test[,-ncol(test)],clase.probable.knn,Proveedor=test[,ncol(test)])
