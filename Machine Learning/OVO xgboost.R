#Algoritmo One versus One con reducción dinámica para clasificador base random forest

OVO.DINAMICO<-function(train,test,model.knn){
  library(randomForest)
  library(kknn)
  atributos<-colnames(train[,-ncol(train)])
  clases<-sort(unique(train[,ncol(train)]))
  
  OVO<-function(train,test,clases){#Genera m*(m-1)/2 modelos binarios
    
    
    #Parámetros de entrada para el xgboost
    mylambda=1
    myalpha=0.8
    mylambda_bias=0
    child_range=c(0.5, 1, 2, 3, 4, 5, 10)
    mychild=3
    round_count=3
    seednum=20150320
    set.seed(seednum)
    # Parámetros generales
    param <- list(  objective           = "binary:logistic", 
                    booster = "gbtree",
                    max_depth           = 5, #changed from default of 8
                    subsample           = 0.4, # 0.7
                    colsample_bytree    = 0.2, # 0.7
                    eval_metric = "auc",
                    early.stop.round = 5
                    #num_parallel_tree   = 2
                    # alpha = 0.0001, 
                    # lambda = 1
    )
    
    seednum=20150320
    set.seed(seednum)
    gamma=1
    eta=0.003
    nround=1500 #Usaremos 180
    
    pred<-data.frame(id=1:nrow(test))
    for (i in clases){
      for (j in clases){
        pos1=match(i,clases)
        pos2=match(j,clases)
        if(i==j){#Evitamos generar modelos binarios con una única clase igual
          pred<-cbind(pred,NA)
          colnames(pred)[ncol(pred)]=paste(pos1,pos2,sep=".")
        }else if(i<j){
          cat("OVO ",i,"frente a",j,"\n")
          #Binariza{
          aux0<-train[which(train[,dim(train)[2]]==i),]
          aux1<-train[which(train[,dim(train)[2]]==j),]
          aux0[,ncol(aux0)]<-0
          aux1[,ncol(aux1)]<-1
          aux<-rbind(aux0,aux1)
          aux[,ncol(aux)]<-as.factor(aux[,ncol(aux)])
          aux<-aux[sample(nrow(aux)),]
          #}
          
          #Construyo la matriz para el xgboost
          h=sample(1:nrow(aux))[1:1000]
          dval<-xgb.DMatrix(data=data.matrix(apply(aux[h,-ncol(aux)],1:2,as.numeric)),label=as.integer(levels(aux$Proveedor[h]))[aux$Proveedor[h]])
          dtrain<-xgb.DMatrix(data=data.matrix(apply(aux[-h,-ncol(aux)],1:2,as.numeric)),label=as.integer(levels(aux$Proveedor[-h]))[aux$Proveedor[-h]])
          watchlist=list(val=dval,train=dtrain)
          
          #Creo el modelo
          model<-xgb.train(params=param, verbose=1, data = dtrain, nrounds=nround,watchlist=watchlist,maximize=T)
          
          #Hago la predicción
          temp= predict(model,data.matrix(apply(test[,-ncol(test)],1:2,as.numeric)))
#           temp = matrix(temp,2,length(temp)/2)
#           temp = t(temp)
#           temp=apply(temp,1,which.max)
          pred<-cbind(pred,temp)
          colnames(pred)[ncol(pred)]<-paste(pos1,pos2,sep=".")
        }else if(i>j){  #Evita repetir modelos
          nombre1=paste(pos2,pos1,sep=".")
          nombre2=paste(pos1,pos2,sep=".")
          pred<-cbind(pred,1-pred[,which(nombre1==colnames(pred))]) #La prob es la recíproca del primer modelo
          colnames(pred)[ncol(pred)]<-nombre2
        }
      }
    }
    return(pred[,-1])
  }
  
  transforma.dinamica<-function(i,pred.matrix,model.knn,clases){
    votacion<-t(matrix(pred.matrix[i,],length(clases),length(pred.matrix)/length(clases))) #Transforma la fila de 82 columnas en una matriz 9x9
    votacion<-apply(votacion,1:2,as.numeric)
    numeros.clases=match(names(table(model.knn[i,])),clases) #Toma las clases vecinas que genera model.knn para eliminar filas de clases que no sean vecinas
    votacion1<-cbind(votacion,apply(votacion,1,sum,na.rm=T))
    orden1<-order(votacion1[,ncol(votacion1)],decreasing=F)
    if(length(numeros.clases)==1){
      orden<-c(numeros.clases,setdiff(orden1,numeros.clases))  #Probando con la votación en exclusiva
      return(c(orden1))
    }else{
      votacion<-votacion[numeros.clases,numeros.clases]
      rownames(votacion)=numeros.clases
      colnames(votacion)=numeros.clases
      votacion<-cbind(votacion,apply(votacion,1,sum,na.rm=T))
      orden=as.integer(rownames(votacion)[order(votacion[,ncol(votacion)],decreasing=F)])
      orden=as.integer(c(orden,setdiff(orden1,as.integer(orden))))
      return(c(orden)) #Devuelve la clase ganadora
    }
  }
  
  pred.matrix<-OVO(ttrain,ttest,clases) #Genera la "hipermatriz" de votación
  pred.test<-t(sapply(1:nrow(test),transforma.dinamica,pred.matrix,model.knn,clases))
#  pred.test<-data.frame(matrix(unlist(pred.test), nrow=length(pred.test), byrow=T))
#  colnames(pred.test)=colnames(train)[ncol(train)]
#  levels(pred.test[,ncol(pred.test)])=levels(test[,ncol(test)])
  return(pred.test)
}