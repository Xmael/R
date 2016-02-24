#########IPF########
library(gbm)
library(kknn)
library(C50)

########
db=read.csv("bookings",header=T,sep=";")
load("clusters.RData")
db=db[,c(3,4,9,10,11,12,13,15,17,18,14)]
db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana= factor(db$Diasemana,levels(db$Diasemana)[c(3,4,5,2,7,1,6)]) #Ordena los niveles de los días por su orden en la semana
db$Mes=factor(db$Mes,levels(db$Mes)[c(4,5,8,1,9,7,6,2,12,11,10,3)])

#Quito valores perdidos en el source
mdb=as.matrix(db)
indices=unlist(sapply(1:nrow(db),function(i){if(any(is.na(mdb[i,]))){return(i)}}))
db=db[-indices,]
db=data.frame(Clusters=clusters,db) #añade los clusters
db=db[,-match(c("Precio","Netprice"),colnames(db))]

indices=which(db$Proveedor==c("EH") | db$Proveedor==c("ST"))

db=db[-indices,]

db$Proveedor=droplevels(db$Proveedor)
niveles=levels(db$Proveedor)
levels(db$Proveedor)=c(1:4)

for(j in 1:(ncol(db)-1)){
  if(class(db[,j])=="factor"){
    db[,j]=as.integer(db[,j])
  }
}

rownames(db)=1:nrow(db)
#######################

filters<-function(i,train,test){
  if (i%%3==1){
   cat("Clasificador 3NN\n")
    clasif<-kknn(class~., train,test,k=3,kernel="rectangular",distance=2) #Utiliza distancia Minkowski de grado 2, que es la euclidea
    df<-as.data.frame(predict(clasif,newdata= test))
    colnames(df)="knn"
    return(df)
  }else if (i%%3==2){
    cat("Clasificador J48\n")
    clasif<-C5.0(class~.,train,trials=1)
    df<-as.data.frame(predict(clasif,newdata= test))
    colnames(df)="j48"
    return(df)
  }else if (i%%3==0){
    cat("Clasificador Regresión Logística\n")
    suppressWarnings(clasif<-glm(class~.,data=train,family=binomial)) #Eliminamos el mensaje de warning que indica que hay separación total
    suppressWarnings(df<-predict(clasif,newdata=test,type="response"))
    df=as.data.frame(ifelse(df>0.5,1,0))
    colnames(df)="logit"
    return(df)
  }
}


votacion<-function(k,Dtv,df,n){
  Dtv[which(Dtv[,n+1]!=df[,k]),n+2]=Dtv[which(Dtv[,n+1]!=df[,k]),n+2]+1
  votos<-Dtv[,n+2]
  return(votos)
}

score=function(CT,CN){
  k=5
  cat("Calculando vecinos\n")
  neighbors=kknn(k=5,class~.,train=CT,test=CT,distance=2,kernel = "rectangular")
  cat("Resto de operaciones\n")
  te=as.matrix(data.frame(1:nrow(CT),te=0)[-1])
  rownames(te)=1:nrow(te)
  ruidosas=as.vector(neighbors$C[as.integer(rownames(CN)),])
  te[as.integer(names(table(ruidosas))),]=as.integer(table(ruidosas))
  confidence=1/sqrt(1+te^2)
  kconfidence=apply(neighbors$C,2,function(nn){confidence[nn]})
  clases=apply(neighbors$C,2,function(n){CT[n,ncol(CT)]})
  differentclases=apply(clases,2,function(cl){ifelse(cl==clases[,1],-1,1)})
  ne=apply(neighbors$C[,-1],2,function(n){n %in% rownames(CN)})
  ne=ifelse(ne==TRUE,1,0)
  ne=as.matrix(apply(ne,1,sum))
  #Construyo isnoise a partir de una matriz de -1 con número de filas igual al de CT
  isnoise=as.matrix(rep(-1,nrow(CT)))
  isnoise[as.integer(rownames(CN))]=1
  clean=(k+isnoise*(ne-k))/(2*k)
  kclean=apply(neighbors$C,2,function(nn){clean[nn]})
  neighborhood=kconfidence*differentclases*kclean
  neighborhood=apply(neighborhood,1,sum)/k
  NS=confidence*neighborhood
  return(NS)
}

INFFC<-function(CT){
  dataset=CT
  colnames(CT)[ncol(CT)]="class"
  CT[,ncol(CT)]=as.factor(CT[,ncol(CT)])
  mayoria=2
  iteracion=0
  p=round(nrow(CT)*0.10)
  g=3
  while(g!=0){
    #Filtrado inicial
    
    cat("##1.Filtrado inicial##\n")
    
    df<-as.data.frame(sapply(1:3,filters,CT,CT)) #Crea los clasificadores
    Dtv<-cbind(CT,votos=0)
    Votos<-sapply(1:3,votacion,Dtv,df,(ncol(CT)-1)) #Comprueba los fallos para los tres clasificadores. Añade un voto por cada uno de ellos
    CT<-cbind(CT,votos=(apply(Votos,1,sum)))
    CPC<-CT[which(CT[,ncol(CT)]<mayoria),-ncol(CT)] #Me quedo con las instancias no ruidosas
    CT=CT[,-ncol(CT)] #Quito la columna de votos
    
    #Filtrado libre de ruido
    
    cat("\n##2.Filtrado libre de ruido\n")
    df<-as.data.frame(sapply(1:3,filters,CPC,CT)) #Utilizamos el conjunto filtrado anterior como entrenamiento en este paso
    Dtv<-cbind(CT,votos=0)
    Votos<-sapply(1:3,votacion,Dtv,df,(ncol(CT)-1)) 
    CT<-cbind(CT,votos=(apply(Votos,1,sum)))
    CN=CT[which(CT[,ncol(CT)]>=mayoria),-ncol(CT)]
#    CC=CT[which(CT[,ncol(CT)]<mayoria),-ncol(CT)]
    CT=CT[,-ncol(CT)] #Quito la columna de votos
    
    #Puntuación
    cat("\n##3.Cálculo de la puntuación de ruido##\n")
    NS=score(CT,CN)
    #Obtengo los índices de las instancias ruidosas y los elimino de CT
    if(any(NS[as.integer(rownames(CN))]>0)){
      CT=CT[-match(as.integer(rownames(CN[which(NS[as.integer(rownames(CN))]>0),])),rownames(CT)),]
      fallos=length(which(NS[as.integer(rownames(CN))]>0))
    }else{fallos=0}
    g=ifelse(fallos<=p,g-1,3)
    rownames(CT)=1:nrow(CT)
    iteracion=iteracion+1
    cat("#############################################\n")
    cat("\n\nIteración",iteracion,"Fallos",fallos,"\n")
    cat("Dimensiones del nuevo conjunto",nrow(CT),"\n\n\n")
    cat("#############################################\n")
    if(nrow(CT[CT[,ncol(CT)]==1,])==0){
      cat("!!!!ELIMINADA TODA LA CLASE MINORITARIA!!!!\n")
      return(dataset)
    }
  }
  return(CT)
}
