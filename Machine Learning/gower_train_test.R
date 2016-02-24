gower=function(k,train,test){
  library(parallel)
  nominales=NULL
  for(i in 1:ncol(train)){if(is.factor(train[,i])){nominales=c(nominales,i)}}
  cualitativos=setdiff(1:ncol(train),nominales)
  distancia=NULL
  rangos=apply(train[,cualitativos],2,range)[2,]
  Sij=NULL
  
  train=as.matrix(sapply(1:ncol(train),function(i){
    if(i %in% nominales){
      as.numeric(levels(train[,i]))[train[,i]]
      }else{train[,i]}}))
  
  knn=mclapply(1:nrow(test),function(i){
    cat(i,"\n")
    Sij=rep(times=nrow(test),x=NA)
    indices=setdiff(as.integer(rownames(train)),i)
    for(j in indices){
        sij=sapply(nominales, function(k){ifelse(train[j,k]==test[i,k],1,0)})
        sij=c(sij,as.numeric(1-abs(train[j,cualitativos]-test[i,cualitativos])/rangos))
        Sij[j]=mean(sij)
    }
    return(order(Sij,decreasing=T)[1:k])
  },mc.preschedule = TRUE, mc.set.seed = TRUE,
  mc.silent = FALSE, mc.cores = getOption("mc.cores", 8L),
  mc.cleanup = TRUE, mc.allow.recursive = TRUE)
  
  knn=matrix(unlist(knn),nrow=length(knn), byrow=T)
  return(knn)
}