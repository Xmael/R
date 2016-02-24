library(xgboost)
library(randomForest)
library(parallel)
library(h2o)
load("randomforestmodel.RData")
load("xgboost.model.RData")
load("deep.model.RData")
load("niveles.RData")
setMKLthreads(1)

h2o.init(ip = "127.0.0.1",max_mem_size = "24G",nthreads=7)
deep=h2o.loadModel("/home/ismael/Trabajo/DeepLearning_model_R_1449829298507_5/")

con <- socketConnection(host="127.0.0.1", port = 6011, blocking=TRUE,server=TRUE, open="r+")
predice=function(i,test){
  if(i==1){
    modelo="XGBOOST"
    pred=predict(bst,data.matrix(as.data.frame(t(as.numeric(test)))))
  }else if(i==2){
    modelo="DEEP"
    for(i in 1:ncol(test)){
      test[,i]=as.factor(test[,i])
      levels(test[,i])=c(as.integer(levels(test[,i]))[test[,i]],setdiff(niveles[[i]],as.integer(levels(test[,i]))[test[,i]]))
    }
    test=as.h2o(test,destination_frame = "test")
    pred=as.data.frame(predict(deep,test))[,-1]

  }else if(i==3){
    modelo="RF"
    for(j in 12:15){test[,j]=as.factor(test[,j])}
    levels(test$RS)=c(as.integer(levels(test$RS))[test$RS],setdiff(c(0,1),as.integer(levels(test$RS))[test$RS]))
    levels(test$HB)=c(as.integer(levels(test$HB))[test$HB],setdiff(c(0,1),as.integer(levels(test$HB))[test$HB]))
    levels(test$SH)=c(as.integer(levels(test$SH))[test$SH],setdiff(c(0,1),as.integer(levels(test$SH))[test$SH]))
    levels(test$GT)=c(as.integer(levels(test$GT))[test$GT],setdiff(c(0,1),as.integer(levels(test$GT))[test$GT]))
    pred=predict(model,test,type="prob")
  }
  list(test,pred,modelo)
}

ptm <- proc.time()
for(i in 1:2000){
  test=(readLines(con,15))
  test=as.data.frame(t(as.integer(test)))
  colnames(test)=c("Ciudad","CodCiudad","PaisHotel","CodPais","Hotel","CodHotel","Diasemanac","Mesc","Source","Personas","Noches",
                   "RS","HB","SH","GT")

   pred=lapply(1, function(j) {predice(j,test)})
  
  cat(i,as.numeric(pred[[1]][[2]]),"\n")
  
  if(max(pred[[1]][[2]])<0.7){
    pred.2=lapply(c(2,3), function(j) {predice(j,test)})#, mc.preschedule = FALSE, mc.set.seed = TRUE,
#                   mc.silent = FALSE, mc.cores = getOption("mc.cores", 1L),
#                   mc.cleanup = TRUE, mc.allow.recursive = FALSE)
    cat(i,"###ENSEMBLE###",(as.numeric(pred[[1]][[2]])+as.numeric(pred.2[[1]][[2]])+as.numeric(pred.2[[2]][[2]]))/3,"\n")
  }

  
}
proc.time() - ptm
