ensemble=mclapply(1:100,function(i){
  model=randomForest(Proveedor~.,data = train,ntree=100,do.trace=T,subset=sample(1:nrow(train))[1:40000]) #100 40000
  pred.random=predict(model,test[,-ncol(test)],type="prob")
  ganadores=apply(pred.random,1,which.max)
  metrica=confusionMatrix(ganadores,test$Proveedor)
  return(list(pred.random,metrica$overall[1]))
},mc.preschedule = TRUE, mc.set.seed = TRUE,
mc.silent = FALSE, mc.cores = getOption("mc.cores", 6L),
mc.cleanup = TRUE, mc.allow.recursive = TRUE)

mean(sapply(1:100,function(i) {ensemble[[i]][[2]]}))
pred.random.ensemble=cbind(sapply(1:100,function(i) {ensemble[[i]][[1]]}))

pred.random.ensemble=matrix(0,nrow=nrow(test),ncol=4)
for(i in 1:100){
  pred.random.ensemble=pred.random.ensemble+ensemble[[i]][[1]]
}

pred.random.ensemble=pred.random.ensemble/100
ganadores=apply(pred.random.ensemble,1,which.max)
metrica=confusionMatrix(ganadores,test$Proveedor)


0.7829 