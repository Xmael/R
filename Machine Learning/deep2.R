library(h2o)
library(caret)
setwd("/home/ismael/Trabajo")
h2o.init(ip = "127.0.0.1",max_mem_size = "24G",nthreads=8)
db=read.csv("bookings",header=T,sep=";")

db=db[,c(3,4,9,10,11,12,13,15,14)]
db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana= factor(db$Diasemana,levels(db$Diasemana)[c(3,4,5,2,7,1,6)]) #Ordena los niveles de los días por su orden en la semana
db$Mes=factor(db$Mes,levels(db$Mes)[c(4,5,8,1,9,7,6,2,12,11,10,3)])

#Quito valores perdidos en el source
mdb=as.matrix(db)
indices=unlist(sapply(1:nrow(db),function(i){if(any(is.na(mdb[i,]))){return(i)}}))
db=db[-indices,]

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

colnames(db)=c("Diasemana","Mes","Hotel","Source","Personas","Noches","Pais","Ciudad","Proveedor")
load("folds.RData")

i=1 #No cargar esto
train=db[folds[[i]],]
test=db[-folds[[i]],]

train_hex=as.h2o(train, destination_frame = "train")
test_hex=as.h2o(test, destination_frame = "test")


score_test_set=T  #disable if only interested in training throughput

workdir="/home/ismael/Trabajo/"

run <- function(extra_params) {
  str(extra_params)
  print("Training.")
  model <- do.call(h2o.deeplearning, modifyList(list(x=1:(ncol(db)-1), y=ncol(db),
                                                     training_frame=train_hex), extra_params))
  sampleshist <-model@model$scoring_history$"samples"
  samples <- sampleshist[length(sampleshist)]
  time <- model@model$run_time/1000
  print(paste0("training samples: ", samples))
  print(paste0("training time   : ", time, " seconds"))
  print(paste0("training speed  : ", samples/time, " samples/second"))
  
  if (score_test_set) {
    print("Scoring on test set.")
    ## Note: This scores full test set (10,000 rows) - can take time!
    test_error <- h2o.performance(model, test_hex)@metrics$MSE
    print(paste0("test set error  : ", test_error))
  } else {
    test_error <- 1.0
  }
  h2o.rm("dlmodel")
  c(paste(names(extra_params), extra_params, sep = "=", collapse=" "), 
    samples, sprintf("%.3f", time), 
    sprintf("%.3f", samples/time), sprintf("%.3f", test_error))
}

writecsv <- function(results, file) {
  table <- matrix(unlist(results), ncol = 5, byrow = TRUE)
  colnames(table) <- c("parameters", "training samples",
                       "training time", "training speed", "test set error")
  write.csv(table, file.path(workdir,file),row.names=F, quote=T)
}

EPOCHS=.1
args <- list(
  list(hidden=c(64),             epochs=EPOCHS),
  list(hidden=c(128),            epochs=EPOCHS),
  list(hidden=c(256),            epochs=EPOCHS),
  list(hidden=c(512),            epochs=EPOCHS),
  list(hidden=c(1024),           epochs=EPOCHS),
  list(hidden=c(64,64),          epochs=EPOCHS),
  list(hidden=c(128,128),        epochs=EPOCHS),
  list(hidden=c(256,256),        epochs=EPOCHS),
  list(hidden=c(512,512),        epochs=EPOCHS),
  list(hidden=c(1024,1024),      epochs=EPOCHS),
  list(hidden=c(64,64,64),       epochs=EPOCHS),
  list(hidden=c(128,128,128),    epochs=EPOCHS),
  list(hidden=c(256,256,256),    epochs=EPOCHS),
  list(hidden=c(512,512,512),    epochs=EPOCHS),
  list(hidden=c(1024,1024,1024), epochs=EPOCHS)
)

args <- list(
  list(hidden=c(512, 512), epochs=EPOCHS, validation_frame=test_hex),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=60000, 
       score_duty_cycle=1, score_interval=1),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=60000),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=10000),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=1000),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=100),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=100, 
       score_duty_cycle=0, score_interval=10000)
)
writecsv(lapply(args, run), "scoring_overhead.csv")

args <- list(
  list(hidden=c(512, 512), epochs=EPOCHS, adaptive_rate=F)
)


writecsv(lapply(args, run), "adaptive_rate.csv")

args <- list(
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=100,
       train_samples_per_iteration=1000, activation="Rectifier"),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=100,
       train_samples_per_iteration=1000, activation="RectifierWithDropout"),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=100, 
       train_samples_per_iteration=1000, activation="Tanh"),
  
  list(hidden=c(512, 512), epochs=EPOCHS, score_training_samples=100, 
       train_samples_per_iteration=1000, activation="TanhWithDropout")
)
writecsv(lapply(args, run), "activation_function.csv")

writecsv(lapply(args, run), "network_topology.csv")


