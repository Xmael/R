library(e1071)
library(caret)

train=read.csv("train.csv",header=T,sep=",")
test=read.csv("test.csv",header=T,sep=",")
train$Proveedor=as.factor(train$Proveedor)

tuneparams<- tune(svm, Proveedor~., data = train[sample(1:nrow(train))[1:5000],],
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4),kernel="sigmoid"),
            tunecontrol = tune.control(sampling = "fix"))

train=db[folds[[i]],]
test=db[-folds[[i]],]
model <- svm (Proveedor~., data=train[sample(1:nrow(train))[1:20000],], probability=TRUE, kernel="linear") 
prediction<-predict(model,newdata=test,probability=TRUE) 
pred<-as.data.frame(attr(prediction,"probabilities")) 
ganadores=apply(pred,1,which.max)
confusionMatrix(ganadores,test$Proveedor)
