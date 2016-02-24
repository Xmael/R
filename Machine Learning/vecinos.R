source("gower.R")
train=read.csv("train.csv",header=T)
test=read.csv("test")
for(i in c(1,2,3,4,7,8,9)){train[,i]=as.factor(train[,i])}
vecinos=gower(5,train)

write(vecinos,"vecinos.RData")