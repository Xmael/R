dblite=dbConnect(SQLite(),dbname="precios.sqlite")
db=as.data.frame(dbGetQuery(conn=dblite,"SELECT * FROM precios "))

for(i in 1:ncol(db)){
  db[,i]=as.factor(db[,i])
}

db=as.matrix(db)

preciosminimos=NULL
count=0
while(length(db)!=0){
  count=count+1
  print(count)
  coincidentes=db[,1]==db[1,1] & db[,5]==db[1,5]
  temp=rbind(db[which(coincidentes),])
  preciosminimos=rbind(preciosminimos,temp[which.min(temp[,7]),])
  db=db[-which(coincidentes),]
}
