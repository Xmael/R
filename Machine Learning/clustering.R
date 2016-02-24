
library(cluster)
setwd("/home/ismael/Trabajo/")
db=read.csv("bookings",header=T,sep=";")
load("clusters.RData")
db=db[as.Date(db$Fecha)>as.Date("2014-11-04"),]
db=db[,c(2,3,9,10,11,12,13,15,14)]
db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana=factor(db$Diasemana,levels(db$Diasemana)[c(3,4,5,2,7,1,6)])
db$Mes=factor(db$Mes,levels(db$Mes)[c(4,5,8,1,9,7,6,2,12,11,10,3)])


#Quito valores perdidos en el source
mdb=as.matrix(db)
indices=unlist(sapply(1:nrow(db),function(i){if(any(is.na(mdb[i,]))){return(i)}}))
db=db[-indices,]

indices=which(db$Proveedor==c("EH") | db$Proveedor==c("ST"))

db=db[-indices,]

#Elimino los niveles que están vacíos
db$Proveedor=droplevels(db$Proveedor)
niveles=levels(db$Proveedor)
levels(db$Proveedor)=c(1:4) #Los cambio a valores numéricos

for(j in 1:(ncol(db)-1)){
  if(class(db[,j])=="factor"){
    db[,j]=as.integer(db[,j])
  }
}

rownames(db)=1:nrow(db)


dbc=db[sample(1:nrow(db))[1:10000],-ncol(db)]
distancias=dist(dbc)
jerarquico=hclust(d = distancias,method = "average")
plot(jerarquico)

clusters=kmeans(db[,-ncol(db)],centers = 6)
h=sample(1:nrow(db))[1:20000]
distancias=dist(db[h,-ncol(db)])
siluetas=silhouette(x=clusters$cluster[h],dist = distancias)
summary(siluetas)

clusters=clusters$cluster
save(clusters,file="clusters.RData")
