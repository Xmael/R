setwd("/home/ismael/Trabajo")
library(forecast)
library(chron)

load("numero_reservas.RData")
db=read.csv("bookings",header=T,sep=";")

#db=db[!db$Proveedor=="",] #Quitamos las entradas que tienen el campo proveedor vacío
db$Diasemana= factor(db$Diasemana,levels(db$Diasemana)[c(3,4,5,2,7,1,6)]) #Ordena los niveles de los días por su orden en la semana
db$Mes=factor(db$Mes,levels(db$Mes)[c(4,5,8,1,9,7,6,2,12,11,10,3)])
#db=db[,c(3,4,5,6,9,10,11,12,13,15,17,18,14)]

#Quito valores perdidos en el source
# mdb=as.matrix(db)
# indices=unlist(sapply(1:nrow(db),function(i){if(any(is.na(mdb[i,]))){return(i)}}))
# db=db[-indices,]

db=db[!is.na(db$Proveedor),]
db=db[!db$Proveedor=="",]
db=db[!db$Proveedor=="0",]
db=db[!db$Proveedor=="BO",]
db=db[!db$Proveedor=="TO",]

db$Proveedor=droplevels(db$Proveedor)
niveles=levels(db$Proveedor)
levels(db$Proveedor)=c(1:length(niveles))
db$Fecha=as.Date(db$Fecha)

for(i in 1:(ncol(db)-1)){
  if(class(db[,i])=="factor"){
    db[,i]=as.integer(db[,i])
  }
}


st=as.data.frame(table(db$Fecha))
st$Var1=as.Date(st$Var1)
st=st[match(as.Date("2015-06-01"),st$Var1):match(as.Date("2015-12-9"),st$Var1),]

k=10
filtro<-rep(1/k, k); # Creamos el filtro

filtrado=data.frame(st$Freq,filter(st$Freq,filter=filtro,sides=2,method="convolution"))
for (i in 1:nrow(st)){
  if (abs(filtrado[i,1]-filtrado[i,2])>0.4*filtrado[i,1] & !is.na(abs(filtrado[i,1]-filtrado[i,2])>0.2*filtrado[i,1])){
    st$Freq[i]=round(filtrado[i,2])
  }
}

#Para actualizar manualmente las reservas
reservas=rbind(reservas,data.frame(Fecha=as.Date(reservas[nrow(reservas),1]+1),Reservas=111))
rownames(reservas)=1:nrow(reservas)
save(reservas,file="numero_reservas.RData")

#Evolución del número de reservas desde julio
#temp=st[as.Date(st[,1])>as.Date("2015-08-31"),]

today=Sys.Date()
temp=reservas

etiquetas=paste(days(temp$Fecha),months(as.Date(temp$Fecha),abbreviate = T),sep=".")
fechas=ifelse(temp$Reservas>300 | temp$Reservas<100,etiquetas,"")

ggplot(temp,aes(x=Fecha,y=Reservas,group=1))+geom_point(size=4,col="blue")+theme( axis.text.x  = element_blank())+
  geom_text(aes(label=fechas),hjust=-0.2, vjust=0.2,angle=45,size=5,col="darkgreen")+geom_line(size=2,col="blue")+ylim(c(0,350))

today=Sys.Date()
temp=temp[match(as.Date("2015-08-31"),temp$Fecha):match(today-1,temp$Fecha),]
serie=ts((temp$Reservas),freq=7)

#Voy a utilizar los valores originales para comparar
# st2=as.data.frame(table(db$Fecha))
# st2$Var1=as.Date(st2$Var1)
# st2=st2[match(as.Date("2015-08-31"),st2$Var1):match(as.Date("2015-12-06"),st2$Var1),]
# serie=ts((st2$Freq),freq=7)

plot(decompose(serie))

NTest=7

serieTr<-serie[1:(length(serie)-NTest)] #Serie de entrenamiento
tiempoTr<- 1:length(serieTr) #Periodo de tiempo para la serie de entrenamiento

serieTs<-serie[(length(serieTr)+1):length(serie)]
tiempoTs<-(length(tiempoTr)+1):(length(tiempoTr)+NTest)


fit=nnetar(serieTr,p=7,P=7,size=28,repeats=100)
SerPred=forecast(fit,h=7)
plot.forecast(SerPred)
accuracy(SerPred,serieTs)
data.frame(serieTs,SerPred$mean)
data.frame(serieTr)

fit=nnetar(serie,p=7,P=7,size=28,repeats=100)
SerPred=forecast(fit,h=3)
plot.forecast(SerPred)
SerPred


plot(SerPred$fitted, pch=1, type= "l")
lines(tiempoTs,serieTs,col="red")
lines(tiempoTs, TendEstimadaTs,col="darkgreen")
