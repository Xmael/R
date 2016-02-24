library(forecast)
library(chron)
library(ggplot2)

load("numero_reservas.RData")

#Para actualizar manualmente las reservas
 reservas=rbind(reservas,data.frame(Fecha=as.Date(reservas[nrow(reservas),1]+1),Reservas=149))
 rownames(reservas)=1:nrow(reservas)
 save(reservas,file="numero_reservas.RData")

today=Sys.Date()
temp=reservas

etiquetas=paste(days(temp$Fecha),months(as.Date(temp$Fecha),abbreviate = T),sep=".")
fechas=ifelse(temp$Reservas>300 | temp$Reservas<100,etiquetas,"")

ggplot(temp,aes(x=Fecha,y=Reservas,group=1))+geom_point(size=4,col="blue")+theme( axis.text.x  = element_blank())+
  geom_text(aes(label=fechas),hjust=-0.2, vjust=0.2,angle=45,size=5,col="darkgreen")+geom_line(size=2,col="blue")+ylim(c(0,350))

today=Sys.Date()
temp=temp[match(as.Date("2015-08-31"),temp$Fecha):match(today-1,temp$Fecha),]
serie=ts((temp$Reservas),freq=7)

plot(decompose(serie))

NTest=7

serieTr<-serie[1:(length(serie)-NTest)] #Serie de entrenamiento
tiempoTr<- 1:length(serieTr) #Periodo de tiempo para la serie de entrenamiento

serieTs<-serie[(length(serieTr)+1):length(serie)]
tiempoTs<-(length(tiempoTr)+1):(length(tiempoTr)+NTest)

#Modelo1
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

#Modelo2
serieEntera=serie
tiempo<-1:length(serieEntera)
parametros<-lm(serieEntera~tiempo)#Ajustamos modelo de tendencia
TendEstimada<-parametros$coefficients[1]+tiempo*parametros$coefficients[2]
serieSinTend<-serieEntera-TendEstimada
aux<-ts(serieEntera,frequency=7)
aux<-decompose(aux)$seasonal
estacionalidad<-as.numeric(aux[1:7])
aux<-rep(estacionalidad,length(serieSinTend)/length(estacionalidad))
serieSinTendEst<-serieSinTend-aux
modelo<-arima(serieSinTendEst,order=c(1,0,1))
valoresAjustados<-serieSinTendEst+modelo$residuals
Predicciones<-predict(modelo,n.ahead=7)
valoresPredichos<-Predicciones$pred

#Deshacemos los cambios
valoresAjustados<-valoresAjustados+aux
valoresPredichos<-valoresPredichos+estacionalidad

valoresAjustados<-valoresAjustados+TendEstimada
tiempoPred<-tiempo[length(tiempo)]+(1:7)
TendEstimadaPred<-parametros$coefficients[1]+tiempoPred*parametros$coefficients[2]
valoresPredichos<-valoresPredichos+TendEstimadaPred


plot.ts(serie)
lines(valoresAjustados,col="blue")