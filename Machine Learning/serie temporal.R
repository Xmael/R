library(forecast)
library(tseries)
library(ggplot2)

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

for(i in 1:(ncol(db)-1)){
  if(class(db[,i])=="factor"){
    db[,i]=as.integer(db[,i])
  }
}


st=as.data.frame(table(db$Fecha))
st=st[600:nrow(st),]

#Quitamos valores anómalos
for (i in 1:nrow(st)){
  if(st$Freq[i]<20){
    st$Freq[i]=round(mean(c(mean(sapply(1:3,{function(j) st$Freq[i-j]})),mean(sapply(1:3,{function(j) st$Freq[i+j]})))))
  }
}

serie.ts=ts((st$Freq),freq=7)
plot(decompose(serie))

NPred<-7
NTest<-7

serieTr<-serie.ts[1:(length(serie.ts)-NTest)] #Serie de entrenamiento
tiempoTr<- 1:length(serieTr) #Periodo de tiempo para la serie de entrenamiento

serieTs<-serie.ts[(length(serieTr)+1):length(serie.ts)]
tiempoTs<-(length(tiempoTr)+1):(length(tiempoTr)+NTest)

#Dibujamos la serie, tanto la de entrenamiento como la de test (rojo)
plot.ts(serieTr,xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col="red")


parametros <- lm (serieTr ~ tiempoTr)

# Calculamos la estimación de la tendencia
TendEstimadaTr<-parametros$coefficients[1]+(tiempoTr)*parametros$coefficients[2]
TendEstimadaTs<-parametros$coefficients[1]+(tiempoTs)*parametros$coefficients[2]

series<-matrix(c(t(serieTr), t(TendEstimadaTr)), ncol=2); # Mostramos resultado
matplot(series, pch=1, type= "l")
lines(tiempoTs,serieTs,col="red")
lines(tiempoTs, TendEstimadaTs,col="darkgreen")

SerSinTendTr<-serieTr-TendEstimadaTr
SerSinTendTs<-serieTs-TendEstimadaTs
plot.ts(SerSinTendTr)
lines(tiempoTs,SerSinTendTs,col="red")
lines(tiempoTs,SerSinTendTs,col="red")

jarque.bera.test(parametros$residuals) #Tiene que ser mayor que 0.05
jarque.bera.test(TendEstimadaTs-serieTs) #Tiene que ser mayor que 0.05
t.test(c(parametros$residuals,TendEstimadaTs-serieTs)) #Tiene que ser mayor que 0.05


#Probamos con filtrado
k=100
filtro<-rep(1/k, k); # Creamos el filtro

# Filtramos señal
SerFiltradaTr<-filter(serieTr,filter=filtro,sides=2,method="convolution")


#Dibujamos la serie con ggplot
compara(SerFiltradaTr,serieTr,"Filtrada","Original","Filtrada y Original")
compara(c(scale(na.omit(serieTr-SerFiltradaTr),center=T,scale=F),rep(NA,k)),scale(SerSinTendTr,center=T,scale=F),"Filtrada","Modelada","Comparación")

#Le quitamos la tendencia
SerSinTendTr<-serieTr-SerFiltradaTr 
pintasolo(SerSinTendTr,"Serie sin la tendencia")
acf(na.omit(SerSinTendTr)) # Mostramos ACF del filtrado
pacf(na.omit(SerSinTendTr)) # Mostramos PACF

jarque.bera.test(na.omit(serieTr-SerSinTendTr))

k<-7
estacionalidad<-decompose(serie)$seasonal[1:k]
aux<-rep(estacionalidad,length(serieTr)/length(estacionalidad))
SerSinTendEstTr<-SerSinTendTr-aux
SerSinTendEstTs<-SerSinTendTs-estacionalidad[1:7]

plot.ts(SerSinTendEstTr,xlim=c(1,tiempoTr[length(tiempoTr)]))
series<-matrix(c(t(SerSinTendEstTr), t(SerSinTendTr)), ncol=2); # Mostramos resultado
matplot(series, pch=1, type= "l")
acf(na.omit(SerSinTendEstTr))
pacf(na.omit(SerSinTendEstTr))


auto.arima(SerSinTendEstTr,stationary = F, seasonal = F)
modelo<-arima(SerSinTendEstTr,order=c(6,0,0))
valoresAjustados<-SerSinTendEstTr+modelo$residuals
predicciones<-predict(modelo,n.ahead=7)
valoresPredichos<-predicciones$pred


errorTr<-sum(modelo$residuals^2)
errorTs<-sum(valoresPredichos-SerSinTendEstTs)^2

plot.ts(SerSinTendEstTr,xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados,col="blue")
lines(tiempoTs,SerSinTendEstTs,col="red")
lines(tiempoTs,valoresPredichos,col="green")

#Tes para la selección del modelo y su validación. Comprobamos primeramente la aleatoriedad
Box.test(modelo$residuals)

#Ahora vamos a ver si los errores se distribuyen como una normal
jarque.bera.test(na.omit(modelo$residuals))#podemos asumir la normalidad de los residuos

#Test de normalidad Shapiro-Wilk
shapiro.test(modelo$residuals)#también podemos asumir la normalidad


#cogemos toda la serie
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
modelo<-arima(serieSinTendEst,order=c(6,0,0))
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
