test=read.csv("test.csv",header=T)
histograma=as.data.frame(table(ttest$Hotel))
indices=which(histograma[,2]==1)
pred=(t(apply(pred,1,function(pred){order(pred,decreasing=T)}))[,1])
fallos=NULL
aciertos=NULL
for(i in 1:nrow(test)){
  if(pred[i]!=test$Proveedor[i]){
    fallos=c(fallos,i)
    }else{aciertos=c(aciertos,i)}
}

tablafallos=as.data.frame(table(test$Hotel[fallos]))
tablaaciertos=as.data.frame(table(test$Hotel[aciertos]))

ggplot(as.data.frame(table(tablaaciertos[,2])/length(aciertos)),aes(x=Var1,y=Freq,fill=Var1))+
  geom_bar(stat="identity")+scale_y_continuous(breaks=seq(0, 0.5, 0.1))+
  xlab("Frecuencias de aparición")+ylab("Porcentaje de acierto sobre el total")

ggplot(as.data.frame(table(tablafallos[,2])/length(fallos)),aes(x=Var1,y=Freq,fill=Var1))+
  geom_bar(stat="identity")+scale_y_continuous(breaks=seq(0, 0.8, 0.1))+
  xlab("Frecuencias de aparición")+ylab("Porcentaje de error sobre el total")

tablattest=as.data.frame(table(ttest$Hotel))
tablaaciertos[,2]/tablattest[match(tablaaciertos[,1],tablattest[,1]),2] #Aquí obtengo que las reservas se han hecho a 5113 hoteles distintos
tablacomparaciona=data.frame(tablattest[match(tablaaciertos[,1],tablattest[,1]),1],
                            as.data.frame(tablattest[match(tablaaciertos[,1],tablattest[,1]),2]),
                            tablaaciertos[,2]/tablattest[match(tablaaciertos[,1],tablattest[,1]),2]) #

colnames(tablacomparaciona)=c("Identificadores","Reservas","Acierto Relativo") 

reservas=unique(tablacomparaciona$Reservas)
porcentajes=NULL
for(i in reservas){
  porcentajes=rbind(porcentajes,cbind(i,mean(tablacomparaciona$`Acierto Relativo`[tablacomparaciona$Reservas==i])))
}
porcentajes=porcentajes[order(porcentajes[,1]),]
porcentajes[1,]=c(1,as.data.frame(table(tablaaciertos[,2]))[1,2]/as.data.frame(table(tablattest[,2]))[1,2])


ggplot(as.data.frame(porcentajes),aes(x=i,y=V2,fill=V2))+
  geom_bar(stat="identity")+scale_y_continuous(breaks=seq(0, 1, 0.1))+
  xlab("Frecuencias de aparición")+ylab("Porcentaje de acierto")

hoteles=read.csv("hoteles.csv",header=T,sep=";")
hoteles$provs=as.character(hoteles$provs)
hoteles=cbind(hoteles[,2],sapply(1:nrow(hoteles),function(i){length(unique(unlist(strsplit(hoteles$provs[i],","))))}))


colnames(tablaaciertos)=c("Hoteles","Freq")
colnames(hoteles)=c("Hoteles","Proveedores")
h.aciertos=merge(tablaaciertos,hoteles)

colnames(tablafallos)=c("Hoteles","Freq")

h.fallos=merge(tablafallos,hoteles)

temp=as.data.frame(table(h.fallos$Proveedores[h.fallos$Freq==2]))
#temp[,2]=temp[,2]/sum(temp[,2])
ggplot(temp,aes(x=Var1,y=Freq,fill=Var1))+
  geom_bar(stat="identity")+
  xlab("Número de proveedores")+ylab("Porcentaje de error para frecuencia 2") #+scale_y_continuous(breaks=seq(0, 1, 0.1))+

temp=as.data.frame(table(h.aciertos$Proveedores[h.aciertos$Freq==2]))
temp[,2]=temp[,2]/sum(temp[,2])
ggplot(temp,aes(x=Var1,y=Freq,fill=Var1))+
  geom_bar(stat="identity")+scale_y_continuous(breaks=seq(0, 1, 0.1))+
  xlab("Número de proveedores")+ylab("Porcentaje de acierto para frecuencia 2")

colnames(tablattest)=colnames(tablafallos)
tablattest=merge(tablattest,hoteles)
temp=as.data.frame(table(h.fallos$Freq[h.fallos$Proveedores==3]))
temp.1=as.data.frame(table(tablattest$Freq[tablattest$Proveedores==3]))
temp[,2]=temp[,2]/temp.1[match(temp[,1],temp.1[,1]),2]
ggplot(temp,aes(x=Var1,y=Freq,fill=Var1))+
  geom_bar(stat="identity")+
  xlab("Frecuencia")+ylab("Porcentaje de acierto")
