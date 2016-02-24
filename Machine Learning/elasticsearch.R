connect(es_base="http://52.28.187.57")
prueba=Search(index = "booking",type = "booking",size=10,fields="device",q="*")$hits$hits
apache=mapeo$`logstash-2015.11.01`$mappings$`apache-access`

today=Sys.Date()
periodo=seq(today, length.out=90, by="-1 day") #Obtengo las fechas desde hoy hasta diez días atrás
periodo=format(periodo,"%Y.%m.%d") #Convierto las fechas a la cadena de logstash

db=list()
for(fecha in periodo){
  indice=paste("logstash-",fecha,sep="")
  db=c(db,Search(index = indice,type = "apache-access",size=1000,fields="device",q="*")$hits$hits)
}

extrae=sapply(1:length(db),function(i){
  c(unlist(db[[i]]$fields$device))
})

df=data.frame(table(extrae))
df=df[df[,1]!="Other",]
df[,2]=df[,2]/sum(df[,2])
df=df[df[,2]>0.005,]
ggplot(df,aes(x=extrae,y=Freq))+geom_bar(stat="identity",colour="blue",fill="blue")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
