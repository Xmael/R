load(file="tourico.RData")

con <- dbConnect(MySQL(), user="root", password="papidorl", 
                 dbname="hoteles", host="localhost",client.flag=CLIENT_MULTI_STATEMENTS)

dbListTables(con)
dbWriteTable(con, name='tourico', value=hoteles)

mapped_hotels=dbGetQuery(con, "select * from mapped_hotels")

mapped_hotels=read.csv2(file = "/home/ismael/Trabajo/Hoteles_temporal/mapped_hotels.csv",sep = ";",header=T,stringsAsFactors = F)

load("/home/ismael/Trivago/tourico.RData")
tourico=hoteles
load("/home/ismael/Trivago/hoteles(25.1.16).RData")
colnames(hoteles)[35]="timezone"
colnames(hoteles)[36]="dst"
hoteles=rbind(hoteles,tourico)
faltan=hoteles[-which(hoteles$id %in% mapped_hotels$id),]
