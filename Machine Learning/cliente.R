con <- socketConnection(host="127.0.0.1", port = 6011, blocking=TRUE,server=FALSE, open="r+")

for(i in 1:2000){
  temp=test[i,-ncol(test)]
  for(j in 12:15){
    temp[,j]=as.integer(levels(temp[,j]))[temp[,j]]
  }
  writeLines(as.character(temp),con)
}
