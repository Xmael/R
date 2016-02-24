for(i in 1:ncol(hoteles)){
  hoteles[which(hoteles[,i]=="NULL"),i]=NA
}


for(k in 1:nrow(countries)){
  codigo=as.character(countries$country_code[k])
  cat(codigo,countries$country[k],"\n")
  temp1=unique(hoteles$country[which(hoteles$country_code==codigo)])
  temp1=temp1[-match(str_to_upper(countries$country[countries$country_code==codigo]),temp1)]
  if(length(temp1)!=0){
    for(i in 1:length(temp1)){
      cat(unique(hoteles$city[which(hoteles$country_code==codigo & hoteles$country==temp1[i])]),", es de ",temp1[i],"?\n")
      lee=str_to_upper(readline(prompt = "Correcto/Nombre país nuevo: "))
      if(lee==""){
        hoteles$country[which(hoteles$country_code==codigo & hoteles$country==temp1[i])]=temp1[i]
        hoteles$country_code[which(hoteles$country_code==codigo & hoteles$country==temp1[i])]=countries$country_code[str_to_upper(countries$country)==temp1[i]]
      }else if(lee!=""){
        hoteles$country[which(hoteles$country_code==codigo & hoteles$country==temp1[i])]=lee
        hoteles$country_code[which(hoteles$country_code==codigo & hoteles$country==temp1[i])]=countries$country_code[str_to_upper(countries$country)==lee]
      }
    }
  }
}