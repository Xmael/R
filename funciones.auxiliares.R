hotelciudad=function(nombre){
  nombre=str_to_upper(nombre)
  hoteles[which(str_to_upper(hoteles$city)==nombre & hoteles$country_code==codigo),]
}

muestra=function(dataframe,tamaño){
  dataframe[sample(1:nrow(dataframe),size=tamaño),]
}

