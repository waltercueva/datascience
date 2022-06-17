library(ggplot2)
#Generacion de datos sinteticos
tallas<-sample(160:195,100, replace = TRUE)
pesos<-sample(70:130,100, replace = TRUE)
dt<- data.frame(tallas,pesos)
etiquetar<-function(tallas, n){
  #etiquetando
  tipo<-c()
  for(i in 1:n){
    if(tallas[i]>=160&tallas[i]<=170){
      tipo<-c(tipo,'A')
    }
    else if(tallas[i]>170&tallas[i]<=180){
      tipo<-c(tipo,'B')
    }
    else tipo<-c(tipo,'C')
  }
  dt<-cbind(dt,tipo)
  return (dt)
}
dt<-etiquetar(tallas,NROW(tallas))

ggplot(dt,aes(tallas,pesos,color=tipo, shape=tipo))+geom_point()

#implementar el knn

#valores referenciales
newX=175
newY=105
k=5# cantidad de vecinos

knn<- function(dt, newX, newY, k){
  a<-dt$tallas-newX
  b<-dt$pesos-newY
  de<-sqrt(a^2+b^2)#distancia euclididiana con todos los puntos
  dt<-cbind(dt,de)
  tmp<-sort(de)[1:k]
  ds1=dt[dt$de %in% tmp,  ]#devuelve todas las columnas que cumplan con la condicion
  return (ds1$tipo)
}

knn(dt,170,110,7)

#Tarea:
* Devolver el % por tipo(clase)
* Etiquetar por rangos
