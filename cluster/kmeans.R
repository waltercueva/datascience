library(ggplot2)
datos<-read.csv("datos.txt",sep = " ",col.names = c("x","y"))
View(datos)

#determinar el K(cluster)-experimentacion
k=3
cont_iter=0 #contador de iteraciones
max_iter=10
NROW(datos)
#centroide =datos[sample(NROW(datos),k),] #eleccion aleatoria

centroide=datos[1:k,] #eleccion de los 3 primeros
puntos=c(0,1,2)
centroide=data.frame(puntos,centroide)
dfCentroides=cbind(puntos,centroide)
tmp<-matrix(0,NROW(datos),ncol = k)

matrix(0,NROW(centroide),ncol = ncol(datos))

#graficando los cluster
grafica<-ggplot(datos,aes(x,y))+geom_point(size=1)+geom_point(data = centroide,color=2:4, size=3)
ggsave(grafica, file="gr.png",width = 48, height =27, units = "cm")

#iterar
while(cont_iter<max_iter){
  cont_iter=cont_iter+1
  #calcular las distancias euclidianas
  for(j in 1:k){
    for(i in 1:nrow(datos)){
      tmp[i,j]=sqrt(sum((datos[i,1:ncol(datos)]-centroide[j,1:ncol(centroide)])^2))
    }  
  }
  
}



