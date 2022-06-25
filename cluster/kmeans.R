library(ggplot2)
datos<-read.csv("datos.txt",sep = " ",col.names = c("x","y"))

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
id=matrix(0,NROW(datos),ncol = 1)
#graficando los cluster
grafica<-ggplot(datos,aes(x,y))+geom_point(size=1)+geom_point(data = centroide,color=2:4, size=3)
ggsave(grafica, file="gr.png",width = 48, height =27, units = "cm")

centroide=centroide[,-1]
#iterar
while(cont_iter<max_iter){
  cont_iter=cont_iter+1
  #calcular las distancias euclidianas
  for(j in 1:k){
    for(i in 1:nrow(datos)){
      tmp[i,j]=sqrt(sum((datos[i,]-centroide[j,])^2))
    }  
  }
  #asignación al cluster correspondiente
  for(i in 1:NROW(tmp)){
    if(tmp[i,1]<tmp[i,2]&tmp[i,1]<tmp[i,3])
      id[i]=0
    else if(tmp[i,2]<tmp[i,1]&tmp[i,2]<tmp[i,3])
      id[i]=1
    else
      id[i]=2
    
    #c[i]<-(which(tmp[1:10,]==min(tmp[1:10,]),arr.ind=T))
    
  }
  #calcular el nuevo centroide
  
  datos1=cbind(puntos=id,datos)
  
  #old=centroide
  for(i in 1:k){
    centroide[i,1]=mean(datos[id==(i-1),1])
    centroide[i,2]=mean(datos[id==(i-1),2])  
  }
  nombre<-paste("gr",cont_iter,".png",sep="")
  centroideX=data.frame(centroide,puntos)
  final<-merge(datos1,centroideX,by = "puntos")
  colnames(final)<-c("puntos","ax","ay","bx","by")
  grafica<-ggplot(final,aes(ax,ay,color=puntos))+geom_point(aes(bx,by),size=1)+
    geom_point(aes(x=bx,y=by),size=5)+geom_segment(aes(x=ax,y=ay,xend=bx,yend=by))
  ggsave(grafica, file=nombre,width = 48, height =27, units = "cm")
  
}
