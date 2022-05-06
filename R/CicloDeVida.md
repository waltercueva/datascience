#Recolección de Datos
library(datasets)
library(hflights)
hflights::hflights
View(hflights)
datos=hflights
############################Preprocesamiento

#1.-verificando valores NA
antes=NROW(datos$ArrDelay)
sum(is.na(datos$ArrDelay))

limpiados=datos$ArrDelay[!is.na(datos$ArrDelay)]#Selecciona los no NA


despues=NROW(limpiados)
antes-despues
promArrDelay=mean(limpiados)
promArrDelay
#imputar
datos$ArrDelay[is.na((datos$ArrDelay))]=promArrDelay

mean(datos$ArrDelay)

#comprobamos
sum(is.na(datos$ArrDelay))

##########
sum(is.na(datos$UniqueCarrier))
unique(datos$UniqueCarrier)

#tarea: Implementar una funcion para imputar los campos con valores numericos con el promedio o mediana

imputar<-function(columna, tipo){
  limpiados=columna[!is.na(columna)]#Selecciona los no NA
  #imputar
  if(tipo==1){
    prom=mean(limpiados)
    columna[is.na((columna))]=prom  
  }
  if(tipo==2){
    mediana=median(limpiados)
    columna[is.na((columna))]=mediana
  }
  #comprobamos
  return (columna)
}

#datos=hflights
#sum(is.na((datos$ArrTime)))
#datos$ArrTime=imputar(datos$ArrTime,2)
#median(datos$ArrTime)

###########################Transformacion
library(dplyr)
select(datos,Year,Month)

dim(datos)
salida=datos %>% select(Year,Month)

datos %>%select(DepTime,AirTime) %>%head(20)

datos %>%select(DepTime,AirTime, contains("Cancel")) %>%head(10)
head(datos)
datos %>%select(DayOfWeek, DepTime,AirTime) %>% filter(DayOfWeek==1)%>% head(5)


head(datos$DepTime)
head(datos$AirTime)
dim(salida)
NROW()

############################Modelos

############################Visualizacion
