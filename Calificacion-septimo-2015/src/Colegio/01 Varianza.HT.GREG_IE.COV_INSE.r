#################################################################################################
######## La presente rutina se ejecuta con el objetivo de obtener las  varianza            ######
######## Jackknife para los estimadores de Horvitz-Thompson (HT) y Regresión Generalizado  ######
######## (GREG) a nivel de escuela  una vez se cuenta con la base estudiantes que          ######
######## participaron en  la prueba de saber 3579, para ejecutar la rutina se debe contar  ######
######## con la siguientes variables;                                                      ######

#### Variable Identificacion: 
###  - ENTIDAD,  Nombre (o ID) de la ETC
###  - ID_INST,  Código DANE de la IE
###  - CONSLECT, Consecutivo de la hoja de lectura 

#### Variable Muestreo: 
###  - PESOS.ESTU,  pesos de muestreo para el estudiante.
###  - PESOS.ESTAB, pesos de muestreo para la IE.
###  - EE.MAT,      Error de estimado de muestreo para el promedio de interés 
###  - N.EST,       Número de estudiantes.

#### Variable respuesta: 
###  - PROM.MAT (o PROM.LENG), promedio del grado-área de interés en el año inmediatamente
###                            anterior (en 
###                            nuestro caso el promedio de matemáticas de grado 5)
###  - V_PLAUS, Los valores plausibles que se emplearon para hacer el cálculo del promedio 
###             en el grado de interés.

###  Covariables:  
###  - INSE, Indice socioeconómico de los estudiantes
###  - SEXO, 
###  - ZONA y SECTOR a la cual pertenese la IE
#################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 15-01 - 2016
## Subdirección de Estadística ICFES

###################################################################################################
###################################################################################################
rm(list = ls())
## Definir el directorio de trabajo
dirpath <-"C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/Calificacion-septimo-2015"
## Definir subcarpetas
inpath <- "/input"
outpath <- "/output"
## Fijar el directorio 
setwd(dirpath)
################################
### Libreria necesaria  ########
################################
library(TeachingSampling)
library(reshape2)
library(dplyr)
require(mice)
require(sampling)
#########################################################
### Lectura de la base de datos de estudiantes   ########
#########################################################

ESTUDIANTES <- read.csv(file = "input/Colegio/Base/ESTUDIANTES.SEPTIMO.txt",sep="\t",header=T)
ESTUDIANTES$ID_INST<-as.character(ESTUDIANTES$ID_INST)
ESTUDIANTES <- ESTUDIANTES %>% filter(parM==1)
###########################################################################################################

####################################
## Cálculo de t_x real ##
####################################
tx_ETC <- read.csv(file = "input/Colegio/Base/Tx_censal.txt",sep="\t",header=T)

###########################################################################################################
IE.RESULTADO<- ESTUDIANTES%>%group_by(ENTIDAD,ID_INST)%>%
                                                summarise(PROM.IE=mean(PROM.MAT))
###########################################################################################################
#### Selección de muestra control 
ESTUDIANTES.MUESTRA<- ESTUDIANTES
###########################################################################################################
## Selección de las ETC que pertenencen a la muestra control
id.ETC <- unique(as.character(ESTUDIANTES.MUESTRA$ENTIDAD))
## Función para la estimacióin de la varianza
source(file = "src/Funciones/03 Varianza HT IE.r")
##########################################################################################################

result <- data.frame(
  ID_INST=NA, PROM.greg.ETC=NA, Sd.greg.PROM=NA, PROM.HT.ETC=NA,
  Sd.HT.PROM=NA,  greg.PROM.IE=NA,greg.PROM.sd=NA, greg.JK.sd=NA,
  greg.SD=NA,HT.PROM.IE=NA, HT.PROM.sd=NA,HT.JK.sd=NA, HT.SD=NA)
set.seed(19012016)

for (i in  id.ETC){
  print(i)
  result<-rbind(result,
                E.GREG.IE(BD.ESTUDIANTES = ESTUDIANTES.MUESTRA,
                          V_PLAUS = "PROM.MAT",
                          ETC=i,xk =c("heater_No","heater_Sí"),txk = tx_ETC,
                method="logit",bounds=c(low=0,upp=100)))
}

#############################################################################################################
ls()
rm(list = c("E.GREG.IE","Var.jk.IE","i","id.ETC","tx_ETC"))
ls()
####################################################################################################
##### ESTIMADOR COMPUESTO ######
####################################################################################################
########################
## Caso 1, cuando alhpa < 1
########################
lambda = 1
result$Med.Comp_C1 <- lambda*result$greg.PROM.IE +(1-lambda)*result$HT.PROM.IE
result$Sd.Comp_C1 <- sqrt(lambda*result$greg.SD^2 +(1-lambda)*result$HT.SD^2)
########################
## Caso 2, cuando alhpa = 1
########################
lambda = 1 
result$Med.Comp_C2 <- lambda*result$greg.PROM.IE +(1-lambda)*result$HT.PROM.IE
result$Sd.Comp_C2 <- sqrt(lambda*result$greg.SD^2 +(1-lambda)*result$HT.SD^2)
########################
## Caso 3, cuando alhpa = 1.5
########################
lambda = 1/1.5
result$Med.Comp_C3 <- lambda*result$greg.PROM.IE +(1-lambda)*result$HT.PROM.IE
result$Sd.Comp_C3 <- sqrt(lambda*result$greg.SD^2 +(1-lambda)*result$HT.SD^2)
########################
## Caso 4, cuando alhpa = 2
########################
lambda = 1/2 
result$Med.Comp_C4 <- lambda*result$greg.PROM.IE +(1-lambda)*result$HT.PROM.IE
result$Sd.Comp_C4 <- sqrt(lambda*result$greg.SD^2 +(1-lambda)*result$HT.SD^2)


IE.RESULTADO<-merge(IE.RESULTADO,result, by="ID_INST",all.x = T)

save(IE.RESULTADO, file = file.path("output/IE_2015.PROM.MAT.RData"))


#############################################################################################
## El resultado de la rutina anterior es una base que contiene el siguente conjunto de variables
#### Variable Identificacion: 
###  - ENTIDAD,  Nombre (o ID) de la ETC
###  - ID_INST,  Código DANE de la IE

###  Covariables:  
###  - SEXO, 
###  - ZONA y SECTOR a la cual pertenese la IE
#### Variable respuesta: 
###  - EE.MAT,      Error de estimado de muestreo para el promedio de interés 
###  - Media.greg.ETC, Media.HT: resultado de la estimación por ETC del PROMEDIO del área de interés
###  - greg.PROM.IE, HT.PROM.IE,Med.Comp_C1,Med.Comp_C2, Med.Comp_C3,Med.Comp_C4; promedio por IE estimado 
###  - greg.SD,      HT.SD,     Sd.Comp_C1, Sd.Comp_C2,  Sd.Comp_C3, Sd.Comp_C4, Desviacion Estándar     

## Estás variables son las que a limentan el modelo SAE 
#############################################################################################
