###########################################################################################################
######## Paso previo "02 Seleccion de modelo SAE.r"                                                  ######
######## En el paso anterior se selecciono el modelo SAE que minimiza el ECM cuando se hacen         ######
######## predicciones, acontinuación de muestran los diferentes resultados para cada uno de los      ######
######## estimadores  empleados (HT, GREG, COMPUESTO)                                                ######                       ######
###########################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 26-01 - 2016
## Subdirección de Estadística ICFES
###################################################################################################
rm(list = ls())
## Definir el directorio de trabajo
dirpath <-"C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/Calificacion-septimo-2015/"
## Definir subcarpetas
inpath <- "/input"
outpath <- "/output"
## Fijar el directorio 
setwd(dirpath)

####################################################   
############# Librerias #############
####################################################
library(TeachingSampling)
library(lme4)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(sae)
library(dplyr)
require(mice)
###########################################################################################################
## Lectura de la base de datos de las IE con sus respectivas covariables 
# load("output/Colegios/Model SAE/IE_2015.PROM.COMPETENCIA_SAE.RData")
# load("output/Colegios/Model SAE/IE_2015.PROM.MATEMATICAS_SAE.RData")
# load("output/Colegios/Model SAE/IE_2015.PROM.LENGUAJE_SAE.RData")
load("output/Colegios/Model SAE/IE_2015.PROM.CIENCIAS_SAE.RData")
## Clasificar la base en dos grupos, 
#  - SIN.IMPUTAR: La información en las covariables esta completa 
#  - IMPUTAR:     La información en las covariables esta incompleta 

## Covariables del Modelo SAE Ciencia
#NOM.COV<-c( "UNOS", "CIENCIAS_5_2014", "CIENCIAS_5_2012", "LENGUAJE_5_2014")

## Covariables del Modelo SAE Matematicas
# NOM.COV<-c("UNOS","MATEMÁTICAS_5_2012", "MATEMÁTICAS_5_2014","PROM_INGLES_20142")

## Covariables del Modelo SAE Lenguaje
# NOM.COV<-c("UNOS","LENGUAJE_3_2014", "LENGUAJE_5_2014","LENGUAJE_9_2014")

## Covariables del Modelo SAE Competencia
# NOM.COV <- c("UNOS",  "CIENCIAS_5_2014", "CIENCIAS_9_2014","LENGUAJE_3_2014")
## Identificar EE que tienen variables que deben ser imputadas

IE.RESULTADO$IMP<-apply(IE.RESULTADO[,NOM.COV],1, function(x)ifelse(anyNA(x),"IMPUTAR","SIN.IMPUTAR"))
## Partir la base en dos MUESTRA.CONTROL y PRONOSTICO
MUESTRA.CONTROL<- subset(IE.RESULTADO,IND=="ESTIMAR"& IMP=="SIN.IMPUTAR")
## Omitir la IE que no presentaron resultado en el calculo de las varianza 
#MUESTRA.CONTROL<-na.omit(MUESTRA.CONTROL) ## se eliminan dos 
##############
table(IE.RESULTADO$IMP,IE.RESULTADO$IND)
sel = paste0(IE.RESULTADO$IMP,sep="_",IE.RESULTADO$IND)
PRONOSTICO<- subset(IE.RESULTADO,sel!="SIN.IMPUTAR_ESTIMAR")
PRONOSTICO[,c(NOM.COV)]<-complete(mice(PRONOSTICO[,c(NOM.COV)],method = "norm.boot"))
##############################################################################
## Evaluar el modelo con los diferentes estimadores
source(file = "src/Funciones/mlFH.r")
####################################
## Fay - Herriot:
####################################
fay<-mlFH(MUESTRA.CONTROL,PRONOSTICO,yhat="greg.PROM.IE",Sd.yhat = "greg.SD",Xk=NOM.COV)
MUESTRA.CONTROL$Fay.GREG<-fay$ESTIMA.Fay
PRONOSTICO$Fay.GREG <-fay$PRONS.Fay
CME.GREG <- fay$CME*100
####################################
## Fay - Herriot:
####################################
fay<-mlFH(MUESTRA.CONTROL,PRONOSTICO,yhat="HT.PROM.IE",Sd.yhat = "HT.SD",Xk=NOM.COV)
MUESTRA.CONTROL$Fay.HT<-fay$ESTIMA.Fay
PRONOSTICO$Fay.HT <-fay$PRONS.Fay
CME.HT <- fay$CME*100
####################################
## Fay - Herriot:
####################################
fay<-mlFH(MUESTRA.CONTROL,PRONOSTICO,yhat="Med.Comp_C3",Sd.yhat = "Sd.Comp_C3",Xk=NOM.COV)
MUESTRA.CONTROL$PROM_SAE <- fay$ESTIMA.Fay
PRONOSTICO$PROM_SAE      <- fay$PRONS.Fay
CME.COMP <- fay$CME*100
####################################
## Filtro de resultados
IE.RESULTADO<- data.frame(rbind(MUESTRA.CONTROL,PRONOSTICO))%>%
               dplyr::select(ID_INST,ENTIDAD,IND,PROM.IE,PROM_SAE)
               
## Estandarizar resultados con media 300 y desviación estándar de 80, 
## truncando los valores superiores a 500 y menores a 100.

media<-mean(IE.RESULTADO$PROM_SAE)
desv<- sd(IE.RESULTADO$PROM_SAE)
z<- (IE.RESULTADO$PROM_SAE-media)/desv
IE.RESULTADO$PROM_SAE<- round(80*z+300,0)
IE.RESULTADO$PROM_SAE<-ifelse(IE.RESULTADO$PROM_SAE<=100,100,
                             ifelse(IE.RESULTADO$PROM_SAE>=500,500,IE.RESULTADO$PROM_SAE))

write.table(IE.RESULTADO,file = "output/Colegios/Promedios/Ciencias/RESULTADOS.EE.txt",sep="\t",quote = FALSE,row.names = FALSE)
########################################################################
################        Resultados por ETC        ######################
########################################################################
PROMEDIO.ETC<- IE.RESULTADO%>%group_by(ENTIDAD)%>%summarise(PROM_SAE=mean(PROM_SAE))
## Adecuar las bases para realizar mapas

PROMEDIO.ETC$ENTIDAD<-toupper(PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Á","A", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("É","E", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Í","I", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Ó","O", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Ú","U", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Ü","U", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("BOGOTA, D.C.","BOGOTA", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("DOSQUEBRADAS","DOS QUEBRADAS", PROMEDIO.ETC$ENTIDAD)
Diccionario<-read.delim("input/Colegio/Base/Diccionario.txt")

PROMEDIO.ETC<-merge(Diccionario,PROMEDIO.ETC,by.x="Nombre",by.y = "ENTIDAD")
write.table(PROMEDIO.ETC,file = "output/Colegios/Promedios/Ciencias/RESULTADOS.ETC.txt",sep="\t",quote = FALSE,row.names = FALSE)


