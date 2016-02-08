###########################################################################################################
######## Seleccion del mejor modelo de SAE, condicionado a las covariables que sean introducidas     ######
######## en el modelo                                                                                ######
###########################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 20-01 - 2016
## Subdirección de Estadística ICFES
###################################################################################################
rm(list = ls())
## Definir el directorio de trabajo
dirpath <-"C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/"
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
##############################################################################################################################
# Resultados de promedio por IE en las pruebas Saber 11 para el 2013
SB11_2013 <- read.csv2("input/Colegio/Covariable/Resultados_Saber_11_2013.csv",sep = ";",
                         colClasses = rep(c("factor","numeric"),c(3,8)))[,-c(1,3)]
colnames(SB11_2013)[-1]<-paste0(colnames(SB11_2013)[-1],"_11_2013")
# Resultados de promedio por IE en pruebas 3579 de años anteriores
IE_HIS<- read.csv2("input/Colegio/Covariable/Estudiantes/infoEstabsCensal12_13.txt",sep = "\t" ,
                   colClasses = rep(c("factor",NA,"factor",NA,"numeric",NA,"numeric",NA),c(1,5,2,2,1,3,1,2)))

IE_HIS<- dcast(IE_HIS,id_institucion ~ areaNom+grado ,value.var ="promedio_2013",fun.aggregate = mean)%>%
   merge(dcast(IE_HIS,id_institucion ~ areaNom+grado ,value.var ="promedio_2012",fun.aggregate = mean,na.rm=T), by="id_institucion")

colnames(IE_HIS)<-toupper(colnames(IE_HIS))

colnames(IE_HIS)<-gsub(".X","_2013",colnames(IE_HIS))
colnames(IE_HIS)<-gsub(".Y","_2012",colnames(IE_HIS))

##############################################################################################################################
IE_COVARIABLE<-merge(IE_HIS,SB11_2013,by.x = "ID_INSTITUCION",by.y = "DANE_ESTABLECIMIENTO",all.x = T)
#$ Filtrando las escuelas que tenian reporte de matematicas del grado 5
IE_COVARIABLE<-IE_COVARIABLE%>%filter(!is.na(MATEMÁTICAS_5_2013))
## Excluyendo algúnas columnas que no reportaban información 
IE_COVARIABLE<-IE_COVARIABLE%>%dplyr::select(-CIENCIAS_5_2012,-MATEMÁTICAS_5_2013,-LENGUAJE_5_2013,-CIENCIAS_9_2013,
                                             -CIENCIAS_5_2013)
##############################################################################################################################
## Clasificación de las covariables en tres grupos,
## - "SIN UN DATO" (cuando la fila de observasiones le hace falta un dato ),"SIN MAS DATO"),"COMPLETA"

IE_COVARIABLE$ESTADO<- apply(IE_COVARIABLE[,-c(2,3)],1,function(x)ifelse(anyNA(x),ifelse(sum(is.na(x))==1,"SIN UN DATO","SIN MAS DATO"),"COMPLETA"))
table(IE_COVARIABLE$ESTADO,useNA = "a")
rm(list=c("IE_HIS", "SB11_2013"))
##############################################################################################################################
## Lectura de la base de datos que contiene la información estimada mediante jackknife de las varianzas por IE.
## esta se obtiene del codigo "01 Varianza.HT.GREG_IE.r"
##############################################################################################################################
load("output/IE_2013.2model.rdata")
#####################
## Lectura selección de los nombres de las covariables a emplear
NOM_COV<-names(IE_COVARIABLE)[!names(IE_COVARIABLE)%in%c("ID_INSTITUCION","ESTADO")]
## Omitil algunas de esta
NOM_COV<-NOM_COV[!grepl(" ",NOM_COV)]

## Uniendo las variables respues y la covariable 
IE.RESULTADO<-merge(IE.RESULTADO,IE_COVARIABLE,by.x = "ID_INST",by.y="ID_INSTITUCION",all.x = T)
## Seleccionar las IE que cuantan con la mayor cantidad de información para así poder hacer la selección de un modelo 
## SAE que sea adecuado para realizar pronosticos 
# IE.RESULTADO$SECTOR <- ifelse(IE.RESULTADO$SECTOR=="Oficial",1,0)
# IE.RESULTADO$ZONA <- ifelse(IE.RESULTADO$ZONA=="Rural",1,0)
IE.RESULTADO<-IE.RESULTADO%>%filter(!is.na(PROM.MAT)&ESTADO%in%c("COMPLETA","SIN UN DATO"))
IE.RESULTADO$UNOS <- 1
## Seleccionando las IE que pertenecen a la muestra control 
MUEST.CONTROL <- IE.RESULTADO%>%filter(M.CONTROL==1)
# MUEST.CONTROL$ZONA <- as.factor(MUEST.CONTROL$ZONA)
# MUEST.CONTROL$SECTOR <- as.factor(MUEST.CONTROL$SECTOR)
MUEST.CONTROL <- na.omit(MUEST.CONTROL)

#NOM_COV<-c(NOM_COV,"ZONA","SECTOR" )

SEL.MODEL<-function(x){  
xk <- paste0("MUEST.CONTROL$",x,collapse = "+")
model<-paste0("resultREML0<-eblupFH(as.vector(MUEST.CONTROL$greg.PROM.IE)~",xk,",vardir=MUEST.CONTROL$greg.SD^2, method = 'REML')")
eval(parse(text=model))
CME=sum((resultREML0$eblup[,1]-MUEST.CONTROL$PROM.MAT)^2)/nrow(MUEST.CONTROL)
temp2 <-na.omit(IE.RESULTADO[,c("PROM.MAT","UNOS",x)])
Temp00 <- as.matrix(temp2[,c("UNOS",x)])%*%resultREML0$fit$estcoef$beta
CME_PRONS<-sqrt(sum((Temp00[,1]-temp2$PROM.MAT)^2,na.rm = T)/nrow(temp2))
c(resultREML0$fit$goodness[1:4], CME=sqrt(CME),CME_PRONS=CME_PRONS)
}
#########################################################################################################
# Seleccion de modelo con 2 covariables
########################################
Cov.xk <-data.frame(t(combn(NOM_COV,2)))
CME <-apply(Cov.xk, 1, SEL.MODEL)
CME<-t(CME)
head(CME[order(CME[,5]),])
head(Cov.xk[order(CME[,5]),])
#--------------------------
head(CME[order(CME[,6]),])
head(Cov.xk[order(CME[,6]),])
## Modelo resultante: 
Model.Propuestos<- rbind(SEL.MODEL(c("MATEMÁTICAS_5_2012", "PROM.NGLÉS_11_2013")),
                        SEL.MODEL(c("MATEMÁTICAS_3_2013" ,"MATEMÁTICAS_5_2012")))
#########################################################################################################
# Seleccion de modelo con 3 covariables
########################################

Cov.xk <-data.frame(t(combn(NOM_COV,3)))
CME <-apply(Cov.xk, 1, SEL.MODEL)

CME<-t(CME)
head(CME[order(CME[,5]),])
head(Cov.xk[order(CME[,5]),])
##----------------------
head(CME[order(CME[,6]),])
head(Cov.xk[order(CME[,6]),])
## Modelo resultante:
Model.Propuestos<- rbind(Model.Propuestos,
                         SEL.MODEL(c("MATEMÁTICAS_5_2012",  "PROM.FÍSICA_11_2013",  "PROM.NGLÉS_11_2013")),
                         SEL.MODEL(c("LENGUAJE_9_2013", "MATEMÁTICAS_3_2013", "MATEMÁTICAS_5_2012")))
#########################################################################################################
# Seleccion de modelo con 4 covariables
########################################
Cov.xk <-data.frame(t(combn(NOM_COV,4)))
CME <-apply(Cov.xk, 1, SEL.MODEL)

CME<-t(CME)
head(CME[order(CME[,5]),])
head(Cov.xk[order(CME[,5]),])
## ---------------------------------
head(CME[order(CME[,6]),])
head(Cov.xk[order(CME[,5]),])
## Modelo resultante:
Model.Propuestos<- rbind(Model.Propuestos,
                         SEL.MODEL(c("MATEMÁTICAS_5_2012","PROM.CIENCIAS.SOCIALES_11_2013","PROM.FÍSICA_11_2013","PROM.NGLÉS_11_2013")),
                         SEL.MODEL(c("LENGUAJE_3_2013", "MATEMÁTICAS_3_2013", "MATEMÁTICAS_9_2013", "MATEMÁTICAS_5_2012")))
#########################################################################################################
# Seleccion de modelo con 5 covariables
########################################

Cov.xk <-data.frame(t(combn(NOM_COV,5)))
CME <-apply(Cov.xk, 1, SEL.MODEL)

CME<-t(CME)
head(CME[order(CME[,5]),])
head(Cov.xk[order(CME[,5]),])
## ---------------------------------
head(CME[order(CME[,6]),])
head(Cov.xk[order(CME[,5]),])
## Modelo resultante:
Model.Propuestos<- rbind(Model.Propuestos,
                         SEL.MODEL(c("MATEMÁTICAS_3_2012","MATEMÁTICAS_5_2012", "PROM.CIENCIAS.SOCIALES_11_2013", "PROM.FÍSICA_11_2013","PROM.NGLÉS_11_2013")),
                         SEL.MODEL(c("LENGUAJE_3_2013", "LENGUAJE_9_2013", "MATEMÁTICAS_3_2013", "MATEMÁTICAS_9_2013", "MATEMÁTICAS_5_2012")))



######################################################################################################
## El resultado de la rutina anterior es una base que contiene el siguente conjunto de variables #####
##                                                                                               #####
##                  GREG = UNOS + LENGUAJE_3_2013+"LENGUAJE_9_2013+MATEMÁTICAS_3_2013+           #####
##                                MATEMÁTICAS_9_2013+MATEMÁTICAS_5_2012                          #####
######################################################################################################
IE.RESULTADO<-IE.RESULTADO %>% dplyr::select(ID_INST,ENTIDAD,M.CONTROL,PROM.MAT,EE.MAT,ESTADO,
                               ## Promedios y desviaciones  estimadas por ETC
                               PROM.greg.ETC,  Sd.greg.PROM,
                               PROM.HT.ETC,    Sd.HT.PROM,
                               ## Promedios y desviaciones  estimadas por IE
                               greg.PROM.IE,   greg.SD,
                               HT.PROM.IE  ,   HT.SD,
                               Med.Comp_C3 ,   Sd.Comp_C3,
                               Med.Comp_C3 ,   Sd.Comp_C3,
                               ## Covariables del Modelo SAE
                               UNOS, LENGUAJE_3_2013    , LENGUAJE_9_2013, 
                               MATEMÁTICAS_3_2013 , MATEMÁTICAS_9_2013, 
                               MATEMÁTICAS_5_2012)


save(IE.RESULTADO, file = file.path("output/IE_2013.2model.RData"))

#############################################################################################
