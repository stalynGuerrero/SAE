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
##############################################################################################################################
# Resultados de promedio por IE en las pruebas Saber 11 para el 2013
source(file = "input/Colegio/Covariable/Cruce de información auxiliar.r")
source(file = "src/Funciones/SEL.MODEL.SAE.r")

SB11<-SB11 %>%  dplyr::select(-PERIODO_2012,-PERIODO_2013,-ESTUDIANTES_20142,-ESTUDIANTES_2015,
                             -PERIODO_20142,-PERIODO_2015,-ESTUDIANTES_2013)
SB11<-SB11[,c("DANE_ESTABLECIMIENTO",colnames(SB11)[grepl("2014",colnames(SB11))])]
apply(SB11, 2,function(x)sum(is.na(x)))              
# Resultados de promedio por IE en pruebas 3579 de años anteriores
IE_HIS<-read.delim(file=file.path(dirpath, inpath,"/Colegio/Covariable/Estudiantes/ResultadosSB359_2014Establecimientos.txt"),
                                colClasses="character",encoding = "UTF-8")
IE_HIS$promedio_2012<-as.numeric(IE_HIS$promedio_2012)
IE_HIS$promedio_2013<-as.numeric(IE_HIS$promedio_2013)
IE_HIS$promedio_2014<-as.numeric(IE_HIS$promedio_2014)

IE_HIS<- dcast(IE_HIS,id_institucion ~ areaNom+grado ,value.var ="promedio_2012",fun.aggregate = mean)%>%
   merge(dcast(IE_HIS,id_institucion ~ areaNom+grado ,value.var ="promedio_2013",fun.aggregate = mean,na.rm=T), by="id_institucion")%>%
   merge(dcast(IE_HIS,id_institucion ~ areaNom+grado ,value.var ="promedio_2014",fun.aggregate = mean,na.rm=T), by="id_institucion")

colnames(IE_HIS)<-toupper(colnames(IE_HIS))

colnames(IE_HIS)<-gsub(".X","_2012",colnames(IE_HIS))
colnames(IE_HIS)<-gsub(".Y","_2013",colnames(IE_HIS))
colnames(IE_HIS)<-gsub("_9$","_9_2014",colnames(IE_HIS))
colnames(IE_HIS)<-gsub("_5$","_5_2014",colnames(IE_HIS))
colnames(IE_HIS)<-gsub("_3$","_3_2014",colnames(IE_HIS))

sum(IE_HIS$ID_INSTITUCION%in%SB11$DANE_ESTABLECIMIENTO)

##############################################################################################################################
IE_COVARIABLE<-merge(IE_HIS,SB11,by.x = "ID_INSTITUCION",by.y = "DANE_ESTABLECIMIENTO")
##############################################################################################################################
## Clasificación de las covariables en tres grupos,
## - "SIN UN DATO" (cuando la fila de observasiones le hace falta un dato ),"SIN MAS DATO"),"COMPLETA"
apply(IE_COVARIABLE, 2,function(x)sum(is.na(x)))              
IE_COVARIABLE$ESTADO<- apply(IE_COVARIABLE[,-1],1,function(x)ifelse(anyNA(x),ifelse(sum(is.na(x))==1,"SIN UN DATO","SIN MAS DATO"),"COMPLETA"))
table(IE_COVARIABLE$ESTADO,useNA = "a")
rm(list=c("IE_HIS", "SB11"))
##############################################################################################################################
## Lectura de la base de datos que contiene la información estimada mediante jackknife de las varianzas por IE.
## esta se obtiene del codigo "01 Varianza.HT.GREG_IE.r"
##############################################################################################################################
# load("output/Colegios/Varianzas Estimadas/IE_2015.PROM.COMPETENCIA_INSE.RData")
# load("output/Colegios/Varianzas Estimadas/IE_2015.PROM.MATEMATICAS_INSE.RData")
# load("output/Colegios/Varianzas Estimadas/IE_2015.PROM.LENGUAJE_INSE.RData")
# load("output/Colegios/Varianzas Estimadas/IE_2015.PROM.CIENCIAS_INSE.RData")

#####################
## Lectura selección de los nombres de las covariables a emplear
NOM_COV<-names(IE_COVARIABLE)[!names(IE_COVARIABLE)%in%c("ID_INSTITUCION","ESTADO","CIENCIAS_5_2013","CIENCIAS_9_2013")]
## Omitil algunas de esta
NOM_COV<-NOM_COV[!grepl(" ",NOM_COV)]

## Uniendo las variables respues y la covariable 
IE.RESULTADO<-merge(IE.RESULTADO,IE_COVARIABLE,by.x = "ID_INST",by.y="ID_INSTITUCION",all = T)
## Seleccionar las IE que cuantan con la mayor cantidad de información para así poder hacer la selección de un modelo 
## SAE que sea adecuado para realizar pronosticos 
#########################################################################################################
# Seleccion de modelo con 2 covariables
########################################

# CME <- SEL.MODEL(IE.RESULTADO,yhat="greg.PROM.IE",Sd.yhat= "greg.SD",NOM_COV,y="PROM.IE",p=2)
# CME%>%arrange(desc(CME.CONTRL))%>%head()

#########################################################################################################
 IE_censal<- read.delim(file=file.path(dirpath, inpath,"/Colegio/Covariable/Estudiantes/ResultadosSB359_2014Establecimientos.txt"),
                        colClasses="character",encoding = "UTF-8")
#########################################################################################################
## Identificando las I.E. que pertenensen al censo de IE (seguimiento desde el 2009 hasta 2014)
#########################################################################################################
IE_censal <- unique(IE_censal[,c("id_institucion","entidad","zona","sector")])
IE.RESULTADO<-merge(IE_censal,IE.RESULTADO,by.x="id_institucion",by.y="ID_INST")
IE.RESULTADO$ENTIDAD<-IE.RESULTADO$entidad
IE.RESULTADO$ID_INST<-IE.RESULTADO$id_institucion
IE.RESULTADO$UNOS<-1
IE.RESULTADO$IND<- ifelse(!is.na(IE.RESULTADO$greg.PROM.IE),"ESTIMAR","PRONOSTICO")
######################################################################################################
IE.RESULTADO<-IE.RESULTADO %>% dplyr::select(ID_INST,ENTIDAD,PROM.IE,IND,
                               ## Promedios y desviaciones  estimadas por ETC
                               PROM.greg.ETC,  Sd.greg.PROM,
                               PROM.HT.ETC,    Sd.HT.PROM,
                               ## Promedios y desviaciones  estimadas por IE
                               greg.PROM.IE,   greg.SD,
                               HT.PROM.IE  ,   HT.SD,
                               Med.Comp_C3 ,   Sd.Comp_C3,
                               Med.Comp_C3 ,   Sd.Comp_C3,
                               ## Covariables del Modelo SAE Ciencia
                               # UNOS,CIENCIAS_5_2014,CIENCIAS_5_2012,LENGUAJE_5_2014
                               
                               ## Covariables del Modelo SAE Matematicas
                                UNOS,MATEMÁTICAS_5_2014,MATEMÁTICAS_5_2012,PROM_INGLES_20142
  
                               ## Covariables del Modelo SAE Lenguaje
                                # UNOS,LENGUAJE_3_2014, LENGUAJE_5_2014, LENGUAJE_9_2014
                               
                               ## Covariables del Modelo SAE Competencia
#                                 UNOS,CIENCIAS_5_2014,	CIENCIAS_9_2014, LENGUAJE_3_2014
                                )
head(IE.RESULTADO)
#########################################################################################################
save(IE.RESULTADO, file = file.path("output/Colegios/Model SAE/IE_2015.PROM.MATEMATICAS_SAE.RData"))
#########################################################################################################
