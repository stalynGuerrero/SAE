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
dirpath <-"C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/Calificacion-quinto-2012"
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
load("output/IE_2013.car.model.RData")
## Clasificar la base en dos grupos, 
#  - SIN.IMPUTAR: La información en las covariables esta completa 
#  - IMPUTAR:     La información en las covariables esta incompleta 
NOM.COV <- c("UNOS","LENGUAJE_3_2013",	"MATEMÁTICAS_3_2013",	"MATEMÁTICAS_5_2012",	"MATEMÁTICAS_9_2012",	"PROM.FÍSICA_11_2013")
IE.RESULTADO$ESTADO<-apply(IE.RESULTADO[,NOM.COV],1, function(x)ifelse(anyNA(x),"IMPUTAR","SIN.IMPUTAR"))
#IE.RESULTADO$ESTADO<-paste0(IE.RESULTADO$ESTADO,sep="_",IE.RESULTADO$M.CONTROL)
table(IE.RESULTADO$M.CONTROL,IE.RESULTADO$ESTADO)
## Partir la base en dos MUESTRA.CONTROL y PRONOSTICO
MUESTRA.CONTROL<- subset(IE.RESULTADO,M.CONTROL==1&ESTADO=="SIN.IMPUTAR")
## Omitir la IE que no presentaron resultado en el calculo de las varianza 
#MUESTRA.CONTROL<-na.omit(MUESTRA.CONTROL) ## se eliminan dos 
##############
sel<- paste0(IE.RESULTADO$ESTADO,sep="_",IE.RESULTADO$M.CONTROL)
table(sel)
PRONOSTICO1<- subset(IE.RESULTADO,sel%in%c("IMPUTAR_1","IMPUTAR_0"))
############################################################################################
SB11_2013 <- read.csv2("input/Colegio/Covariable/Resultados_Saber_11_2013.csv",sep = ";",
                       colClasses = rep(c("factor","numeric"),c(3,8)))
SB11_2013<-subset(SB11_2013,PERIODO=="20132")

SB11_2013$DANE_ESTABLECIMIENTO<- as.character(SB11_2013$DANE_ESTABLECIMIENTO)
SB11_2013$DANE_SEDE<- as.character(SB11_2013$DANE_SEDE)
SB11_2013<-subset(SB11_2013,DANE_ESTABLECIMIENTO==DANE_SEDE)[,-c(1,3)]

colnames(SB11_2013)[-1]<-paste0(colnames(SB11_2013)[-1],"_11_2013")
# Resultados de promedio por IE en pruebas 3579 de años anteriores
IE_HIS<- read.csv2("input/Colegio/Covariable/Estudiantes/infoEstabsCensal12_13.txt",sep = "\t" ,
                   colClasses = rep(c("factor",NA,"factor",NA,"numeric",NA,"numeric",NA),c(1,5,2,2,1,3,1,2)))
head(IE_HIS)
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
colnames(IE_COVARIABLE)<- gsub(" ","_",colnames(IE_COVARIABLE))

IE_COVARIABLE<-IE_COVARIABLE%>%dplyr::select(-CIENCIAS_5_2012,-MATEMÁTICAS_5_2013,-LENGUAJE_5_2013,-CIENCIAS_9_2013,
                                             -CIENCIAS_5_2013,-PENSAMIENTO_CIUDADANO_5_2013)
##############################################################################################################################
imputar<-merge(IE_COVARIABLE,PRONOSTICO1[,c("ID_INST","UNOS")],by.x="ID_INSTITUCION",by.y="ID_INST",all.y = T)

head(cbind(PRONOSTICO1[,NOM.COV[2]],imputar))
for(i in NOM.COV[-1]){
  print(i)
  for(j in colnames(imputar)[-1]){
    cat(j,"=",cor(na.omit(cbind(imputar[,c(i,j)])))[2,1],"\n")
  }
  
}
"LENGUAJE_3_2013     = MATEMÁTICAS_3_2013"
"MATEMÁTICAS_3_2013  = LENGUAJE_3_2013"
"MATEMÁTICAS_5_2012  = LENGUAJE_5_2012+PENSAMIENTO_CIUDADANO_5_2012"
"MATEMÁTICAS_9_2012  = CIENCIAS_9_2012+LENGUAJE_9_2012"
"PROM.FÍSICA_11_2013 = PROM.BIOLOGÍA_11_2013+PROM.QUÍMICA_11_2013"

imp<-complete(mice(imputar[,c("LENGUAJE_3_2013","MATEMÁTICAS_3_2013")],method = "norm.boot"))
PRONOSTICO1[,c("LENGUAJE_3_2013","MATEMÁTICAS_3_2013")]<- imp

imp<-complete(mice(imputar[,c("MATEMÁTICAS_5_2012","LENGUAJE_5_2012","PENSAMIENTO_CIUDADANO_5_2012")],method = "norm.boot"))
PRONOSTICO1[,c("MATEMÁTICAS_5_2012")]<- imp[,"MATEMÁTICAS_5_2012"]

imp<-complete(mice(imputar[,c("MATEMÁTICAS_9_2012","CIENCIAS_9_2012","LENGUAJE_9_2012")],method = "norm.boot"))
PRONOSTICO1[,c("MATEMÁTICAS_9_2012")]<- imp[,"MATEMÁTICAS_9_2012"]

imp<-complete(mice(imputar[,c("PROM.FÍSICA_11_2013","PROM.BIOLOGÍA_11_2013","PROM.QUÍMICA_11_2013")],method = "norm.boot"))
PRONOSTICO1[,c("PROM.FÍSICA_11_2013")]<- imp[,"PROM.FÍSICA_11_2013"]


##############################################################################################################################
## Clasificación de las covariables en tres grupos,
## - "SIN UN DATO" (cuando la fila de observasiones le hace falta un dato ),
## - "SIN MAS DATO"(cuando la fila de observasiones le hace falta mas de una observación)
## - "COMPLETA" (No tiene observaciones faltantes)

############################################################################################
PRONOSTICO<- subset(IE.RESULTADO,sel%in%c("SIN.IMPUTAR_0"))
PRONOSTICO<-rbind(PRONOSTICO1,PRONOSTICO)
rm(list=c("IE_HIS", "SB11_2013","imp","imputar","PRONOSTICO1"))
##############################################################################
## Evaluar el modelo con los diferentes estimadores
MODEL.Fay<-function(ESTIMA,NOM.COV,ESTIMA.SD){  
  xk <- paste0("MUESTRA.CONTROL$",NOM.COV[-1],collapse = "+")
  model<-paste0("resultREML0<-eblupFH(as.vector(MUESTRA.CONTROL$",ESTIMA,")~",xk,",vardir=MUESTRA.CONTROL$",ESTIMA.SD,"^2, method = 'REML')")
  eval(parse(text=model))
  list(
    ESTIMA.Fay = resultREML0$eblup[,1],
    PRONS.Fay  = as.matrix(PRONOSTICO[,NOM.COV])%*%resultREML0$fit$estcoef$beta)
  }
####################################
## Fay - Herriot:
####################################
fay<-MODEL.Fay("greg.PROM.IE",NOM.COV, "greg.SD")
MUESTRA.CONTROL$Fay.GREG<-fay$ESTIMA.Fay
PRONOSTICO$Fay.GREG <-fay$PRONS.Fay
####################################
## Fay - Herriot:
####################################
fay<-MODEL.Fay("HT.PROM.IE",NOM.COV, "HT.SD")
MUESTRA.CONTROL$Fay.HT<-fay$ESTIMA.Fay
PRONOSTICO$Fay.HT <-fay$PRONS.Fay
####################################
## Fay - Herriot:
####################################
fay<-MODEL.Fay("Med.Comp_C3",NOM.COV, "Sd.Comp_C3")
MUESTRA.CONTROL$Fay.Comp<-fay$ESTIMA.Fay
PRONOSTICO$Fay.Comp <-fay$PRONS.Fay

IE.RESULTADO<- data.frame(rbind(MUESTRA.CONTROL,PRONOSTICO))
table(IE.RESULTADO$M.CONTROL,IE.RESULTADO$ESTADO)
IE.RESULTADO$M.CONTROL <-factor(IE.RESULTADO$M.CONTROL,labels=c("PRONOSTICO","ESTIMACIÓN"))

G0 <- ggplot(IE.RESULTADO,aes(x=Fay.GREG,y=PROM.MAT))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+xlim(100,500)+ylim(100,500)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G0<-G0+facet_grid(. ~ ESTADO)+ggtitle("ESTIMADOR GREG")
X11()
G0


G1 <- ggplot(IE.RESULTADO,aes(x=Fay.HT,y=PROM.MAT))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+xlim(100,500)+ylim(100,500)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G1<-G1+facet_grid(. ~ ESTADO)+ggtitle("ESTIMADOR HT")
X11()
G1

G2 <- ggplot(IE.RESULTADO,aes(x=Fay.Comp,y=PROM.MAT))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+xlim(100,500)+ylim(100,500)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G2<-G2+facet_grid(. ~ ESTADO)+ggtitle("ESTIMADOR COMP")
X11()
G2

X11()
gridExtra::grid.arrange( G0,G1,G2, ncol=1)

jpeg(filename = file.path("output/Colegios/Graficas/Resultado_covar_INSE/Modelo car/Fay.jpg"),width = 1200,height = 1200)
gridExtra::grid.arrange( G1,G0,G2, ncol=1)
dev.off()     

ggsave(G0,filename = "output/Colegios/Graficas/Resultado_covar_INSE/Modelo car/Fay.GREG.jpg",width=15, height=12)
ggsave(G1,filename = "output/Colegios/Graficas/Resultado_covar_INSE/Modelo car/Fay.HT.jpg",width=15, height=12)
ggsave(G2,filename = "output/Colegios/Graficas/Resultado_covar_INSE/Modelo car/Fay.COMP.jpg",width=15, height=12)

CME <- IE.RESULTADO %>%group_by(ESTADO)%>% summarise(
  GREG =sqrt(sum((PROM.MAT-Fay.GREG)^2,na.rm=T)/n()),
  HT =sqrt(sum((PROM.MAT-Fay.HT)^2,na.rm=T)/n()),
  COMP =sqrt(sum((PROM.MAT-Fay.Comp)^2,na.rm=T)/n()))

write.table(CME,file = "output/Colegios/Graficas/Resultado_covar_INSE/Modelo washMach/CME.txt",sep="\t",quote = FALSE,row.names = FALSE)

IE.RESULTADO <-IE.RESULTADO%>%dplyr::select(ID_INST,ENTIDAD,M.CONTROL,PROM.MAT,Fay.GREG, Fay.HT,Fay.Comp)

write.table(IE.RESULTADO,file = "output/Colegios/Promedios/Resultados.IE1.car.txt",sep="\t",quote = FALSE,row.names = FALSE)


