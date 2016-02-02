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
###########################################################################################################
## Lectura de la base de datos de las IE con sus respectivas covariables 
load("output/IE_2013.RData")
## Clasificar la base en dos grupos, 
#  - SIN.IMPUTAR: La información en las covariables esta completa 
#  - IMPUTAR:     La información en las covariables esta incompleta 
NOM.COV <- c("UNOS", "LENGUAJE_3_2013", "LENGUAJE_9_2013","MATEMÁTICAS_3_2013","MATEMÁTICAS_9_2013","MATEMÁTICAS_5_2012")
IE.RESULTADO$ESTADO<-apply(IE.RESULTADO[,NOM.COV],1, function(x)ifelse(anyNA(x),"IMPUTAR","SIN.IMPUTAR"))
## Partir la base en dos MUESTRA.CONTROL y PRONOSTICO
MUESTRA.CONTROL<- subset(IE.RESULTADO,M.CONTROL==1& ESTADO=="SIN.IMPUTAR")
## Omitir la IE que no presentaron resultado en el calculo de las varianza 
MUESTRA.CONTROL<-na.omit(MUESTRA.CONTROL) ## se eliminan dos 
##############
PRONOSTICO<- subset(IE.RESULTADO,M.CONTROL!=1& ESTADO=="SIN.IMPUTAR")
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
IE.RESULTADO$M.CONTROL <-factor(IE.RESULTADO$M.CONTROL,labels=c("PRONOSTICO","ESTIMACIÓN"))

G0 <- ggplot(IE.RESULTADO,aes(x=Fay.GREG,y=PROM.MAT))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+xlim(100,500)+ylim(100,500)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G0<-G0+facet_grid(. ~ M.CONTROL)+ggtitle("ESTIMADOR GREG")
X11()
G0


G1 <- ggplot(IE.RESULTADO,aes(x=Fay.HT,y=PROM.MAT))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+xlim(100,500)+ylim(100,500)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G1<-G1+facet_grid(. ~ M.CONTROL)+ggtitle("ESTIMADOR HT")
X11()
G1

G2 <- ggplot(IE.RESULTADO,aes(x=Fay.Comp,y=PROM.MAT))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+xlim(100,500)+ylim(100,500)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G2<-G2+facet_grid(. ~ M.CONTROL)+ggtitle("ESTIMADOR COMP")
X11()
G2


CME <- IE.RESULTADO %>%group_by(M.CONTROL)%>% summarise(
  GREG =sum((PROM.MAT-Fay.GREG)^2,na.rm=T)/n(),
  HT =sum((PROM.MAT-Fay.HT)^2,na.rm=T)/n(),
  COMP =sum((PROM.MAT-Fay.Comp)^2,na.rm=T)/n())
t(CME)
sqrt(CME[,-1])
