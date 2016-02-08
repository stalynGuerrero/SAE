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
load("output/IE_2013.2model.RData")
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

X11()
gridExtra::grid.arrange( G0,G1,G2, ncol=1)

jpeg(filename = file.path("output/Colegios/Graficas/Resultado_covar_INSE/Fay.jpg"),width = 1200,height = 1200)
gridExtra::grid.arrange( G1,G0,G2, ncol=1)
dev.off()     

ggsave(G0,filename = "output/Colegios/Graficas/Resultado_covar_INSE/Fay.GREG.jpg",width=15, height=12)
ggsave(G1,filename = "output/Colegios/Graficas/Resultado_covar_INSE/Fay.HT.jpg",width=15, height=12)
ggsave(G2,filename = "output/Colegios/Graficas/Resultado_covar_INSE/Fay.COMP.jpg",width=15, height=12)

CME <- IE.RESULTADO %>%group_by(M.CONTROL)%>% summarise(
  GREG =sqrt(sum((PROM.MAT-Fay.GREG)^2,na.rm=T)/n()),
  HT =sqrt(sum((PROM.MAT-Fay.HT)^2,na.rm=T)/n()),
  COMP =sqrt(sum((PROM.MAT-Fay.Comp)^2,na.rm=T)/n()))

write.table(CME,file = "output/Colegios/Graficas/Resultado_covar_INSE/CME.txt",sep="\t",quote = FALSE,row.names = FALSE)

IE.RESULTADO <-IE.RESULTADO%>%dplyr::select(ID_INST,ENTIDAD,M.CONTROL,PROM.MAT,Fay.GREG, Fay.HT,Fay.Comp)

write.table(IE.RESULTADO,file = "output/Colegios/Promedios/Resultados.IE2.txt",sep="\t",quote = FALSE,row.names = FALSE)


