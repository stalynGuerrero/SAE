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
load("output/IE_2015.PROM.MAT2.RData")
## Clasificar la base en dos grupos, 
#  - SIN.IMPUTAR: La información en las covariables esta completa 
#  - IMPUTAR:     La información en las covariables esta incompleta 

## Covariables del Modelo SAE Ciencia
#NOM.COV<-c("UNOS","LENGUAJE_5_2013","MATEMÁTICAS_5_2013")
## Covariables del Modelo SAE Matematicas
 NOM.COV<-c("UNOS","PROM_MATEMATICAS_20142","MATEMÁTICAS_5_2013")

## Covariables del Modelo SAE Lenguaje
# NOM.COV<-c("UNOS","CIENCIAS_5_2014","LENGUAJE_9_2014")
## Covariables del Modelo SAE Competencia
#NOM.COV <- c("UNOS", "CIENCIAS_5_2014",	"CIENCIAS_9_2014")
IE.RESULTADO$ESTADO<-apply(IE.RESULTADO[,NOM.COV],1, function(x)ifelse(anyNA(x),"IMPUTAR","SIN.IMPUTAR"))
IE.RESULTADO$M_CONTROL<-apply(IE.RESULTADO[,c("greg.PROM.IE","HT.PROM.IE","Med.Comp_C3")],1, function(x)ifelse(anyNA(x),0,1))
## Partir la base en dos MUESTRA.CONTROL y PRONOSTICO
MUESTRA.CONTROL<- subset(IE.RESULTADO,M_CONTROL==1& ESTADO=="SIN.IMPUTAR")
## Omitir la IE que no presentaron resultado en el calculo de las varianza 
#MUESTRA.CONTROL<-na.omit(MUESTRA.CONTROL) ## se eliminan dos 
##############
table(IE.RESULTADO$ESTADO,IE.RESULTADO$M_CONTROL)
PRONOSTICO<- subset(IE.RESULTADO,M_CONTROL==0& ESTADO=="SIN.IMPUTAR")
#PRONOSTICO[,NOM.COV]<-complete(mice(PRONOSTICO[,NOM.COV],method = "norm.boot"))
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
IE.RESULTADO$M.CONTROL <-factor(IE.RESULTADO$M_CONTROL,labels=c("PRONOSTICO","ESTIMACIÓN"))

G0 <- ggplot(IE.RESULTADO,aes(x=Fay.GREG,y=PROM.IE))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G0<-G0+facet_grid(. ~ M.CONTROL,scales = "free")+ggtitle("ESTIMADOR GREG")
X11()
G0


G1 <- ggplot(IE.RESULTADO,aes(x=Fay.HT,y=PROM.IE))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G1<-G1+facet_grid(. ~ M.CONTROL,scales = "free")+ggtitle("ESTIMADOR HT")
X11()
G1

G2 <- ggplot(IE.RESULTADO,aes(x=Fay.Comp,y=PROM.IE))+geom_point(size=2.5)+
  theme_light(20)+ geom_smooth(method = lm,se=FALSE,size=1.5)+
  ylab("Censal")+xlab("SAE")+labs(colour="")+ geom_abline(intercept = 0, slope = 1)
G2<-G2+facet_grid(. ~ M.CONTROL,scales = "free")+ggtitle("ESTIMADOR COMP")
X11()
G2


X11()
gridExtra::grid.arrange( G0,G1,G2, ncol=1)

jpeg(filename = file.path("output/Colegios/Graficas/Resultado_Mat/Fay.jpg"),width = 1200,height = 1200)
gridExtra::grid.arrange( G1,G0,G2, ncol=1)
dev.off()     

ggsave(G0,filename = "output/Colegios/Graficas/Resultado_Mat/Fay.GREG.jpg",width=15, height=12)
ggsave(G1,filename = "output/Colegios/Graficas/Resultado_Mat/Fay.HT.jpg",width=15, height=12)
ggsave(G2,filename = "output/Colegios/Graficas/Resultado_Mat/Fay.COMP.jpg",width=15, height=12)

# CME <- IE.RESULTADO %>%group_by(M_CONTROL)%>% summarise(
#   GREG =sqrt(sum((PROM.IE-Fay.GREG)^2,na.rm=T)/n()),
#   HT =sqrt(sum((PROM.IE-Fay.HT)^2,na.rm=T)/n()),
#   COMP =sqrt(sum((PROM.IE-Fay.Comp)^2,na.rm=T)/n()))

#write.table(CME,file = "output/Colegios/Graficas/Resultado_covar_INSE/Modelo washMach/CME.txt",sep="\t",quote = FALSE,row.names = FALSE)

IE.RESULTADO <-IE.RESULTADO%>%dplyr::select(ID_INST,ENTIDAD,M.CONTROL,PROM.IE,Fay.GREG, Fay.HT,Fay.Comp)

write.table(IE.RESULTADO,file = "output/Colegios/Promedios/Resultados.IE.MAT.txt",sep="\t",quote = FALSE,row.names = FALSE)


CME<-subset(IE.RESULTADO,!is.na(PROM.IE))
sqrt(sum((CME$PROM.IE-CME$Fay.Comp)^2)/nrow(CME))

G2 <- ggplot(subset(IE.RESULTADO,!is.na(PROM.IE)),aes(x=PROM.IE))+geom_density(size=2,adjust=2)+theme_light(20)+
  labs(x="",y="",title="Muestra, n=331, CME =5.54%")+xlim(0,1)
dim(subset(IE.RESULTADO,is.na(PROM.IE)))
G2.1 <- ggplot(subset(IE.RESULTADO,is.na(PROM.IE)),aes(x=Fay.Comp))+geom_density(size=2,adjust=2)+theme_light(20)+
  labs(x="",y="",title="Pronóstico, N=7266")+xlim(0,1)

jpeg(filename = file.path("output/Colegios/Graficas/Resultado_Mat/FayMAT.jpg"),width = 1200,height = 1200)
gridExtra::grid.arrange( G2,G2.1, ncol=1)
dev.off()     

CME<-IE.RESULTADO %>% dplyr::select(M.CONTROL,PROM.IE,Fay.Comp)%>%melt(id=c("M.CONTROL"))

G2 <- ggplot(CME,aes(x=value,fill=variable))+geom_density(size=2,adjust=2,alpha=0.3)+
  labs(x="")+xlim(0,1)+labs(y="Densidad",fill="")+theme_light(35)

ggsave(G2,filename = "output/Colegios/Graficas/Resultado_Mat/Densidad.jpg",width=15, height=12)


PROMEDIO.ETC<- IE.RESULTADO%>%group_by(ENTIDAD)%>%summarise(Comp.Ciencia=mean(Fay.Comp))

PROMEDIO.ETC$ENTIDAD<-toupper(PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Á","A", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("É","E", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Í","I", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Ó","O", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Ú","U", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("Ü","U", PROMEDIO.ETC$ENTIDAD)
PROMEDIO.ETC$ENTIDAD<-gsub("BOGOTA, D.C.","BOGOTA", PROMEDIO.ETC$ENTIDAD)

Diccionario<-read.delim("input/Colegio/Base/Diccionario.txt")
PROMEDIO.ETC<-merge(Diccionario,PROMEDIO.ETC,by.x="Nombre",by.y = "ENTIDAD")
write.table(PROMEDIO.ETC,file = "output/Colegios/Promedios/PROMEDIO.ETC.MATEMATICAS.txt",sep="\t",quote = FALSE,row.names = FALSE)
