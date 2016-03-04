#########################################################################################
######## Adecuación de las bases necesarias para la calificación en las escuelas  #######
#########################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 15-01 - 2016
## Subdirección de Estadística ICFES

###################################################################################################
###################################################################################################
rm(list = ls())
## Definir el directorio de trabajo
dirpath <-"C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/Calificacion-quinto-2012"
#dirpath<-"C:/Users/guerr/Dropbox/investigacion icfes/SAE/SAE.git/SAE/Calificacion-quinto-2012"
## Definir subcarpetas
inpath <- "/input"
outpath <- "/output"
## Fijar el directorio 
setwd(dirpath)
################################
### Libreria necesaria  ########
################################
require(ggplot2)
require(reshape2)
#############################################################################
# Lectura de la base de datos sin manipular 
#############################################################################
IE_censal<- read.delim(file="output/Colegios/Promedios/Resultados.IE1.car.txt")

head(IE_censal)
IE_censal<-melt(IE_censal[,c("M.CONTROL","PROM.MAT","Fay.Comp")],id=c("M.CONTROL"))
summary(IE_censal)
table(IE_censal$M.CONTROL,IE_censal$variable)
levels(IE_censal$variable) <-c("Promedio actual","Promedio SAE")
levels(IE_censal$M.CONTROL) <-c("Control","Censo")

G1<-ggplot(IE_censal,aes(x=value,fill=as.factor(M.CONTROL)))+geom_density(alpha=0.4,size=1)+
    facet_grid(. ~ variable)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(100,500)

ggsave(G1,filename = "output/Colegios/Graficas/Resultado_covar_INSE/Modelo car/Fay.Densidad.jpg",width=15, height=12)


G1<-ggplot(IE_censal,aes(x=value,fill=as.factor(variable)))+geom_density(alpha=0.4,size=1)+
    facet_grid(. ~ M.CONTROL)+theme_bw(20)+labs(x="",y="Densidad",fill="")+xlim(100,500)

ggsave(G1,filename = "output/Colegios/Graficas/Resultado_covar_INSE/Modelo car/Fay.Densidad2.jpg",width=15, height=12)
