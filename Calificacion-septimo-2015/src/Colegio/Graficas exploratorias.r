#########################################################################################
######## La rutina se ejecuta como un anális exploratorio de los resultados       #######
######## obtenidos en septimo                                                     #######
#########################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 17-02 - 2016
## Subdirección de Estadística ICFES

###################################################################################################
###################################################################################################
rm(list = ls())
## Definir el directorio de trabajo
dirpath <-"C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/Calificacion-septimo-2015"
#dirpath<-"C:/Users/guerr/Dropbox/investigacion icfes/SAE/SAE.git/SAE"
## Definir subcarpetas
inpath <- "/input"
outpath <- "/output"
## Fijar el directorio 
setwd(dirpath)
################################
### Libreria necesaria  ########
################################
require(reshape2)
library(dplyr)
require(ggplot2)
###########################################################
## Lectura de datos por Estudiante en el censo de IE     ##
###########################################################
## Datos de grado quinto por estudiante 
IE_censal<- read.delim(file=file.path(dirpath, inpath,"/Colegio/Covariable/Estudiantes/ResultadosSB359_2014Establecimientos.txt"),
                       colClasses="character",encoding = "UTF-8")

## Identificando las I.E. que pertenensen al censo de IE (seguimiento desde el 2009 hasta 2014)
IE_censal <- unique(IE_censal[,c("id_institucion","entidad","zona","sector")])
###########################################################
## lectura de las califiaciónes 
Septimo.2015<- read.delim(file=file.path(dirpath, inpath,"/Colegio/Base/Cal.septimo.2015.txt"),
                          colClasses=rep(c("character","numeric"),c(19,4)))


Septimo.2015 <- merge(IE_censal,Septimo.2015[,c("daneEstab","parC","parD","parL","parM","consLect","Calf.Ciencia","Calf.Comp","Calf.Leng","Calf.Mate")],
                      by.y = "daneEstab",by.x = "id_institucion")

Septimo.2015<-Septimo.2015 %>% group_by(id_institucion,entidad)%>%summarise_each(funs(mean(.,na.rm=T)),matches("Calf"))

G1<-ggplot(Septimo.2015,aes(x=Calf.Ciencia))+geom_density(size=2,adjust=2)+theme_light(20)+
  labs(x="",y="",title="Ciencias")
G2<-ggplot(Septimo.2015,aes(x=Calf.Comp))+geom_density(size=2,adjust=2)+theme_light(20)+
  labs(x="",y="",title="Competencias")
G3<-ggplot(Septimo.2015,aes(x=Calf.Leng))+geom_density(size=2,adjust=2)+theme_light(20)+
  labs(x="",y="",title="Lenguaje")
G4<-ggplot(Septimo.2015,aes(x=Calf.Mate))+geom_density(size=2,adjust=2)+theme_light(20)+
  labs(x="",y="",title="Matemáticas")
jpeg(filename = "output\\Colegios\\Graficas\\Explorar.jpeg",width = 1200,height = 1000)
gridExtra::grid.arrange(G1,G2,G3,G4)
dev.off()
dim(Septimo.2015)

#####################################################################################
Septimo.Ciencia<- read.delim(file="output/Colegios/Promedios/Resultados.IE.CIENCIA.txt",
                             sep="\t",
                          colClasses=rep(c("character","numeric"),c(3,4)))


head(Septimo.Ciencia)
Septimo.Ciencia$M.CONTROL<-ifelse(!is.na(Septimo.Ciencia$PROM.IE),"Control","Censal")
table(Septimo.Ciencia$M.CONTROL)       
Septimo.Ciencia<-melt(Septimo.Ciencia[,c("M.CONTROL","PROM.IE","Fay.Comp")],id=c("M.CONTROL"))
head(Septimo.Ciencia)
table(Septimo.Ciencia$M.CONTROL,Septimo.Ciencia$variable)
levels(Septimo.Ciencia$variable) <-c("Promedio actual","Promedio SAE")


G1<-ggplot(Septimo.Ciencia,aes(x=value,fill=as.factor(M.CONTROL)))+geom_density(alpha=0.4,size=1)+
  facet_grid(. ~ variable)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(0,1)

ggsave(G1,filename = "output/Colegios/Graficas/Resultado_Ciencia/Fay.Densidad.jpg",width=15, height=12)

G1<-ggplot(Septimo.Ciencia,aes(x=value,fill=as.factor(variable)))+geom_density(alpha=0.4,size=1)+
  facet_grid(. ~ M.CONTROL)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(0,1)

ggsave(G1,filename = "output/Colegios/Graficas/Resultado_Ciencia/Fay.Densidad2.jpg",width=15, height=12)

#####################################################################################
Septimo.Matemat<- read.delim(file="output/Colegios/Promedios/Resultados.IE.MAT.txt",
                             sep="\t",
                             colClasses=rep(c("character","numeric"),c(3,4)))

head(Septimo.Matemat)
Septimo.Matemat$M.CONTROL<-ifelse(!is.na(Septimo.Matemat$PROM.IE),"Control","Censal")
table(Septimo.Matemat$M.CONTROL)       
Septimo.Matemat<-melt(Septimo.Matemat[,c("M.CONTROL","PROM.IE","Fay.Comp")],id=c("M.CONTROL"))
head(Septimo.Matemat)
table(Septimo.Matemat$M.CONTROL,Septimo.Matemat$variable)
levels(Septimo.Matemat$variable) <-c("Promedio actual","Promedio SAE")


G1<-ggplot(Septimo.Matemat,aes(x=value,fill=as.factor(M.CONTROL)))+geom_density(alpha=0.4,size=1)+
  facet_grid(. ~ variable)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(0,1)

ggsave(G1,filename = "output/Colegios/Graficas/Resultado_Mat/Fay.Densidad.jpg",width=15, height=12)

G1<-ggplot(Septimo.Matemat,aes(x=value,fill=as.factor(variable)))+geom_density(alpha=0.4,size=1)+
  facet_grid(. ~ M.CONTROL)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(0,1)

ggsave(G1,filename = "output/Colegios/Graficas/Resultado_Mat/Fay.Densidad2.jpg",width=15, height=12)

#####################################################################################
Septimo.LENG<- read.delim(file="output/Colegios/Promedios/Resultados.IE.LENG.txt",
                             sep="\t",
                             colClasses=rep(c("character","numeric"),c(3,4)))

head(Septimo.LENG)
Septimo.LENG$M.CONTROL<-ifelse(!is.na(Septimo.LENG$PROM.IE),"Control","Censal")
table(Septimo.LENG$M.CONTROL)       
Septimo.LENG<-melt(Septimo.LENG[,c("M.CONTROL","PROM.IE","Fay.Comp")],id=c("M.CONTROL"))
head(Septimo.LENG)
table(Septimo.LENG$M.CONTROL,Septimo.LENG$variable)
levels(Septimo.LENG$variable) <-c("Promedio actual","Promedio SAE")


G1<-ggplot(Septimo.LENG,aes(x=value,fill=as.factor(M.CONTROL)))+geom_density(alpha=0.4,size=1)+
  facet_grid(. ~ variable)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(0,1)

ggsave(G1,filename = "output/Colegios/Graficas/Resultado_Lenguaje/Fay.Densidad.jpg",width=15, height=12)

G1<-ggplot(Septimo.LENG,aes(x=value,fill=as.factor(variable)))+geom_density(alpha=0.4,size=1)+
  facet_grid(. ~ M.CONTROL)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(0,1)

ggsave(G1,filename = "output/Colegios/Graficas/Resultado_Lenguaje/Fay.Densidad2.jpg",width=15, height=12)

#####################################################################################
Septimo.COMP<- read.delim(file="output/Colegios/Promedios/Resultados.IE.COMPET.txt",
                          sep="\t",
                          colClasses=rep(c("character","numeric"),c(3,4)))

head(Septimo.COMP)
Septimo.COMP$M.CONTROL<-ifelse(!is.na(Septimo.COMP$PROM.IE),"Control","Censal")
table(Septimo.COMP$M.CONTROL)       
Septimo.COMP<-melt(Septimo.COMP[,c("M.CONTROL","PROM.IE","Fay.Comp")],id=c("M.CONTROL"))
head(Septimo.COMP)
table(Septimo.COMP$M.CONTROL,Septimo.COMP$variable)
levels(Septimo.COMP$variable) <-c("Promedio actual","Promedio SAE")


G1<-ggplot(Septimo.COMP,aes(x=value,fill=as.factor(M.CONTROL)))+geom_density(alpha=0.4,size=1)+
  facet_grid(. ~ variable)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(0,1)

ggsave(G1,filename = "output/Colegios/Graficas/Resultados_Competencia/Fay.Densidad.jpg",width=15, height=12)

G1<-ggplot(Septimo.COMP,aes(x=value,fill=as.factor(variable)))+geom_density(alpha=0.4,size=1)+
  facet_grid(. ~ M.CONTROL)+theme_bw(20)+labs(x="",y="Densidad",fill="Grupo")+xlim(0,1)

ggsave(G1,filename = "output/Colegios/Graficas/Resultados_Competencia/Fay.Densidad2.jpg",width=15, height=12)

