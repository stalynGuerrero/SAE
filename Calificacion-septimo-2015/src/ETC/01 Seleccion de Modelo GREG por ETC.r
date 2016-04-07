###########################################################################################
##### En la rutina se hace la selección del modelo más adecuado a nivel de estudiantes   ## 
##### que se deben emplear para calcular la varianza de un EE que pertenece a la muestra ##
##### control                                                                            ##
###########################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 16-03 - 2016
## Subdirección de Estadística ICFES

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
require(sampling)
###################################################################
# Lectura de la base de datos de la calificasión de los estudiantes 
# de grado séptimo, esta debe contar con: 
# ID_INST  : El identificador del EE 
# ENTIDAD  : El identificador de la entidad
# CONSLECT : El identificador del estudiante, en caso que las covariables procedan de otra base 
# PROM     : El promedio del área de interés
# PESO_EST : El peso del estudiante dada un área de interés
# Xk       ; Vector de posibles covariables
###################################################################
## Lectura de la base de calificación 
Data.base <- read.table(file = "input/Colegio/Base/ESTUDIANTES.SEPTIMO_INSE.txt",
                        sep="\t",header=TRUE,
                        colClasses = rep(c("character","numeric","character","numeric"),c(9,9,1,1)))
## Seleccion del área de interés
Data.base<-Data.base%>%filter(parC==1)%>%dplyr::select(ID_INST,ENTIDAD,CONSLECT,PesoC,PROM.CIENCIAS,INSE)
###################################################################
# Lectura de las covariables 
load(file = "input/Colegio/Covariable/Estudiantes/datSocEc2015_Septimo2.rdata")
# seleccón de los nombres covariables
xk <- names(socSeptimo)[-c(1:3)]
xk <- c("INSE",xk[-c(11,12)])
# Relacionar las calificaciones con las covariables 
Data.base<-merge(Data.base,socSeptimo[,-1],by.x = c("ID_INST","CONSLECT"),by.y = c("INSTITUCION","SNP"))

#####################################################################
################## LAS COVARIABLES DE CALIBRACIÓN  ##################
#####################################################################
## Lectura de datos del censo de IE 
IE_censal<- read.delim(file=file.path(dirpath, inpath,"/Colegio/Covariable/Estudiantes/ResultadosSB359_2014Establecimientos.txt"),
                       colClasses="character",encoding = "UTF-8")%>%
                       dplyr::select(id_institucion,entidad,zona,sector)%>%unique()
## Índice socioeconómico de estudiantes de grado 5 y 9 para el año 2013
load(file = file.path(dirpath,"input/Colegio/Covariable/Estudiantes/Inse_2013_v2.RData"))
## Agregando a la BD del INSE  las caracteristicas del colegio
IE_censal <-  merge(IE_censal,CtrlInse_2013fin,by.x="id_institucion",by.y="daneEstab")%>%
  filter(grado==5)%>%
  dplyr::select(-grado)%>%rename(ID_INST     = id_institucion,
                                 ENTIDAD     = entidad,
                                 INSE        = NV_SES2013,
                                 gender      = sexo,
                                 cellphone   = cellPhone)
## Selección de variales que tienen información historica
IE_censal<-IE_censal[,names(IE_censal)%in%c("ENTIDAD","ID_INST",xk)]
xk<-names(IE_censal)[-c(1:2)]
Data.base<-Data.base[,names(Data.base)%in%c("ID_INST","ENTIDAD","PesoC","PROM.CIENCIAS",xk)]

#####################################################################
## Cambio de nombre a las modalidades de las  variables categoricas
  for(i in xk[-13]){
    IE_censal[,i]<-paste0(i,sep="_",IE_censal[,i])  
    Data.base[,i]<-paste0(i,sep="_",Data.base[,i])   
    }
## Obtenido las indicadoras por cada modalidad

COVARIABLES<-cbind(IE_censal[,!colnames(IE_censal)%in%xk],
                   do.call("cbind",apply(IE_censal[,xk[-13]],2,TeachingSampling::Domains)))

M.CONTROL<-cbind(Data.base[,!colnames(Data.base)%in%xk],
                     do.call("cbind",apply(Data.base[,xk[-13]],2,TeachingSampling::Domains)))
## Agregando variable continua 
COVARIABLES$INSE<-IE_censal$INSE
M.CONTROL$INSE<-Data.base$INSE 
## Eliminar espacio de los nombres de columnas 
colnames(COVARIABLES)<-gsub(" ","_",colnames(COVARIABLES))
colnames(M.CONTROL)<-gsub(" ","_",colnames(M.CONTROL))

## Calculo de los totales de calibración por ETC
Tx_censal<-COVARIABLES[,-1]%>%group_by(ENTIDAD)%>%summarise_each(funs(sum))

## Cambiar y eliminar nombres que no coinciden.
M.CONTROL<-M.CONTROL%>%rename(floors_NA = floors_NR,
                              walls_NA  = walls_NR, 
                            sanitary_NA = sanitary_NR,
                            books_NA    = books_NR,
                            gender_NA   = gender_NR)%>%
                           dplyr::select(-dvd_NR,-cellphone_NR,-fridge_NR,-washMach_NR,
                         -heater_NR,-microWave_NR,-car_NR)

## Eliminar archivos de lamemoria de R
rm(list = c("COVARIABLES","IE_censal","CtrlInse_2013fin","Data.base",  "i",
            "socSeptimo"))
############################################################
####### REPORTE DESCRIPTIVO DE LAS COVARIABLES #############
############################################################

#########################################################################
## Poncentaje de datos faltantes en el colegeio en la muestra control
#########################################################################
M.CONTROL%>%group_by(ID_INST)%>%summarise(INSE=round(sum(is.na(INSE))/n(),2))%>%
  arrange(desc(INSE))%>%filter(INSE>0)%>%data.frame()
#############################################################
## Poncentaje de datos faltantes en la ETC en el censo
#############################################################
M.CONTROL%>%group_by(ENTIDAD)%>%summarise(INSE=round(sum(is.na(INSE))/n(),2))%>%
  arrange(desc(INSE))%>%filter(INSE>0)%>%data.frame()
###################################################################################################
# Dado que el porcentaje de faltantes en la muestra control es alta se debe imputar los faltantes 
# Esto lo haremos con el promedio del INSE en la institución

M.CONTROL<-M.CONTROL%>%group_by(ID_INST)%>%mutate(INSE = ifelse(is.na(INSE),mean(INSE,na.rm = T),INSE))

sel<-M.CONTROL%>%group_by(ID_INST)%>%summarise(INSE=round(sum(is.na(INSE))/n(),2))%>%
     arrange(desc(INSE))%>%filter(INSE<1)%>%data.frame()

M.CONTROL<-subset(M.CONTROL,M.CONTROL$ID_INST%in%sel$ID_INST)

###################################################################################################
#  Xk <-"floors"
#  M_CONTROL <-M.CONTROL
#  Txk <- Tx
#  PROM.ESTU <- "PROM.CIENCIAS"
#  PESO.ESTU <- "PesoC"
source("src/Funciones/SEL.MODEL.ETC.r")
 ##################################################################################################
## Filtrar las ETC que estan en la base
 ENTIDAD.MC<-M.CONTROL%>%dplyr::select(ENTIDAD)%>%unique()
## La base de salida
 Model.V1<-data.frame(ETC=NA,Var=NA,R2_1=NA,CME.GREG1=NA)

 set.seed(1234)
 for(jj in xk){ 
 for(i in unique(ENTIDAD.MC$ENTIDAD)){
   M_CONTROL <- M.CONTROL   %>% filter(ENTIDAD==i)%>%data.frame()
   
   Tx        <- Tx_censal   %>% filter(ENTIDAD==i)%>%data.frame()
   Tx<-as.data.frame(Tx[,grepl(jj,colnames(Tx))])
   Xk <- names(Tx)
   
   Model.V1<-rbind(Model.V1,
           cbind(ETC=i,Var=jj,
                 tryCatch(SEL.MODEL.ETC(M_CONTROL,Txk=Tx,Xk=Xk,PROM.ESTU="PROM.CIENCIAS",
                                        PESO.ESTU="PesoC",ID_INST="ID_INST",
                                        max_iter=10000, method = "logit"), 
                          error = function(e) data.frame(R2_1=NA,CME.GREG1=NA))
                 
     ))
 }
 }
 
 Model.V1<-dcast(Model.V1,Var~ETC,fun.aggregate = mean, value.var = c("CME.GREG1"))%>%
           merge(dcast(Model.V1,Var~ETC,fun.aggregate = mean, value.var = c("R2_1")),by="Var")
 

 colnames(Model.V1)<-gsub(".y","_R2.PROM",colnames(Model.V1)) 
 colnames(Model.V1)<-gsub(".x","_CME",colnames(Model.V1)) 
 
 Model.V1.Resumen<- 
  data.frame(Model.V1$Var, PRO_CME=apply(Model.V1[,grepl("_CME",colnames(Model.V1))],1,mean,na.rm=T),
            PRO_R2=apply(Model.V1[,grepl("_R2.PROM",colnames(Model.V1))],1,mean,na.rm=T),
            NumFal=apply(Model.V1[,grepl("_CME",colnames(Model.V1))],1,function(x)sum(is.na(x))))
 
write.table(Model.V1,file = "output/Colegios/Covariable/Modelo/Competencia/Modelo de un Parametro_INSE.txt",sep="\t",quote = FALSE,row.names = FALSE)
write.table(Model.V1.Resumen,file = "output/Colegios/Covariable/Modelo/Competencia/Model.V1.Resumen_INSE.txt",sep="\t",quote = FALSE,row.names = FALSE)
###################################################################################################

Model.V2<-data.frame(ETC=NA,Model=NA,R2_1=NA,CME.GREG1=NA)
 xk2 <- combn(xk,2)
 set.seed(1234)
 for(jj in 1:78){ 
   for(i in ENTIDAD.MC$ENTIDAD){
     M_CONTROL <- M.CONTROL   %>% filter(ENTIDAD==i)%>%data.frame()
     
     Tx        <- Tx_censal   %>% filter(ENTIDAD==i)%>%data.frame()
     xk2[1,jj]
     Tx<-data.frame(Tx[,grepl(xk2[1,jj],colnames(Tx))],
                       Tx[,grepl(xk2[2,jj],colnames(Tx))])
     Xk <- names(Tx)
     
     Model.V2<-rbind(Model.V2,
                     cbind(ETC=i,Model=paste0(xk2[,jj],collapse = "+"),
                           tryCatch(SEL.MODEL.ETC(M_CONTROL,Txk=Tx,Xk=Xk,PROM.ESTU="PROM.CIENCIAS",
                                                  PESO.ESTU="PesoC",ID_INST="ID_INST",
                                                  max_iter=10000, method = "logit"), 
                                    error = function(e) data.frame(R2_1=NA,CME.GREG1=NA))   
                     ))
   }
 }
 
 Model.V2<-dcast(Model.V2,Model~ETC,fun.aggregate = mean, value.var = c("CME"))%>%
 merge(dcast(Model.V2,Model~ETC,fun.aggregate = mean, value.var = c("R2_PROM")),by="Model")
 
 colnames(Model.V2)<-gsub(".y","_R2.PROM",colnames(Model.V2)) 
 colnames(Model.V2)<-gsub(".x","_CME",colnames(Model.V2)) 
 
 Model.V2.Resumen<- 
   data.frame(Model.V2$Model, PRO_CME=apply(Model.V2[,grepl("_CME",colnames(Model.V2))],1,mean,na.rm=T),
              PRO_R2=apply(Model.V2[,grepl("_R2.PROM",colnames(Model.V2))],1,mean,na.rm=T),
              NumFal=apply(Model.V2[,grepl("_CME",colnames(Model.V2))],1,function(x)sum(is.na(x))))
 
 
write.table(Model.V2,file = "output/Colegios/Covariable/Modelo/Matematicas/Modelo de dos Parametro.txt",sep="\t",quote = FALSE,row.names = FALSE)
write.table(Model.V2.Resumen,file = "output/Colegios/Covariable/Modelo/Matematicas/Model.V2.Resumen.txt",sep="\t",quote = FALSE,row.names = FALSE)



