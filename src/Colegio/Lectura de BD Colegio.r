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
dirpath <-"C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/"
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
###########################################################
## Lectura de datos por Estudiante en el censo de IE     ##
###########################################################
## Datos de grado quinto por estudiante 
IE_censal<- read.delim(file=file.path(dirpath, inpath,"/Colegio/Covariable/Estudiantes/infoEstabsCensal12_13.txt"),
                       colClasses=rep(c("factor","character","factor","character","factor",
                                        "numeric","factor","numeric","NULL"),c(1,5,1,1,1,3,1,3,1)),encoding = "latin1")
## Identificando las I.E. que pertenensen al censo de IE
IE_censal <- unique(IE_censal[,c("id_institucion","entidad","zona","sector")])
###########################################################
## Lectura la BD que contiene los resultados de las IE para el año 2013
IE_2013 <- read.csv2(file = file.path(dirpath, inpath,"Colegio/Covariable/Estudiantes/2013interEstabs.txt"),sep = "\t",
                     colClasses = rep(c("character","numeric","character"),c(2,7,5)))

####################################################################################################
## Extrar los Errores de Muestro y el promedio reportado para el grado 5
IE_2013_ee<- dcast(IE_2013,id_institucion ~ prueba+grado ,value.var ="promedio",fun.aggregate = mean)%>%
  merge(dcast(IE_2013,id_institucion ~ prueba+grado ,value.var ="desviacion",fun.aggregate = mean,na.rm=T),
        by="id_institucion") %>%dplyr::select(id_institucion,Matematicas_5.x,Matematicas_5.y)%>%
  rename(Prom.Mat_5=Matematicas_5.x,Prom_ee_Mat_5=Matematicas_5.y)
## Eliminar las IE que no se les envió reporte 
IE_2013_ee<-IE_2013_ee%>%filter(!is.na(Prom_ee_Mat_5))
####################################################################################################
################## LAS COVARIABLES  ##################
####################################################################################################
IE_censal<-merge(IE_censal,IE_2013_ee,by="id_institucion")
rm(IE_2013_ee)

######## Índice socioeconómico de estudiantes de grado 5 y 9  ########
load(file = file.path(dirpath,"input/Colegio/Covariable/Estudiantes/Inse_2013_v2.RData"))

## Agregando a la BD del INSE  las caracteristicas del colegio
IE_censal <- CtrlInse_2013fin%>%
  merge(IE_censal,by.y="id_institucion",by.x="daneEstab")%>%filter(grado==5)%>%dplyr::select(-grado)

############################################################################################
## Lectura de estidiantes en muestra control prueba de matematicas en grado 5
EST_2013<-read.csv2(file=file.path(dirpath,"input/Colegio/Covariable/Estudiantes/desi2013-m5.csv"),
                    colClasses = rep(c("numeric","factor","numeric"),c(1,14,6)))

IE_censal<-merge(IE_censal,EST_2013[,c("n" ,"NoHoja","weight","score1","score2","score3","score4","score5")],
                 by.x="CONSLECT",by.y = "NoHoja",all.x = T)
############################################################################################
load("input/Colegio/Covariable/Estudiantes/datos5.RData")
IE_censal<-IE_censal%>% merge(datos[,c("id_institucion","PesosEstab")],by.y="id_institucion",by.x="daneEstab",all.x = T)

IE_censal$M.CONTROL <- ifelse(is.na(IE_censal$score1),0,1)

####################################
## valores plausibles ##
####################################
IE_censal= IE_censal %>% mutate(V_PLAUS1 = (score1*1.0174894932 + 0.0218150985) * 75.7940249552242 + 281.916493289599,
                                V_PLAUS2 = (score2*1.0188898143 + 0.0217805305) * 75.7940249552242 + 281.916493289599,
                                V_PLAUS3 = (score3*1.0159595289 + 0.0217915269) * 75.7940249552242 + 281.916493289599,
                                V_PLAUS4 = (score4*1.0172404493 + 0.0217704705) * 75.7940249552242 + 281.916493289599,
                                V_PLAUS5 = (score5*1.0159256514 + 0.0227784769) * 75.7940249552242 + 281.916493289599)%>%
  dplyr::select(-score1,-score2,-score3,-score4,-score5)
rm(list = c("CtrlInse_2013fin","datos", "EST_2013", "IE_2013" ))

IE_censal<-IE_censal%>% rename(ID_INST      = daneEstab,
                               CONSLECT      = CONSLECT,
                               INSE          = NV_SES2013,
                               SEXO         = sexo,
                               ENTIDAD       = entidad,
                               ZONA          = zona,
                               SECTOR        = sector,
                               PROM.MAT     = Prom.Mat_5,
                               EE.MAT        = Prom_ee_Mat_5,
                               N.EST         = n,
                               PESOS.ESTU    = weight,
                               PESOS.ESTAB   = PesosEstab,
                               sanitari=sanitary)
###################################################################################################
## Seleccionar mejor modelo para a nivel de ETC

xk <- c("ENTIDAD","ID_INST", "M.CONTROL","EduDad","EduMom", "floors"  , "walls","sanitari", "OverCr","dvd","cellPhone", "fridge",
        "washMach", "heater", "microWave","soundEqui" , "car","books", "SEXO","ZONA","SECTOR","INSE")
COVARIABLES <- IE_censal[,xk]

## source(file = "src/ETC/01 Seleccion de Modelo GREG por ETC.r")

####### El resultado de la rutina anterior se obtienen los resultados 
## - Model_1 
##   - washMach
##   - car

## - Model_2
##   - car+washMach


COVAR_ <- c(colnames(IE_censal)[grepl("car",colnames(IE_censal))],
           colnames(IE_censal)[grepl("washMach",colnames(IE_censal))],
           "ENTIDAD")
IE_censal<-IE_censal[,COVAR_]%>%group_by(ENTIDAD)%>%summarise_each(funs(sum))%>%merge(IE_censal,by="ENTIDAD")

colnames(IE_censal)<-gsub(".y","",colnames(IE_censal)) 
colnames(IE_censal)<-gsub(".x","_TX",colnames(IE_censal))

##################################################################################
###################################################################################################
### Nota: Para tener encuenta en la base de datos de colegios (y/o ETC)
# - Las variables iniciales con las que se trabaja la calificación son las siguientes: 
#    - ID_ETC
#    - ID_INSTITUCION 
#    - COVAR_ (EL PROMEDIO DEL AÑOS ANTERIORES EN MATEMÁTICAS, LENGUAJE,etc,INSE(O SUS VARIABLES))
#    - TX_COVAR_ (TOTAL OBTENIDO PARA CADA UNA DE LAS COVARIABLES ANTERIORES)
#    - PRMEDIO_SEP (EL PROMEDIO DEL GRADO SEPTIMO EN MATEMÁTICAS, LENGUAJE,etc)
#    - PESO_ESTAB (FACTOR DE EXPANCION DE COLEGIO DADO EL DISEÑO DE  MUESTREO)
#    - PESO_ESTUD (FACTOR DE EXPANCION DE ESTUDIANTE DADO EL DISEÑO DE  MUESTREO)
#
###################################################################################################
names(IE_censal)
IE_censal <- IE_censal[,c("ENTIDAD","ID_INST","CONSLECT","PROM.MAT","EE.MAT",
                          "PESOS.ESTU","PESOS.ESTAB","M.CONTROL","V_PLAUS1",	"V_PLAUS2",	"V_PLAUS3",
                          "V_PLAUS4",	"V_PLAUS5",COVAR_[-5],paste0(COVAR_[-5],"_TX"))]

IE_censal$ID_INST <-as.character(IE_censal$ID_INST)
write.table(IE_censal,file = "input/Colegio/Base/ESTUDIANTES.CENSAL.INSE.txt",sep="\t",row.names = FALSE)

