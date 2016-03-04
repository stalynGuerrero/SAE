#########################################################################################
######## Adecuación de las bases necesarias para la calificación en las IE        #######
#########################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 15-01 - 2016
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
Septimo.2015<- read.delim(file=file.path(dirpath, inpath,"/Colegio/Base/Cal.septimo.20152.txt"),sep="\t")
Septimo.2015$DANE_ESTAB<-as.character(Septimo.2015$DANE_ESTAB)
Septimo.2015$consLect<-as.character(Septimo.2015$consLect)  
Septimo.2015$snp<-as.character(Septimo.2015$snp)  

Septimo.2015 <- merge(IE_censal,
                      Septimo.2015[,c("DANE_ESTAB","parC","parD","parL","parM","PESOS_ESTB","PesoC","PesoD","PesoL","PesoM",
                                      "consLect","Calf.Ciencia","Calf.Comp","Calf.Leng","Calf.Mate")],
                   by.y = "DANE_ESTAB",by.x = "id_institucion")


Calif.Septimo.ETC<-
   Septimo.2015%>% filter(parC==1)%>%group_by(entidad)%>%summarise(PROM.Ciencia=sum(Calf.Ciencia*54,na.rm=T)/(54*n()))%>%
   merge(Septimo.2015%>% filter(parD==1)%>%group_by(entidad)%>%summarise(PROM.Compten=sum(Calf.Comp*54,na.rm=T)/(54*n())),by="entidad")%>%
   merge(Septimo.2015%>% filter(parL==1)%>%group_by(entidad)%>%summarise(PROM.Lenguaje=sum(Calf.Leng*54,na.rm=T)/(54*n())),by="entidad")%>%
   merge(Septimo.2015%>% filter(parD==1)%>%group_by(entidad)%>%summarise(PROM.Matemat=sum(Calf.Mate*54,na.rm=T)/(54*n())),by="entidad")
head(Calif.Septimo.ETC)

#write.table(Calif.Septimo.ETC,file = "output/Colegios/Promedios/Calif.Septimo.ETC.txt",sep="\t",row.names = FALSE)

####################################################################################################
####################################################################################################
################## LAS COVARIABLES ÍNDICE SOCIOECONOMICO MUESTRA CONTROL              ##############
####################################################################################################
load(file = "input\\Colegio\\Covariable\\Estudiantes\\datSocEc2015_Septimo.rdata")
str(socSeptimo)
summary(socSeptimo)
dic <- read.csv("C:/Users/sguerrero/Desktop/dic.csv", sep=";")
for (i in 1:48){
  names(socSeptimo)[names(socSeptimo)==as.character(dic$Septimo)[i]] <- as.character(dic$Nombre)[i]
}

#socSeptimo$SNP <- paste0("0",socSeptimo$SNP)
str(Septimo.2015)
Septimo.2015$consLect
Septimo.2015<-merge(Septimo.2015,socSeptimo,by.x = "consLect",by.y = "SNP",all = T)
############################################################################################
Septimo.2015$M.CONTROL <- 1
Septimo.2015$weight <- Septimo.2015$PesoM
Septimo.2015$PesosEstab <- Septimo.2015$PESOS_ESTB
############################################################################################
Septimo.2015<-Septimo.2015%>% rename(ID_INST      = id_institucion,
                               CONSLECT      = consLect,
                              #INSE          = NV_SES2013,
                              # SEXO          = SE01,
                               ENTIDAD       = entidad,
                               ZONA          = zona,
                               SECTOR        = sector,
                               PROM.MAT      = Calf.Mate,
                               PROM.LENG     = Calf.Leng,
                               PROM.CIENCIAS = Calf.Ciencia,
                               PROM.COMPET   = Calf.Comp,
                              #EE.MAT        = Prom_ee_Mat_5,
                              #N.EST         = n,
                               PESOS.ESTU    = weight,
                               PESOS.ESTAB   = PesosEstab,
                               sanitari=sanitary
                              )
head(Septimo.2015)
xk <- c("ENTIDAD","ID_INST", "M.CONTROL","EduDad","EduMom", "floors"  , "walls","sanitari","dvd","cellphone", "fridge",
        "washMach", "heater", "microWave", "car","books")

M.CONTROL<-Septimo.2015[,c("CONSLECT","parC", "parD", "parL", "parM","PESOS.ESTU","PESOS.ESTAB","PROM.CIENCIAS", "PROM.COMPET", 
                "PROM.LENG", "PROM.MAT",xk)]
####################################################################################################
################## LAS COVARIABLES  ##################
####################################################################################################

######## Índice socioeconómico de estudiantes de grado 5 y 9  ########
load(file = file.path(dirpath,"input/Colegio/Covariable/Estudiantes/Inse_2013_v2.RData"))
## Agregando a la BD del INSE  las caracteristicas del colegio
IE_censal <- CtrlInse_2013fin%>%
  merge(IE_censal,by.y="id_institucion",by.x="daneEstab")%>%filter(grado==5)%>%dplyr::select(-grado)
IE_censal$M.CONTROL <-0
head(IE_censal)
IE_censal<-IE_censal%>%rename(ID_INST      = daneEstab,
                               ENTIDAD     = entidad,
                               sanitari    = sanitary,
                               cellphone   = cellPhone
                               )

COVARIABLES <- IE_censal[,xk]


## source(file = "src/ETC/01 Seleccion de Modelo GREG por ETC.r")

####### El resultado de la rutina anterior se obtienen los resultados 
## - Model_1 
##   - washMach
##   - car

## - Model_2
##   - car+washMach


xk <- c(colnames(COVARIABLES)[grepl("heater",colnames(COVARIABLES))],"ENTIDAD")

Tx_censal<-COVARIABLES[,xk]%>%group_by(ENTIDAD)%>%summarise_each(funs(sum))
write.table(Tx_censal,file = "input/Colegio/Base/Tx_censal.txt",sep="\t",row.names = FALSE)
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
M.CONTROL<-Septimo.2015[,c("ENTIDAD","ID_INST","parC", "parD", "parL", "parM","PESOS.ESTU","PESOS.ESTAB","PROM.CIENCIAS", "PROM.COMPET", 
                           "PROM.LENG", "PROM.MAT","heater")]

M.CONTROL$ID_INST <-as.character(M.CONTROL$ID_INST)

  M.CONTROL[,"heater"]<-paste0("heater",sep="_",M.CONTROL[,"heater"])
  M.CONTROL<-M.CONTROL%>% select(-heater)%>%cbind( TeachingSampling::Domains(M.CONTROL[,"heater"]))
  
write.table(M.CONTROL,file = "input/Colegio/Base/ESTUDIANTES.SEPTIMO.txt",sep="\t",row.names = FALSE)

