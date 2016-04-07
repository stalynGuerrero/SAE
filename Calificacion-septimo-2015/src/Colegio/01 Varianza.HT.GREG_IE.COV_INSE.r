#################################################################################################
######## La presente rutina se ejecuta con el objetivo de obtener las  varianza            ######
######## Jackknife para los estimadores de Horvitz-Thompson (HT) y Regresión Generalizado  ######
######## (GREG) a nivel de escuela  una vez se cuenta con la base estudiantes que          ######
######## participaron en  la prueba de saber 3579.                                         ######
######## Para ejecutar la rutina se debe contar con la siguientes variables;               ######
#################################################################################################

#### Variable Identificacion: 
# ID_INST   :	Nombre (o ID) de la ETC
# CONSLECT	: Consecutivo de la hoja de lectura
# ENTIDAD	  : Código DANE de la IE

#### Variable Muestreo: 
# PESOS.X     : Pesos de muestreo para el estudiante dada un área.
# PESOS.ESTAB : Pesos de muestreo para la IE.

#### Variable respuesta: 
# PROM.X      : Califiación en el área X

####  Covariables:  
###  - INSE, Indice socioeconómico de los estudiantes
#################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 17-03 - 2016
## Subdirección de Estadística ICFES

###################################################################################################
###################################################################################################
rm(list = ls())
## Definir el directorio de trabajo
dirpath <-"C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/SAE.git/SAE/Calificacion-septimo-2015"
## Definir subcarpetas
inpath <- "/input"
outpath <- "/output"
## Fijar el directorio 
setwd(dirpath)
################################
### Libreria necesaria  ########
################################
library(TeachingSampling)
library(reshape2)
library(dplyr)
require(mice)
require(sampling)
#########################################################
### Lectura de la base de datos de estudiantes   ########
#########################################################

ESTUDIANTES <- read.csv(file = "input/Colegio/Base/ESTUDIANTES.SEPTIMO_INSE.txt",sep="\t",header=T)
ESTUDIANTES$ID_INST<-as.character(ESTUDIANTES$ID_INST)
## Selecional el grupo de interés 
ESTUDIANTES <- ESTUDIANTES %>% filter(parM==1)%>%dplyr::select(ENTIDAD,ID_INST,PESOS.ESTAB,PesoM,PROM.MAT,INSE)
AREA="PROM.MAT"
PESO.ESTUDIANTE ="PesoM"

## Imputar datos faltantes
ESTUDIANTES<-ESTUDIANTES%>%group_by(ID_INST)%>%filter(INSE!="NaN") %>%mutate(INSE2=ifelse(is.na(INSE),mean(INSE,na.rm = T),INSE))%>%
                           dplyr::select(-INSE)%>%rename(INSE=INSE2)
###########################################################################################################
####################################
## Cálculo de t_x real ##
####################################
tx_ETC <- read.csv(file = "input/Colegio/Base/Tx_censal_INSE.txt",sep="\t",header=T)
###########################################################################################################
IE.RESULTADO<- ESTUDIANTES%>%group_by(ENTIDAD,ID_INST)%>%
                              summarise(PROM.IE=mean(PROM.MAT),
                                        Nhat= sum(PesoM))
###########################################################################################################
## Selección de las ETC que pertenencen a la muestra control
id.ETC <- unique(as.character(ESTUDIANTES$ENTIDAD))
## Función para la estimacióin de la varianza
source(file = "src/Funciones/03 Varianza HT IE.r")

##########################################################################################################

result <- data.frame(
  ID_INST=NA, PROM.greg.ETC=NA, Sd.greg.PROM=NA, PROM.HT.ETC=NA,
  Sd.HT.PROM=NA,  greg.PROM.IE=NA,greg.PROM.sd=NA, greg.JK.sd=NA,
  greg.SD=NA,HT.PROM.IE=NA, HT.PROM.sd=NA,HT.JK.sd=NA, HT.SD=NA)
set.seed(19012016)


for (i in  id.ETC){
  ## Seleccion de la ETC de interés
  x <- ESTUDIANTES%>%filter(ENTIDAD==i)%>%data.frame()
  tx<-tx_ETC%>%filter(ENTIDAD==i)%>%data.frame()
  
  result<-rbind(result,
                E.GREG.IE(BD.ESTUDIANTES = x,
                          PESOS.ESTU=PESO.ESTUDIANTE,
                          V_PLAUS = AREA,
                          ETC=i,xk =c("INSE"),tx = tx,IND ="ID_INST", 
                method="linear"))
}
result<-result[-1,]
#############################################################################################################
ls()
rm(list = c("E.GREG.IE","Var.jk.IE","i","id.ETC","tx_ETC","x","tx"))
ls()
####################################################################################################
##### ESTIMADOR COMPUESTO ######
####################################################################################################
########################
## Caso 1, cuando alhpa < 1
########################
lambda = 1
result$Med.Comp_C1 <- lambda*result$greg.PROM.IE +(1-lambda)*result$HT.PROM.IE
result$Sd.Comp_C1 <- sqrt(lambda*result$greg.SD^2 +(1-lambda)*result$HT.SD^2)
########################
## Caso 2, cuando alhpa = 1
########################
lambda = 1 
result$Med.Comp_C2 <- lambda*result$greg.PROM.IE +(1-lambda)*result$HT.PROM.IE
result$Sd.Comp_C2 <- sqrt(lambda*result$greg.SD^2 +(1-lambda)*result$HT.SD^2)
########################
## Caso 3, cuando alhpa = 1.5
########################
lambda = 1/1.5
result$Med.Comp_C3 <- lambda*result$greg.PROM.IE +(1-lambda)*result$HT.PROM.IE
result$Sd.Comp_C3 <- sqrt(lambda*result$greg.SD^2 +(1-lambda)*result$HT.SD^2)
########################
## Caso 4, cuando alhpa = 2
########################
lambda = 1/2 
result$Med.Comp_C4 <- lambda*result$greg.PROM.IE +(1-lambda)*result$HT.PROM.IE
result$Sd.Comp_C4 <- sqrt(lambda*result$greg.SD^2 +(1-lambda)*result$HT.SD^2)


IE.RESULTADO<-merge(IE.RESULTADO,result, by="ID_INST",all.x = T)

save(IE.RESULTADO, file = file.path("output/Colegios/Varianzas Estimadas/IE_2015.PROM.MATEMATICAS_INSE.RData"))
#####################################################################################
## El paso siguiente se debe realizar cuando las covariables del modelo SAE estan ###
## identificadas, en caso contrario debe ejecutar el código                       ###
## src/Colegio/02 Seleccion de modelo SAE.r                                       ###
#####################################################################################
# Variable Salida	
# ID_INST           : 	Código DANE del Establecimiento Educativo (EE)
# ENTIDAD	          :   La ETC a la cual pertenece el E.E.
# PROM.IE	          :   Promedio obtenido por el E.E. en el área de interés
# PROM.greg.ETC	    :   Promedio estimado para la ETC mediante el estimador GREG
# Sd.greg.PROM	    :   Desviación estándar del estimador GREG utilizado por en la ETC
# PROM.HT.ETC	      :   Promedio estimado para la ETC mediante el estimador Horvitz-Thompson
# Sd.HT.PROM	      :   Desviación estándar del estimador Horvitz-Thompson en la ETC

# greg.PROM.IE, 
# HT.PROM.IE	      :   Promedio estimado para el E.E. para el estimador  Horvitz-Thompson y GREG

# greg.PROM.sd,
# HT.PROM.sd	      :   Desviación estándar de los promedios estimados, esté valor diferente de cero
#                       cuando se emplea los valores plausibles para realizar la calificación, para nuestro caso es cero.

# greg.JK.sd,
# HT.JK.sd	        :   Estimación de la desviación estándar del E.E. mediante Jackknife

# greg.SD,
# HT.SD	            :   Aproximación de la varianza 

# Med.Comp_C1,
# Med.Comp_C2,      :   El promedio estimado mediante el estimador compuesto definido y 
# Med.Comp_C3,          C_i indica cada uno de los valores posibles de λ.
# Med.Comp_C4	

# Sd.Comp_C1,
# Sd.Comp_C2,
# Sd.Comp_C3,
# Sd.Comp_C4	      :  La desviación estándar del estimador compuesto para cada posible valor de λ.

## Estás variables son las que a limentan el modelo SAE 
#############################################################################################

# Resultados de promedio por IE en las pruebas Saber 11 para el 2013
source(file = "input/Colegio/Covariable/Cruce de información auxiliar.r")
# Filtrando el año 2014
SB11<-SB11[,c("DANE_ESTABLECIMIENTO",colnames(SB11)[grepl("2014",colnames(SB11))])]
apply(SB11, 2,function(x)sum(is.na(x)))              
# Resultados de promedio por IE en pruebas 3579 de años anteriores
IE_HIS<-read.delim(file=file.path(dirpath, inpath,"/Colegio/Covariable/Estudiantes/ResultadosSB359_2014Establecimientos.txt"),
                   colClasses="character",encoding = "UTF-8")
IE_HIS$promedio_2012<-as.numeric(IE_HIS$promedio_2012)
IE_HIS$promedio_2013<-as.numeric(IE_HIS$promedio_2013)
IE_HIS$promedio_2014<-as.numeric(IE_HIS$promedio_2014)
IE_censal <- unique(IE_HIS[,c("id_institucion","entidad","zona","sector")])


IE_HIS<- dcast(IE_HIS,id_institucion ~ areaNom+grado ,value.var ="promedio_2012",fun.aggregate = mean)%>%
  merge(dcast(IE_HIS,id_institucion ~ areaNom+grado ,value.var ="promedio_2013",fun.aggregate = mean,na.rm=T), by="id_institucion")%>%
  merge(dcast(IE_HIS,id_institucion ~ areaNom+grado ,value.var ="promedio_2014",fun.aggregate = mean,na.rm=T), by="id_institucion")

colnames(IE_HIS)<-toupper(colnames(IE_HIS))

colnames(IE_HIS)<-gsub(".X","_2012",colnames(IE_HIS))
colnames(IE_HIS)<-gsub(".Y","_2013",colnames(IE_HIS))
colnames(IE_HIS)<-gsub("_9$","_9_2014",colnames(IE_HIS))
colnames(IE_HIS)<-gsub("_5$","_5_2014",colnames(IE_HIS))
colnames(IE_HIS)<-gsub("_3$","_3_2014",colnames(IE_HIS))

## Identificar el número de establecimientos en cuentan con primaria y secundaria  
sum(IE_HIS$ID_INSTITUCION%in%SB11$DANE_ESTABLECIMIENTO)

##############################################################################################################################
IE_COVARIABLE<-merge(IE_HIS,SB11,by.x = "ID_INSTITUCION",by.y = "DANE_ESTABLECIMIENTO")
##############################################################################################################################
rm(list=c("IE_HIS", "SB11"))
## Uniendo las variables respues y la covariable 
IE.RESULTADO<-merge(IE.RESULTADO,IE_COVARIABLE,by.x = "ID_INST",by.y="ID_INSTITUCION",all = T)
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

