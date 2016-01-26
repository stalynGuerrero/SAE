###################################################################################################
######## Adecuación de las bases necesarias para la calificación en las escuelas y por ETC ########
###################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 14-01 - 2016
## Subdirección de Estadística ICFES

###################################################################################################
###################################################################################################
rm(list = ls())
## Definir el directorio de trabajo
dirpath <- "C:/Users/sguerrero/Dropbox/investigacion icfes/SAE/Resultados SAE"
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
###################################################################################################
### Nota: Para tener encuenta en la base de datos de colegios (y/o ETC)
# - Las variables iniciales con las que se trabaja la calificación son las siguientes: 
#    - ID_ETC
#    - ID_INSTITUCION 
#    - ID_ESTRATOS (ESTÉ SE OBTINE DEL DISEÑO MUESTRAL)
#    - COVAR_ (EL PROMEDIO DEL AÑOS ANTERIORES EN MATEMÁTICAS, LENGUAJE,etc,INSE(O SUS VARIABLES))
#    - TX_COVAR_ (TOTAL OBTENIDO PARA CADA UNA DE LAS COVARIABLES ANTERIORES)
#    - PRMEDIO_SEP (EL PROMEDIO DEL GRADO SEPTIMO EN MATEMÁTICAS, LENGUAJE,etc)
#    - PESO_ESTAB (FACTOR DE EXPANCION DE COLEGIO DADO EL DISEÑO DE  MUESTREO)
#    - PESO_ESTUD (FACTOR DE EXPANCION DE ESTUDIANTE DADO EL DISEÑO DE  MUESTREO)
#
# - Validar resultados con muestra control 
###################################################################################################

###########################################################
## Lectura de datos por Estudiante en la muestra control ##
###########################################################
## Datos de grado quinto por estudiante 
ESTUDIANTES<-read.csv2(file.path(dirpath,"input/ETC/Covariable", "Estudiante5_2013.csv"),sep=";",dec=",")
# Se hace id_institucion un factor: BD de grado quinto por estudiante 
ESTUDIANTES[["establecimiento"]] <- as.factor(ESTUDIANTES[["establecimiento"]])
######################################################################################
## valores plausibles, en el caso que no se tenga el promedio del grado de interés ##
######################################################################################
ESTUDIANTES=cbind(ESTUDIANTES,
                  V_PLAUS1 = (ESTUDIANTES[["score1"]]*1.0174894932 + 0.0218150985) * 75.7940249552242 + 281.916493289599,
                  V_PLAUS2 = (ESTUDIANTES[["score2"]]*1.0188898143 + 0.0217805305) * 75.7940249552242 + 281.916493289599,
                  V_PLAUS3 = (ESTUDIANTES[["score3"]]*1.0159595289 + 0.0217915269) * 75.7940249552242 + 281.916493289599,
                  V_PLAUS4 = (ESTUDIANTES[["score4"]]*1.0172404493 + 0.0217704705) * 75.7940249552242 + 281.916493289599,
                  V_PLAUS5 = (ESTUDIANTES[["score5"]]*1.0159256514 + 0.0227784769) * 75.7940249552242 + 281.916493289599)
##########################################################################################
#### $$   Sección: Adecuar BD  $$ ####  
##########################################################################################
### Adicionando los promedios de 3 y 5 para realizar imputación múltiple 
temp <- temp %>% merge(datos[,c("id_institucion","promedio_5","promedio_3")],by.x="establecimiento",by.y="id_institucion",all.x = T)
### Adicionando los promedios (Grados 3 y 5) calculados a la BD de estudiantes 
plaus <- merge(datos2,temp[,-2],by.y ="establecimiento",by.x="establecimiento")

### Verificando si el número de I.E es el mismo en la BD consolidada por institución y en la BD de estudiantes

length(unique(datos$entidad)) ## Número de entidades en la BD de instituciones 
length(unique(plaus$Nombre))  ## Número de entidades en la BD de estudiantes

### Entidades en las que se difiere 
unique(datos2$Nombre)[!unique(datos2$Nombre)%in%unique(datos$entidad)]

# Agregando los pesos a la base de datos consolidada
plaus$pesos<-datos2$weight


####################################
## Cálculo de t_x real ##
####################################

##### Censal: históricos de I.E
datos5_censal<- read.delim(file=file.path(ruta,"/Datos/XLS_CSV/infoEstabsCensal12_13.txt"),
                           colClasses=rep(c("factor","character","factor","character","factor",
                                            "numeric","factor","numeric","NULL"),c(1,5,1,1,1,3,1,3,1)),encoding = "latin1")
IE_His <- datos5_censal
IE_His$entidad[IE_His$entidad== "Bogotá, D.C."] <-  "Bogotá, D,C,"

## Suma de promedios grado tercero para Matemáticas y Lenguaje por entidad(tx real)
tx <-  IE_His %>%group_by(entidad)%>%filter(grado==3,areaNom=="Matemáticas") %>%
  summarise(N_Mat = sum(n_2013,na.rm=T),prom_mat =mean(promedio_2013,na.rm = T))

tx <- IE_His %>%group_by(entidad)%>%filter(grado==3,areaNom=="Lenguaje") %>%
  summarise(N_Leng = sum(n_2013,na.rm=T),prom_leng=mean(promedio_2013,na.rm=T))%>%
  merge(tx,all.y = T,by="entidad") %>% mutate(N_Est=N_Leng+ N_Mat,T_Prom =(N_Leng+ N_Mat)*prom_mat )

## Selección de las variables de interés en BD Censal
tx <- tx[,c("entidad","N_Est","T_Prom")]

## Estimación de tx_pi por variable auxiliar y por ETC
colnames(plaus)[colnames(plaus)=="Nombre"]<-"entidad"
tx_pi <-plaus%>%group_by(entidad) %>%summarise(N = sum(pesos,na.rm=T),Tx = sum(pesos*promedio_3,na.rm = T))

## Observando la diferencia en entre tx y tx_pi
colSums(tx[,-1])-colSums(tx_pi[,-1])

## Selección de variables de trabajo en muestra control
plaus <-plaus %>%merge(tx, all.x = T,by="entidad")%>%dplyr::select(establecimiento,estrato,entidad,pesos,N_Est,T_Prom,promedio_5,promedio_3,pv1,pv2,pv3,pv4,pv5)
temp.plaus <- plaus

#################################################
## Imputación y ecuación de regularidad ##
#################################################

## Imputando datos en la BD general por estudiante
set.seed(09112015)
imp <- mice(plaus[,c("promedio_5","promedio_3")],meth="norm.predict")

## Reemplazando valores imputados 
temp.plaus[,c("promedio_5","promedio_3")] <- complete(imp)[,c("promedio_5","promedio_3")]

## Recalculando pesos utilizado BD general por estudiantes completa
gkl <- calib(cbind(1,temp.plaus[["promedio_3"]]),d = temp.plaus[["pesos"]],
             total=as.numeric(colSums(tx[,-1])),
             method="logit",bounds=c(0,10),max_iter=1000)
pesos.greg<-temp.plaus[["pesos"]]*gkl 

## Evualando la ecuación de regularidad
round(pesos.greg%*%cbind(1,temp.plaus[["promedio_3"]]) -as.numeric(colSums(tx[,-1])))

## tx_pi con datos imputados 
tx_pi2 <-temp.plaus%>%group_by(entidad) %>%summarise(N = sum(pesos,na.rm=T),Tx = sum(pesos*promedio_3,na.rm = T))

## Observando la diferencia en entre tx y tx_pi2 (Sin y con imputación)
(colSums(tx[,-1])-colSums(tx_pi[,-1]))
(colSums(tx[,-1])-colSums(tx_pi2[,-1]))
temp.plaus$pesos.greg <-pesos.greg
###########################################################################################
################# Enfrentando el problema desde dos puntos de vistas diferentes ###########
###########################################################################################
## Con los datos imputados se determina los errores del modelo lineal ajustado para cada ##
##            valor plausible y se ejecuta JK de forma analoga al HT                     ##
#####      #####   #####    #####     #####      #####   #####    #####      #####   ######  

bar.greg <- function(plaus){
  bar.greg <- colSums(plaus[["pesos.greg"]]*plaus[,c("pv1","pv2","pv3","pv4","pv5")])/sum(plaus[["pesos.greg"]])
  bar.HT <- colSums(plaus[["pesos"]]*plaus[,c("pv1","pv2","pv3","pv4","pv5")])/sum(plaus[["pesos"]])
  
  return(
    round(c(Media.greg= mean(bar.greg),Sd.greg.media =sd(bar.greg),
            Media.HT= mean(bar.HT),Sd.HT.media =sd(bar.HT),
            Dif_GREG_HT = mean(bar.greg)-mean(bar.HT)),2))
}

bar.greg(temp.plaus)

SAE.Greg <- data.frame( do.call("rbind",by(temp.plaus,INDICES = temp.plaus$entidad,bar.greg)))

## Aplicando un Modelo Lineal a la base de datos completa 
temp.plaus$pv1 <- as.numeric(residuals(lm(pv1~promedio_3,data=temp.plaus)))
temp.plaus$pv2 <- as.numeric(residuals(lm(pv2~promedio_3,data=temp.plaus)))
temp.plaus$pv3 <- as.numeric(residuals(lm(pv3~promedio_3,data=temp.plaus)))
temp.plaus$pv4 <- as.numeric(residuals(lm(pv4~promedio_3,data=temp.plaus)))
temp.plaus$pv5 <- as.numeric(residuals(lm(pv5~promedio_3,data=temp.plaus)))
### Descriptiva de los residuales 
round(temp.plaus%>%summarise(n=n(),N_IE = length(unique(establecimiento)),bar1=sum(pv1),
                             bar2=sum(pv2),bar3=sum(pv3),bar4=sum(pv4),bar5=sum(pv5)))

## Cargando la funciÃ³n de JK para HT
source(file = file.path(ruta,"R codes/Estimacion Varianza/Varianza_HT.r"))
## Estimación de la varianza JK para el estimador GREG
SAE.Greg$Sd.JK <- do.call("rbind",by(temp.plaus,INDICES = temp.plaus$entidad,function(x)Media.HT(x)))[,5]
## Sd de la estimadar GREG
SAE.Greg$Sd.greg <- sqrt(SAE.Greg$Sd.JK^2+1.2*SAE.Greg$Sd.greg^2)
## Agregando nombre de las ETC
SAE.Greg[,"ETC"] <- rownames(SAE.Greg)
## Estimación de la varianza para el estimador de HT
SAE.HT<- data.frame(do.call("rbind",by(plaus,INDICES = plaus$entidad,function(x)Media.HT(x))))
## Agregando nombre de las ETC
SAE.HT[,"ETC"] <- rownames(SAE.HT)
### merge entre GREG y HT
xx<-merge(SAE.Greg,SAE.HT,by.y ="ETC")
### Diferencia entre HT - GREG
xx$dif_SD <-xx$Sd-xx$Sd.greg
### Ordenar la variable por # de establecimientos en la ETC
xx<-xx[order(xx$nEstabs,decreasing = T),]
### Entidades que disminuyen su varianza 
sum(xx$dif_SD>0,na.rm = T)
subset(xx[,c("ETC","nEstabs", "Dif_GREG_HT","Sd","Sd.greg", "dif_SD")],dif_SD>0&nEstabs>2)
### 
mean(subset(xx[,c("ETC","nEstabs", "Dif_GREG_HT","Sd","Sd.greg", "dif_SD")],dif_SD>0&nEstabs>2)[["dif_SD"]])
###
plot(xx$Media.greg,xx$Media.HT)

### Entidades que aumentan su varianza 
sum(xx$dif<0,na.rm = T)
subset(xx[,c("ETC","nEstabs", "Dif_GREG_HT","Sd","Sd.greg", "dif_SD")],dif_SD<0&nEstabs>2)

ETC.REAL <-read.csv2(file= file.path(ruta,"/Datos/XLS_CSV/censalSABER2013_ET.csv"),
                     comment.char="#",encoding = "latin1")

xx$ETC[xx$ETC=="Bogotá, D,C,"] <- "Bogotá, D.C."


xx<-merge(xx,ETC.REAL,by.x="ETC",by.y = "Entidad.Territorial")
X11()
plot(xx$PromCensal5 -xx$Media.greg)
abline(h=0)
X11()
plot(xx$PromCensal5 -xx$Media.HT)
abline(h=0)


Estimaciones.Greg <- xx[,c("ETC","nEstabs","Media.greg","Media.HT","Sd.greg","Sd","dif_SD")] 

save(Estimaciones.Greg, file = file.path(ruta,"/Datos/Rdata/Estimacion.Greg.rdata"))


