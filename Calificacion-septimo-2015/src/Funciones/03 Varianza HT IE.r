###########################################################################################################
######## La función es creada para realizar el calculo de la varianza Jackknife para los estimadores ######
######## de Horvitz-Thompson (HT) y Regresión Generalizado (GREG) a nivel de escuela.                ######
###########################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 15-01 - 2016
## Subdirección de Estadística ICFES

###################################################################################################
###### Argumentos: Bade de datos de los estudaintes dentro de una ETC  ######
###################################################################################################
Var.jk.IE <- function(x,PESOS.ESTU,V_PLAUS=NULL,ee.V_PLAUS=NULL){
## Argumentos
  ## - x : data.frame que contiene las indicadora de IE (ID_INST),los pesos de estidante (PESOS.ESTU,pesos.greg),
  ##       los valores pausibles (V_PLAUS) y sus errores (ee.V_PLAUS)
  ## ee.V_PLAUS : Tipo character, que contiene los nombres de los errores de los V_PLAUS, por defector es NULL
  
  # Estiamdo el promedio mediante los estimadores Horvitz-Thompson (HT) y GREG  para una IE
  n <- nrow(x)
  if(n<6){return(data.frame(greg = cbind(PROM.IE =NA,PROM.sd=NA,JK.sd=NA,SD=NA),
                            HT   = cbind(PROM.IE =NA,PROM.sd=NA,JK.sd=NA,SD=NA)))}
  
  
  theta<-paste0("theta<-data.frame(",paste0("theta",1:length(V_PLAUS),sep="=NA",collapse = ","),")")
  eval(parse(text=theta))
  theta2<-paste0("theta2<-data.frame(",paste0("theta",1:length(V_PLAUS),sep="=NA",collapse = ","),")")
  eval(parse(text=theta2))
  
  
  if(length(ee.V_PLAUS)==1){
    bar.HT   <- sum(x[[PESOS.ESTU]]*x[,V_PLAUS])/sum(x[[PESOS.ESTU]])
    bar.HT   <- c(bar.HT,bar.HT)
    bar.greg <- sum(x[["pesos.greg"]]*x[,V_PLAUS]) /sum(x[["pesos.greg"]])
    bar.greg <- c(bar.greg,bar.greg)
    theta1<-sum(x[,"pesos.greg"]*x[,ee.V_PLAUS],na.rm = T)/sum(x[,"pesos.greg"],na.rm = T)
    theta0<-sum(x[,PESOS.ESTU]*x[,ee.V_PLAUS],na.rm = T)/sum(x[,PESOS.ESTU],na.rm = T)
    
    for (i in 1:n){
      theta[i,]<-sum(x[-i,"pesos.greg"]*x[-i,ee.V_PLAUS],na.rm = T)/sum(x[-i,"pesos.greg"],na.rm = T)
      theta2[i,]<-sum(x[-i,PESOS.ESTU]*x[-i,ee.V_PLAUS],na.rm = T)/sum(x[-i,PESOS.ESTU],na.rm = T)
    }
    
  }else{
    bar.HT   <- rowMeans((x[[PESOS.ESTU]]*x[,V_PLAUS])/sum(x[[PESOS.ESTU]]))
    bar.greg <- rowMeans((x[["pesos.greg"]]*x[,V_PLAUS])/sum(x[["pesos.greg"]]))
    
    theta1<- mean(colSums(x[,"pesos.greg"]*x[,ee.V_PLAUS],na.rm = T)/sum(x[,"pesos.greg"],na.rm = T))
    theta0<- mean(colSums(x[,PESOS.ESTU]*x[,ee.V_PLAUS],na.rm = T)/sum(x[,PESOS.ESTU],na.rm = T))
    
    for (i in 1:n){
      theta[i,]<- mean(colSums(x[-i,"pesos.greg"]*x[-i,ee.V_PLAUS],na.rm = T)/sum(x[-i,"pesos.greg"],na.rm = T))
      theta2[i,]<-mean(colSums(x[-i,PESOS.ESTU]*x[-i,ee.V_PLAUS],na.rm = T)/sum(x[-i,PESOS.ESTU],na.rm = T))
    }
  }
  
  theta1 <-matrix(rep(theta1,n),byrow = T,ncol=1) 
  theta0 <-matrix(rep(theta0,n),byrow = T,ncol=1) 
  
  a <- (n-1)/n
  diff2<-a*(theta-theta1)^2
  sd.greg=sqrt(mean(colSums(diff2))) 
  
  diff2<-a*(theta2-theta0)^2
  sd.HT=sqrt(mean(colSums(diff2))) 
  
  
  data.frame(greg=cbind(PROM.IE   =  sum(bar.greg),  # Promedio de la IE
                        PROM.sd   =  sd(bar.greg),    # Desviacion Estándar de entre las media
                        JK.sd     =  sd.greg,         # 
                        SD        =  sqrt(sd.greg^2+1.2*var(bar.greg))),
             
             HT = cbind(PROM.IE   =  sum(bar.HT),
                        PROM.sd   =  sd(bar.HT),
                        JK.sd     =  sd.HT,
                        SD        =  sqrt(sd.HT^2+1.2*var(bar.HT))))
}

############################################################################################################
######## La función es creada para realizar los calculos necesarios para aplicar la función Var.jk.IE ######
######## de Horvitz-Thompson (HT) y Regresión Generalizado (GREG) a nivel de escuela.                 ######
############################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 18-01 - 2016
## Subdirección de Estadística ICFES

###################################################################################################
###### Argumentos: Base de datos de estuciantes  ######
###################################################################################################

E.GREG.IE <- function(BD.ESTUDIANTES,PESOS.ESTU,V_PLAUS,ETC,xk,txk,...){
## Argumentos: 
  ## - BD.ESTUDIANTES: data.frame que contiene las indicadora de IE (ID_INST) y la ETC,los pesos de estidantes (PESOS.ESTU),
  ##                   los valores pausibles (V_PLAUS) y las covariables (INSE, SEXO, SECTOR, etc.)
  ## - V_PLAUS: Cadena de caracter que contiene los nombres con los cuales se identifican los valores plausibles
  ## - ETC : Tipo character, esta indica la Entidad Territorial Certificada (ETC) de interés
  ## - xk  : Cadena de caracteres que indica las covariables que se emplean en el modelo (esto es a nivel de estudiantes)
  ## - txk : data.frame que contiene los totales de las covariables "xk" por ETC. 
  ## - ... : argumentos adicionales que se deseen introducir a la función   "calib" que se utiliza para calibrar los PESOS.ESTU 

## Seleccion de la ETC de interés
  x <- BD.ESTUDIANTES%>%filter(ENTIDAD==ETC)%>%data.frame()
  tx<-txk%>%filter(ENTIDAD==ETC)%>%data.frame()
## En caso que las covariables tengan datos faltantes se realiza la imputación de estos:
  if(anyNA(x[[PESOS.ESTU]])){
    # x<-x[which(!is.na(x$PESOS.ESTU)),]   
    stop(paste0("Algunos pesos de estudiantes son NA en la ETC ",ETC))
    }
  
  if(anyNA(x[,xk])){
    warning("Las covariables (xk) tienen valores NA´s, estos serán imputados")
    imp <- mice(x[,xk],meth="norm.predict")
    ## Reemplazando valores imputados 
    x[,xk] <- complete(imp)[,xk]
  }
  
## Recalculando los pesos de los estudiantes por ETC, usando los txk.
  gkl <- calib(cbind(x[,xk]),d = x[[PESOS.ESTU]],
               total=as.numeric(tx[,xk]),...)
# Definir los pesos GREG
  x$pesos.greg<-x[[PESOS.ESTU]]*gkl 
# Estiamdo el promedio de mediante los estimadores Horvitz-Thompson (HT) y GREG 
  if(length(V_PLAUS)==1){
    bar.HT   <- (x[[PESOS.ESTU]]*x[,V_PLAUS])/sum(x[[PESOS.ESTU]])
    bar.greg <- (x[["pesos.greg"]]*x[,V_PLAUS])/sum(x[["pesos.greg"]])
  }else{
    bar.HT   <- rowMeans((x[[PESOS.ESTU]]*x[,V_PLAUS])/sum(x[[PESOS.ESTU]]))
    bar.greg <- rowMeans((x[["pesos.greg"]]*x[,V_PLAUS])/sum(x[["pesos.greg"]]))
       }
  
  
  salida   <-  data.frame(PROM.greg.ETC   = sum(bar.greg),   Sd.greg.PROM = sd(bar.greg),
                          PROM.HT.ETC     = sum(bar.HT),     Sd.HT.PROM   = sd(bar.HT))
# Calculando los errores del modelo de regresión 
  for(i in 1:length(V_PLAUS)){
  texto <- paste0("x$eepv",i,"=residuals(lm(",V_PLAUS[i],"~.-1,data=x[,c(V_PLAUS[",i,"],xk)],weights = x[['PESOS.ESTU']]))")
  eval(parse(text=texto))}
  ee.V_PLAUS<-colnames(x)[grepl("eepv",colnames(x))]
  
# Seleccionando las IE pertenecen a la ETC
  id_IE <- factor(as.character(x$ID_INST))
# Estimar la Varianza Jackknife para las IE dentro de la ETC
  sd.jk<- do.call("rbind",
                  as.list(by(id_IE,data = x,function(x)Var.jk.IE(x,V_PLAUS = V_PLAUS,
                                                                 ee.V_PLAUS=ee.V_PLAUS,
                                                                 PESOS.ESTU=PESOS.ESTU))))
  
  salida <-data.frame(ID_INST = factor(rownames(sd.jk)),salida,sd.jk)
  return(salida) 
}