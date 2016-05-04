###########################################################################################################
######## En esta función se evaluan los posbles modelos que se pueden obtener dado un conjunto de    ######
######## variables, para ellos se toma encuenta el CME, así como el numero de datos faltantes        ######
###########################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 15-01 - 2016
## Subdirección de Estadística ICFES
###################################################################################################

mlFH<-function(Datos,Datos.PRONS,yhat,Sd.yhat,Xk,method="REML"){
  # Datos       : Base de datos que contiene la información con la cual se realizan las estimaciones
  # Datos.PRONS : Observaciones a las cuales se realizara el pronostico
  # yhat        : Nombre de la columna que contiene los valores estimados de la variable
  # Sd.yhat     : Nombre de la columna de Desviación estándar estimada para yhat
  # Xk          : Vector de caractres que contiene los nombres de las covariables 
  # method      : Método apropiado a elegir "ML", "REML" o "FH" 
  
  xk <- paste0("Datos$",Xk[-1],collapse = "+")
  model<-paste0("resultREML0<-eblupFH(as.vector(Datos$",yhat,")~",xk,",vardir=Datos$",Sd.yhat,"^2, method ='",method,"')")
  eval(parse(text=model))
  xk <- paste0("Datos.JK$",Xk[-1],collapse = "+")
  CME<-NA
  
  for(i in 1:nrow(Datos)){
    Datos.JK<- Datos[-i,]
    model<-paste0("JK0<-eblupFH(as.vector(Datos.JK$",yhat,")~",xk,",vardir=Datos.JK$",Sd.yhat,"^2, method = '",method,"')")
    eval(parse(text=model))
    CME[i]<-as.vector(as.matrix(Datos[i,Xk])%*%JK0$fit$estcoef$beta)
  }
  
  CME <- (CME-Datos[[yhat]])^2
  list(
    ESTIMA.Fay = resultREML0$eblup[,1],
    PRONS.Fay  = as.matrix(PRONOSTICO[,NOM.COV])%*%resultREML0$fit$estcoef$beta,
    CME        = sum(CME)/nrow(Datos))
  
}

