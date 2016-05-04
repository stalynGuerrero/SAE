###########################################################################################################
######## En esta función se evaluan los posbles modelos que se pueden obtener dado un conjunto de    ######
######## variables, para ellos se toma encuenta el CME, así como el numero de datos faltantes        ######
###########################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 15-01 - 2016
## Subdirección de Estadística ICFES
###################################################################################################

SEL.MODEL<-function(Datos,yhat,Sd.yhat,Xk,p=1,y=NULL){
# Datos    : Base de datos que contiene la información con la cual se realizan las estimaciones
# yhat     : Nombre de la columna que contiene los valores estimados de la variable
# Sd.yhat  : Nombre de la columna de Desviación estándar estimada para yhat
# Xk       : Vector de caractres que contiene los nombres de las covariables 
# p=1      : El número máximo de variables a emplear en el modelo
# y=NULL   : El valor real de la estimación en caso que se cuente con esta.
  Datos$UNOS <- 1
fit<-function(jj,...){
 
  Xk<-jj
     if(is.null(y)){Datos$IND<- ifelse(!is.na(Datos[[yhat]]),1,0)
}else{Datos$IND<- ifelse(!is.na(Datos[[y]]),1,0)}
    
    temp <-Datos[,c(yhat,Sd.yhat,y,"IND","UNOS",Xk)]
    temp$predic <- apply(temp[,c(yhat,Sd.yhat,"IND")],1,
                         function(x)ifelse(!anyNA(x[1:2])&x[3]==1,1,0))
  if(length(Xk)==1){
    temp$IMP <-      ifelse(is.na(temp[,Xk]),"SIN UN DATO","COMPLETA")
    }else{
    temp$IMP <- apply(temp[,Xk],1,
                      function(x)ifelse(anyNA(x),ifelse(sum(is.na(x))==1,
                                                        "SIN UN DATO","SIN MAS DATO"),"COMPLETA"))}
  
    MUEST.CONT <- subset(temp,predic==1&IMP=="COMPLETA")
    xk       <- paste0("MUEST.CONT$",Xk,collapse = "+")
    model    <- paste0("resultREML0<-eblupFH(as.vector(MUEST.CONT$",yhat,")~",xk,",vardir=MUEST.CONT$",Sd.yhat,"^2, method = 'REML')")
    eval(parse(text=model))
    N.CONTRL <- nrow(MUEST.CONT)
    if(is.null(y)){CME =  0
    }else{ CME <- sum((resultREML0$eblup[,1]-MUEST.CONT[[y]])^2)/N.CONTRL}
  
  # ftable(temp$IMP,temp$IND,temp$predic)
    temp2   <- subset(temp,predic==0 &IMP=="COMPLETA"& IND==1)
    if (nrow(temp2)>1&&!is.null(y)){
       PROM.IE       <- temp2[[y]]
       predic        <- as.matrix(temp2[,c("UNOS",Xk)])%*%resultREML0$fit$estcoef$beta
       N.PRONS.MC    <- nrow(temp2)
       CME_PRONS_MC  <- sqrt(sum((predic[,1]-PROM.IE)^2,na.rm = T)/N.PRONS.MC)
    }else{CME_PRONS_MC=0; N.PRONS.MC=0}
  
    temp2   <- subset(temp,IMP=="SIN UN DATO"&IND==1&predic==0)
    if (nrow(temp2)>5&&!is.null(y)){
       PROM.IE      <- temp2[[y]]
       N.PRONS.IMPU  <- nrow(temp2)
       imput <- tryCatch(mice(temp2[,Xk],method = "norm.boot"),error = function(e) NULL)
       if(is.null(imput)||anyNA(complete(imput))){CME_PRONS_IMPU<-NA
       }else{temp2[,Xk] <- complete(imput)
        predic <- as.matrix(temp2[,c("UNOS",Xk)])%*%resultREML0$fit$estcoef$beta
        CME_PRONS_IMPU <- sqrt(sum((predic[,1]-PROM.IE)^2)/N.PRONS.IMPU)}    
    }else{N.PRONS.IMPU=0;CME_PRONS_IMPU=0}
  
  c(resultREML0$fit$goodness[1:4],
    ### MUESTRA CONTROL
    CME.CONTRL=sqrt(CME),N.CONTRL=N.CONTRL,
    ### PRONOSTICO SIN IMPUTAR
    CME_PRONS_MC=CME_PRONS_MC,N.PRONS.MC=N.PRONS.MC,
    ### PRONOSTICO CON IMPUTAR
    CME_PRONS_IMPU=CME_PRONS_IMPU,N.PRONS.IMPU=N.PRONS.IMPU)
}

Resustado<-list()
for(i in 1:p){
Cov.xk <-data.frame(Model=t(combn(Xk,i)))
CME <-t(apply(Cov.xk, 1,function(jj){fit(jj,Datos,yhat,Sd.yhat,y)}))
if(p>1){Cov.xk <- apply(Cov.xk,2,as.character)
        Cov.xk <- apply(Cov.xk,1,function(x)paste0(x,collapse = "+"))
        Cov.xk <- data.frame(Model=Cov.xk)
}
Resustado[[i]]<- cbind(Cov.xk,CME)
}
do.call("rbind",Resustado)
}
