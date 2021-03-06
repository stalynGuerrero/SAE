####################################################################################
######## La presente rutina se ejecuta con el objetivo el mejor modelo que lineal ## 
######## que logre la reducción de la varianza de HT estimada con el diseño       ##
######## muestral.                                                                ##
####################################################################################

######## La base que se emplea es la Censal, la cual debe incluir las variables:  

#### Variable Identificacion: 
###  - ENTIDAD,  Nombre (o ID) de la ETC
###  - ID_INST,  Código DANE de la IE
###  - M.CONTROL, Es dicotomica, con 1 si esta en muestra control y 0 en caso contrario

#### Variable Muestreo: 
###  - PESOS.ESTU,  pesos de muestreo para el estudiante.

#### Variable respuesta: 
###  - PROM.MAT (o PROM.LENG), promedio del grado-área de interés en el año inmediatamente
###                            anterior (en nuestro caso el promedio de matemáticas de grado 5)
###  - V_PLAUS, Los valores plausibles que se emplearon para hacer el cálculo del promedio 
###             en el grado de interés.

###  Covariables:  
###  - xk, contiene el nombre de las posibles variables auxiliares que se emplean para hacer la 
###        busqueda de un modelo que logre la reducción.
#################################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 15-01 - 2016
## Subdirección de Estadística ICFES

###################################################################################################
###################################################################################################

require(sampling)

  cat("----------------------------------------------------------------------------------------------","\n")
  cat("Esta rutina selecciona el modelo con mayor R2, menor numero de datos faltantes con menor  CME,","\n",
       "empleando una regresión paso a paso","\n")
  cat("----------------------------------------------------------------------------------------------","\n")

### El primer paso es determinar el poncentaje de datos faltantes que posee la base censal
  
#############################################################
## Poncentaje de datos faltantes en el colegeio en el censo
#############################################################
PORCEN.FALTANTE<-COVARIABLES%>%dplyr::select(-ENTIDAD,-M.CONTROL)%>%
group_by(ID_INST)%>%summarise_each(funs(NO.OBS=sum(is.na(.))/n()))
write.table(PORCEN.FALTANTE,file = "output/Colegios/Covariable/PROCEN_FALTANTES_IE_CENSO.txt",
            sep="\t",quote = FALSE,row.names = FALSE)
#########################################################################
## Poncentaje de datos faltantes en el colegeio en la muestra control
#########################################################################
PORCEN.FALTANTE<-COVARIABLES%>%filter(M.CONTROL==1)%>%dplyr::select(-ENTIDAD,-M.CONTROL)%>%  
   group_by(ID_INST)%>%summarise_each(funs(NO.OBS=sum(is.na(.))/n()))
write.table(PORCEN.FALTANTE,file = "output/Colegios/Covariable/PROCEN_FALTANTES_IE_CONTROL.txt",
            sep="\t",quote = FALSE,row.names = FALSE)
#############################################################
## Poncentaje de datos faltantes en la ETC en el censo
#############################################################
PORCEN.FALTANTE<-COVARIABLES%>%dplyr::select(-ID_INST,-M.CONTROL)%>% 
   group_by(ENTIDAD)%>%summarise_each(funs(NO.OBS=sum(is.na(.))/n()))
write.table(PORCEN.FALTANTE,file = "output/Colegios/Covariable/PROCEN_FALTANTES_ETC_CENSO.txt",
            sep="\t",quote = FALSE,row.names = FALSE)
#############################################################
## Poncentaje de datos faltantes en la ETC en el censo
#############################################################
PORCEN.FALTANTE<-COVARIABLES%>%filter(M.CONTROL==1)%>%dplyr::select(-ID_INST,-M.CONTROL)%>%
  group_by(ENTIDAD)%>%summarise_each(funs(NO.OBS=sum(is.na(.))/n()))
write.table(PORCEN.FALTANTE,file = "output/Colegios/Covariable/PROCEN_FALTANTES_ETC_CONTROL.txt",
            sep="\t",quote = FALSE,row.names = FALSE)
###################################################################################################
#### Excuir las variables que identifican a la población, note que algunas de la variables empleadas
#### son de tipo categóricas por tanto se debe hacen el calculo del total para cada modalidad de la 
#### variable 
 xk<-xk[!xk%in%c("ENTIDAD","ID_INST","M.CONTROL","INSE")]
###################################################################################################

 for(i in xk){
   COVARIABLES[,i]<-paste0(i,sep="_",COVARIABLES[,i])  
 }
 
 IE_censal<-cbind(IE_censal[,!colnames(IE_censal)%in%xk],
                  do.call("cbind",apply(COVARIABLES[,xk],2,TeachingSampling::Domains)))
 
 colnames(IE_censal)<-gsub(" ","_",colnames(IE_censal))
 
###################################################################################################
 
SEL.MODEL.ETC <-function(.data,Xk,V_PLAUS=paste0("V_PLAUS",1:5)){
  if(length(Xk)==1){xk.dom <-colnames(.data)[grepl(Xk,colnames(.data))]
  }else{xk.dom <-colnames(.data)[grepl(Xk[1],colnames(.data))]
    for(i in 2:length(Xk)){
      xk.dom <-c(xk.dom,colnames(.data)[grepl(Xk[i],colnames(.data))])
    }
  }
Txk <-colSums(.data[,xk.dom])
  
  MUESTRA<-subset(.data,M.CONTROL==1)
  gkl <- calib(cbind(MUESTRA[,xk.dom]),d = MUESTRA[["PESOS.ESTU"]],
                total=as.numeric(Txk),max_iter=10000, method = "linear" )

  # Definir los pesos GREG
  MUESTRA$pesos.greg<-MUESTRA[["PESOS.ESTU"]]*gkl 
  
  MUESTRA[,xk.dom]<-apply(MUESTRA[,xk.dom], 2, factor)
  
  Resul<-paste0("Resul<-data.frame(",paste0("R2_",1:length(V_PLAUS),sep="=NA",collapse = ","),")")
  eval(parse(text=Resul))
  
  for(i in 1:length(V_PLAUS)){
       texto <- paste0("Resul$R2_",i,"=summary(lm(",V_PLAUS[i],"~.-1,data=MUESTRA[,c(V_PLAUS[",i,"],xk.dom)],weights = MUESTRA[['PESOS.ESTU']]))$r.squared")
       eval(parse(text=texto))
       texto <- paste0("MUESTRA$",V_PLAUS[i],"<-MUESTRA[['pesos.greg']]*MUESTRA$",V_PLAUS[i])
       eval(parse(text=texto))
       }
  
  PROM.MAT.IE<-MUESTRA %>%group_by(ID_INST) %>%summarise(PROM.MAT=mean(PROM.MAT,na.rm = T),
                                                         hatN=sum(pesos.greg))
  
  PROM.MAT.GREG<-MUESTRA %>%group_by(ID_INST) %>%summarise_each(funs(sum), matches("V_PLAU"))%>%
    merge(PROM.MAT.IE,by="ID_INST")
  
  xk<- rowMeans((PROM.MAT.GREG[,grepl("V_PL",colnames(PROM.MAT.GREG))]/PROM.MAT.GREG[["hatN"]]))-PROM.MAT.GREG[["PROM.MAT"]]
  
  Resul<-data.frame(SESGO.PROM=mean(xk^2,na.rm = T)/length(xk),R2_PROM=rowMeans(Resul))
  Resul
  }
 ##################################################################################################

#  ENTIDAD.MC<-IE_censal %>%filter(M.CONTROL==1&!is.na(PROM.MAT)) %>%dplyr::select(ENTIDAD)%>%unique()
#  Model.V1<-data.frame(ETC=NA,Var=NA,SESGO.PROM=NA,  R2_PROM=NA)
#  
#  set.seed(1234)
#  for(jj in 1:18){ 
#  for(i in ENTIDAD.MC$ENTIDAD){
#    IE_censal2<- IE_censal %>%filter(ENTIDAD==i)
#    Model.V1<-rbind(Model.V1,
#            cbind(ETC=i,Var=xk[jj],
#                  tryCatch(SEL.MODEL.ETC(IE_censal2,xk[jj]), 
#                           error = function(e) data.frame(SESGO.PROM=NA,  R2_PROM=NA))
#                  
#      ))
#  }
#  }
#  
#  Model.V1<-dcast(Model.V1,Var~ETC,fun.aggregate = mean, value.var = c("SESGO.PROM"))%>%
#            merge(dcast(Model.V1,Var~ETC,fun.aggregate = mean, value.var = c("R2_PROM")),by="Var")
#  
# 
#  colnames(Model.V1)<-gsub(".y","_R2.PROM",colnames(Model.V1)) 
#  colnames(Model.V1)<-gsub(".x","_SESGO.PROM",colnames(Model.V1)) 
# 
# write.table(Model.V1,file = "output/Colegios/Covariable/Modelo/Modelo de un Parametro.txt",sep="\t",quote = FALSE,row.names = FALSE)
# 
# 
#  Model.V2<-data.frame(ETC=NA,Model=NA,SESGO.PROM=NA,  R2_PROM=NA)
#  xk2 <- combn(xk2,2)
#  
#  set.seed(1234)
#  
#  for(jj in 1:ncol(xk2)){ 
#    for(i in ENTIDAD.MC$ENTIDAD){
#      IE_censal2<- IE_censal %>%filter(ENTIDAD==i)
#      Model.V2<-rbind(Model.V2,
#                      cbind(ETC=i,Model=paste0(xk2[,jj],collapse = "+"),
#                            tryCatch(SEL.MODEL.ETC(IE_censal2,xk2[,jj]), 
#                                     error = function(e) data.frame(SESGO.PROM=NA,  R2_PROM=NA))
#                            
#                      ))
#    }
#   }
#  
#  
#  Model.V2<-dcast(Model.V2,Model~ETC,fun.aggregate = mean, value.var = c("SESGO.PROM"))%>%
#    merge(dcast(Model.V2,Model~ETC,fun.aggregate = mean, value.var = c("R2_PROM")),by="Model")
#  
#  colnames(Model.V2)<-gsub(".y","_R2.PROM",colnames(Model.V2)) 
#  colnames(Model.V2)<-gsub(".x","_SESGO.PROM",colnames(Model.V2)) 
#  
# write.table(Model.V2,file = "output/Colegios/Covariable/Modelo/Modelo de dos Parametro.txt",sep="\t",quote = FALSE,row.names = FALSE)
# 
# Model_2  <- (apply(Model.V2[,2:(length(ENTIDAD.MC$ENTIDAD)+1)],1,function(x){sum(is.na(x))})/81)*100
# Model_2 <- Model.V2[Model_2 <=2,1] 
# row.names(Model.V2)[1:jj]<-Model.V2[["Model"]][1:jj]
# 
# xk2 <- sort(apply(Model.V1[,2:(length(xk)+1)],2,function(x){sum(is.na(x))})/81)*100
# 
# xk2 <- names(xk2[xk2 <=10] )
# xk2 <- gsub("_SESGO.PROM","",xk2) 
# 
# 
# Model.V3<-data.frame(ETC=NA,Model=NA,SESGO.PROM=NA,  R2_PROM=NA)
# xk2 <- combn(xk2,3)
# set.seed(1234)
# 
# for(jj in 1:ncol(xk2)){ 
#   for(i in ENTIDAD.MC$ENTIDAD){
#     IE_censal2<- IE_censal %>%filter(ENTIDAD==i)
#     Model.V3<-rbind(Model.V3,
#                     cbind(ETC=i,Model=paste0(xk2[,jj],collapse = "+"),
#                           tryCatch(SEL.MODEL.ETC(IE_censal2,xk2[,jj]), 
#                                    error = function(e) data.frame(SESGO.PROM=NA,  R2_PROM=NA))
#                           
#                     ))
#   }
#   }
# 
# Model.V3<-dcast(Model.V3,Model~ETC,fun.aggregate = mean, value.var = c("SESGO.PROM"))%>%
#   merge(dcast(Model.V3,Model~ETC,fun.aggregate = mean, value.var = c("R2_PROM")),by="Model")
# 
# colnames(Model.V3)<-gsub(".y","_R2.PROM",colnames(Model.V3)) 
# colnames(Model.V3)<-gsub(".x","_SESGO.PROM",colnames(Model.V3)) 
# 
# write.table(Model.V3,file = "output/Colegios/Covariable/Modelo/Modelo de tres Parametro.txt",sep="\t",quote = FALSE,row.names = FALSE)


cat("Revisar los archivos: ","\n")
cat(
"1) output/Colegios/Covariable/Modelo/Modelo de un Parametro.txt","\n",
"2) output/Colegios/Covariable/Modelo/Modelo de dos Parametro.txt","\n",
"3) output/Colegios/Covariable/Modelo/Modelo de tres Parametro.txt","\n",
"y seleccionar el modelo con menor CME.PROM y NUM.NA, pero con mayor R2.PROM")

