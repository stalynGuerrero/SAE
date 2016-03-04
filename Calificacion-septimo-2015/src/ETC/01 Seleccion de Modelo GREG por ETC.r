require(sampling)

  cat("----------------------------------------------------------------------------------------------","\n")
  cat("Esta rutina selecciona el modelo con mayor R2 y CME, empleando una regresión paso a paso","\n")
  cat("----------------------------------------------------------------------------------------------","\n")

#############################################################
## Poncentaje de datos faltantes en el colegeio en el censo
#############################################################
PORCEN.FALTANTE<-COVARIABLES%>%dplyr::select(-ENTIDAD,-M.CONTROL)%>%
group_by(ID_INST)%>%summarise_each(funs(NO.OBS=sum(is.na(.))/n()))
write.table(PORCEN.FALTANTE,file = "output/Colegios/Covariable/PROCEN_FALTANTES_IE_CENSO.txt",sep="\t",quote = FALSE,row.names = FALSE)
#########################################################################
## Poncentaje de datos faltantes en el colegeio en la muestra control
#########################################################################
PORCEN.FALTANTE<-M.CONTROL%>%dplyr::select(-ENTIDAD,-M.CONTROL)%>%  
   group_by(ID_INST)%>%summarise_each(funs(NO.OBS=sum(is.na(.))/n()))
write.table(PORCEN.FALTANTE,file = "output/Colegios/Covariable/PROCEN_FALTANTES_IE_CONTROL.txt",sep="\t",quote = FALSE,row.names = FALSE)
#############################################################
## Poncentaje de datos faltantes en la ETC en el censo
#############################################################
PORCEN.FALTANTE<-COVARIABLES%>%dplyr::select(-ID_INST,-M.CONTROL)%>% 
   group_by(ENTIDAD)%>%summarise_each(funs(NO.OBS=sum(is.na(.))/n()))
write.table(PORCEN.FALTANTE,file = "output/Colegios/Covariable/PROCEN_FALTANTES_ETC_CENSO.txt",sep="\t",quote = FALSE,row.names = FALSE)
#############################################################
## Poncentaje de datos faltantes en la ETC en el censo
#############################################################
PORCEN.FALTANTE<-M.CONTROL%>%dplyr::select(-ID_INST,-M.CONTROL)%>%
  group_by(ENTIDAD)%>%summarise_each(funs(NO.OBS=sum(is.na(.))/n()))
write.table(PORCEN.FALTANTE,file = "output/Colegios/Covariable/PROCEN_FALTANTES_ETC_CONTROL.txt",sep="\t",quote = FALSE,row.names = FALSE)
###################################################################################################
 xk<-xk[!xk%in%c("ENTIDAD","ID_INST","M.CONTROL")]
###################################################################################################

 for(i in xk){
   COVARIABLES[,i]<-paste0(i,sep="_",COVARIABLES[,i])  
   M.CONTROL[,i]<-paste0(i,sep="_",M.CONTROL[,i])   
   }

COVARIABLES<-cbind(COVARIABLES[,!colnames(COVARIABLES)%in%xk],
                  do.call("cbind",apply(COVARIABLES[,xk],2,TeachingSampling::Domains)))
 
 colnames(COVARIABLES)<-gsub(" ","_",colnames(COVARIABLES))
 ###################################################################################################
 M.CONTROL<-cbind(M.CONTROL[,!colnames(M.CONTROL)%in%xk],
                    do.call("cbind",apply(M.CONTROL[,xk],2,TeachingSampling::Domains)))
 
 colnames(M.CONTROL)<-gsub(" ","_",colnames(M.CONTROL))
 
  
###################################################################################################
#  Xk <-c("EduDad")
#  M_CONTROL <-M.CONTROL
#  X.COV<- COVARIABLES
#  V_PLAUS <- "PROM.MAT"
SEL.MODEL.ETC <-function(M_CONTROL,X.COV,Xk,V_PLAUS=paste0("V_PLAUS",1:5)){
  if(length(Xk)==1){xk.dom <-colnames(X.COV)[grepl(Xk,colnames(X.COV))]
  }else{xk.dom <-colnames(X.COV)[grepl(Xk[1],colnames(X.COV))]
    for(i in 2:length(Xk)){
      xk.dom <-c(xk.dom,colnames(X.COV)[grepl(Xk[i],colnames(X.COV))])
    }
  }
Txk <-colSums(X.COV[,xk.dom])
  
  MUESTRA<- subset(M_CONTROL,parM==1)
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
       texto <- paste0("MUESTRA$",V_PLAUS[i],".1<-MUESTRA[['pesos.greg']]*MUESTRA$",V_PLAUS[i])
       eval(parse(text=texto))
       }
  
  PROM.CIENCIAS<-MUESTRA %>%group_by(ID_INST) %>%summarise(PROM.CIENCIAS=mean(PROM.MAT,na.rm = T),
                                                           PROM.CIENCIAS.GREG =sum(PROM.MAT.1)/sum(pesos.greg))
    xk<-abs(PROM.CIENCIAS$PROM.CIENCIAS-PROM.CIENCIAS$PROM.CIENCIAS.GREG)
  

  Resul<-data.frame(CME=sum(xk^2,na.rm = T),R2_PROM=rowMeans(Resul))
  Resul
  }
 ##################################################################################################

 ENTIDAD.MC<-M.CONTROL%>%dplyr::select(ENTIDAD)%>%unique()
 Model.V1<-data.frame(ETC=NA,Var=NA,CME=NA,  R2_PROM=NA)
 
 set.seed(1234)
 for(jj in 1:13){ 
 for(i in ENTIDAD.MC$ENTIDAD){
   M_CONTROL <- M.CONTROL   %>% filter(ENTIDAD==i)
   X.COV     <- COVARIABLES %>% filter(ENTIDAD==i)
   Model.V1<-rbind(Model.V1,
           cbind(ETC=i,Var=xk[jj],
                 tryCatch(SEL.MODEL.ETC(M_CONTROL,X.COV,xk[jj],V_PLAUS = "PROM.MAT"), 
                          error = function(e) data.frame(CME=NA,  R2_PROM=NA))
                 
     ))
 }
 }
 
 Model.V1<-dcast(Model.V1,Var~ETC,fun.aggregate = mean, value.var = c("CME"))%>%
           merge(dcast(Model.V1,Var~ETC,fun.aggregate = mean, value.var = c("R2_PROM")),by="Var")
 

 colnames(Model.V1)<-gsub(".y","_R2.PROM",colnames(Model.V1)) 
 colnames(Model.V1)<-gsub(".x","_CME",colnames(Model.V1)) 
 
 Model.V1.Resumen<- 
  data.frame(Model.V1$Var, PRO_CME=apply(Model.V1[,grepl("_CME",colnames(Model.V1))],1,mean,na.rm=T),
            PRO_R2=apply(Model.V1[,grepl("_R2.PROM",colnames(Model.V1))],1,mean,na.rm=T),
            NumFal=apply(Model.V1[,grepl("_CME",colnames(Model.V1))],1,function(x)sum(is.na(x))))
 
write.table(Model.V1,file = "output/Colegios/Covariable/Modelo/Matematicas/Modelo de un Parametro.txt",sep="\t",quote = FALSE,row.names = FALSE)
write.table(Model.V1.Resumen,file = "output/Colegios/Covariable/Modelo/Matematicas/Model.V1.Resumen.txt",sep="\t",quote = FALSE,row.names = FALSE)

 Model.V2<-data.frame(ETC=NA,Model=NA,CME=NA,  R2_PROM=NA)
 xk2 <- combn(xk,2)
 
 set.seed(1234)
 
 for(jj in 1:78){ 
   for(i in ENTIDAD.MC$ENTIDAD){
     M_CONTROL <- M.CONTROL   %>% filter(ENTIDAD==i)
     X.COV     <- COVARIABLES %>% filter(ENTIDAD==i)
     Model.V2<-rbind(Model.V2,
                     cbind(ETC=i,Model=paste0(xk2[,jj],collapse = "+"),
                           tryCatch(SEL.MODEL.ETC(M_CONTROL,X.COV,xk2[,jj],V_PLAUS = "PROM.MAT"), 
                                    error = function(e) data.frame(CME=NA,  R2_PROM=NA))
                           
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

Model.V3<-data.frame(ETC=NA,Model=NA,CME=NA,  R2_PROM=NA)
xk2 <- combn(xk,3)
set.seed(1234)

for(jj in 1:286){ 
  for(i in ENTIDAD.MC$ENTIDAD){
    M_CONTROL <- M.CONTROL   %>% filter(ENTIDAD==i)
    X.COV     <- COVARIABLES %>% filter(ENTIDAD==i)
    Model.V3<-rbind(Model.V3,
                    cbind(ETC=i,Model=paste0(xk2[,jj],collapse = "+"),
                          tryCatch(SEL.MODEL.ETC(M_CONTROL,X.COV,xk2[,jj],V_PLAUS = "PROM.CIENCIAS"), 
                                   error = function(e) data.frame(CME=NA, R2_PROM=NA))
                          
                    ))
  }
}

Model.V3<-dcast(Model.V3,Model~ETC,fun.aggregate = mean, value.var = c("SESGO.PROM"))%>%
  merge(dcast(Model.V3,Model~ETC,fun.aggregate = mean, value.var = c("R2_PROM")),by="Model")

colnames(Model.V3)<-gsub(".y","_R2.PROM",colnames(Model.V3)) 
colnames(Model.V3)<-gsub(".x","_SESGO.PROM",colnames(Model.V3)) 

write.table(Model.V3,file = "output/Colegios/Covariable/Modelo/Modelo de tres Parametro.txt",sep="\t",quote = FALSE,row.names = FALSE)


cat("Revisar los archivos: ","\n")
cat(
"1) output/Colegios/Covariable/Modelo/Modelo de un Parametro.txt","\n",
"2) output/Colegios/Covariable/Modelo/Modelo de dos Parametro.txt","\n",
"3) output/Colegios/Covariable/Modelo/Modelo de tres Parametro.txt","\n",
"y seleccionar el modelo con menor CME.PROM y NUM.NA, pero conmayor R2.PROM")

