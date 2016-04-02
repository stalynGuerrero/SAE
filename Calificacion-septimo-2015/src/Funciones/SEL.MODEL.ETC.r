###########################################################################################
##### La función se emplea para hacer la selección del mejor modelo dado un conjunto de ##
##### posibles variables, para lo cual se basa en el CME.                               ##
###########################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 16-03 - 2016
## Subdirección de Estadística ICFES

###################################################################################################


SEL.MODEL.ETC <-function(M_CONTROL,Txk,Xk,PROM.ESTU,PESO.ESTU,ID_INST="ID_INST",...){
  #   M_CONTROL  :	Tipo data.frame en donde se encuentra la muestral control en la i-esima ETC
  #   Txk        :	Total de la variable auxiliar en la i-esima ETC
  #   Xk         :	Vector de carácter que contiene los nombres de las covariables, estos debes
  #                 coincidir con el orden introducido en Txk
  #   PROM.ESTU  :	Tipo carácter que contiene el nombre de la columna de la(s) calificación(es)
  #                 del estudiante 
  #   PESO.ESTU  :	Tipo carácter que contiene el nombre de la columna del factor de expansión
  #                 (1/π_k)  del k-esimo 
  #   ID_INST    :	Tipo carácter que contiene el nombre de la columna que identifica al j-esmio
  #                 Establecimiento Educativo en la i-esima ETC
  #   …          :	Argumentos adicional empleado para indicar el método de calibración 
  #   
  
  gkl <- calib(cbind(M_CONTROL[,Xk]),d = M_CONTROL[[PESO.ESTU]],
               total=as.numeric(Txk),...)
  
  # Definir los pesos GREG
  M_CONTROL$pesos.greg<-M_CONTROL[[PESO.ESTU]]*gkl 
  
  Resul<-paste0("Resul<-data.frame(",paste0("R2_",1:length(PROM.ESTU),sep="=NA",collapse = ","),")")
  eval(parse(text=Resul))
  
  for(i in 1:length(PROM.ESTU)){
    texto <- paste0("Resul$R2_",i,"=summary(lm(",PROM.ESTU[i],"~.-1,data=M_CONTROL[,c(PROM.ESTU[",i,"],Xk)],weights = M_CONTROL$",PESO.ESTU,"))$r.squared")
    eval(parse(text=texto))
    texto <- paste0("M_CONTROL$PROM.GREG",i,"<-M_CONTROL[['pesos.greg']]*M_CONTROL$",PROM.ESTU[i])
    eval(parse(text=texto))
  }
  
  texto<- paste("PROM.IE<-M_CONTROL%>%group_by(",ID_INST,")%>%summarise_each(funs(mean(.,na.rm = T)),matches('",PROM.ESTU[1],"'))",sep="")
  eval(parse(text=texto))
  PROM.GREG<-M_CONTROL%>%group_by(ID_INST)%>%summarise_each(funs(sum(.,na.rm = T)/sum(pesos.greg,na.rm = T)),matches("PROM.GREG"))
  for(i in 1:length(PROM.ESTU)){
    xk<-abs(PROM.IE[,1+i]-PROM.GREG[,1+i])^2
    n<-length(xk)
    texto <- paste("Resul$CME.GREG",i,"<-sum(xk,na.rm = T)/n",sep="")
    eval(parse(text=texto))
  }        
  Resul
}