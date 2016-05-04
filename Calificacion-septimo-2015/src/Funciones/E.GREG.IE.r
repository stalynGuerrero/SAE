#' Estimación de varianza jackknife de grupos y subgrupos
#'@description
#' La función realiza la estimación de las varianzas para el estimador
#' Horvitz-Thompson (TH)  y el estimador de regresión generalizada (GREG)  mediante jackknife
#'@param x  Tipo \code{\link{data.frame}}
#'@param PESOS.ESTU Tipo carácter que indica la columna de los pesos de las observacion
#'@param V_PLAUS Vector de caracteres donde se indica los nombres con los cuales se identifican los valores plausibles
#'@param xk   Vector de caracteres que identifica a las covariables del estimador GREG
#'@param txk  \code{\link{data.frame}} que contiene los totales de las covariables \code{xk} por grupo.
#'@param IND  Corresponde a la varaible que indica el subgrupo para los cuales se desea obtener
#' una estimación de la varianza del JK, por defeto \code{IND=NULL}.
#'@param ...  Paramétros que requeridos por la función \code{\link{sampling::calib}}, esta dispone de
#' los métodos calibarción linear, raking, logit y truncated

#'@return Retorna un \code{\link{data.frame}} con los resultados obtenidos  Horvitz-Thompson (TH)  y
#' el estimador de regresión generalizada (GREG)
#'así como sus respectivas varianzas para los grupos y subgrupos indicados.
#'@examples
#'data("CAL_MUNI")
#'Datos<-REstudiante$ETC
#'TX<-as.data.frame(t(REstudiante$Tx))
#'
#'Var.jk.IE(Datos,PESOS.ESTU = "PESO",V_PLAUS = c("RESULTADO"))
#'E.GREG.IE(Datos,PESOS.ESTU = "PESO",V_PLAUS = "RESULTADO",
#'          xk = "INSE",tx = TX,method="linear")

#'E.GREG.IE(Datos,PESOS.ESTU = "PESO",V_PLAUS = "RESULTADO",
#'          xk = "INSE",tx = TX,method="linear",IND="SECTOR")

#'@seealso  \code{\link{Var.jk.IE,calib}}
#' @export
#'

E.GREG.IE <- function(x,PESOS.ESTU,V_PLAUS,xk,tx,IND=NULL,...){

  ## En caso que las covariables tengan datos faltantes se realiza la imputación de estos:
  if(anyNA(x[[PESOS.ESTU]])){
      stop(paste0("Algunos pesos son 'NA'"))
  }

  if(anyNA(x[,xk])){
    warning("Las covariables (xk) tienen valores NA´s, estos serán imputados")
    imp <- mice(x[,xk],meth="norm.predict")
    ## Reemplazando valores imputados
    x[,xk] <- complete(imp)[,xk]
  }

  ## Recalculando los pesos de los estudiantes por ETC, usando los txk.
  gkl <- calib(cbind(x[,xk]),d =x[[PESOS.ESTU]],
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


  salida   <-  data.frame(PROM.greg.ETC   = sum(bar.greg),   Sd.greg.PROM = sd(bar.greg),cve.greg.ETC=sd(bar.greg)/sum(bar.greg),
                          PROM.HT.ETC     = sum(bar.HT),     Sd.HT.PROM   = sd(bar.HT),  cve.HT.ETC=sd(bar.HT)/sum(bar.HT))
  # Calculando los errores del modelo de regresión
  for(i in 1:length(V_PLAUS)){
    texto <- paste0("x$eepv",i,"=residuals(lm(",V_PLAUS[i],"~.-1,data=x[,c(V_PLAUS[",i,"],xk)],weights = x[['PESOS.ESTU']]))")
    eval(parse(text=texto))}
  ee.V_PLAUS<-colnames(x)[grepl("eepv",colnames(x))]

  # Seleccionando las IE pertenecen a la ETC
  if(is.null(IND)){
    sd.jk<- cbind(HT   = Var.jk.IE(x,V_PLAUS = V_PLAUS,PESOS.ESTU=PESOS.ESTU)[,-5],
                  GREG = Var.jk.IE(x,V_PLAUS = ee.V_PLAUS,PESOS.ESTU="pesos.greg")[,-5])

    sd.jk$GREG.PROM.IE<- Var.jk.IE(x,V_PLAUS = V_PLAUS,PESOS.ESTU="pesos.greg")[,1]
    salida <-data.frame(salida,sd.jk)
  }else{
    id_IE <- factor(as.character(x[[IND]]))
    # Estimar la Varianza Jackknife para las IE dentro de la ETC
    sd.HT<- do.call("rbind",
                    as.list(by(id_IE,data = x,
                               function(x)Var.jk.IE(x,V_PLAUS = V_PLAUS,
                                                    PESOS.ESTU=PESOS.ESTU)[,-5])))
    PROM.IE<- do.call("c",
                    as.list(by(id_IE,data = x,
                               function(x)Var.jk.IE(x,V_PLAUS = V_PLAUS,
                                                    PESOS.ESTU="pesos.greg")[,1])))
    sd.GREG<- do.call("rbind",
                    as.list(by(id_IE,data = x,
                               function(x)Var.jk.IE(x,V_PLAUS = ee.V_PLAUS,
                                                    PESOS.ESTU="pesos.greg")[,-5])))
    sd.GREG$PROM.IE<-PROM.IE

    sd.jk<-cbind(HT=sd.HT,GREG=sd.GREG)
    salida <-data.frame(ID_INST = factor(rownames(sd.HT)),salida,sd.jk)
  }

  return(salida)
}

