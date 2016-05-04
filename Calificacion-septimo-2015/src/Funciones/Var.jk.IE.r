#' Estimación de la varinza jackknife
#'@description
#' Estiama el promedio de una variable mediante los estimadores Horvitz-Thompson (HT)
#'
#'@param x  Tipo \code{data.frame}
#'@param PESOS.ESTU El peso de la observación en la muestra
#'@param V_PLAUS  Tipo caracter donde se indican las columnas con las cuales se
#'       se realizará la estimación del promedio
#'@return
#'Regresa un \code{data.frame} con los valores
#'\describe{
#'\item{Model}{El modelo ajustado}
#'\item{R2_i}{El R2 para cada uno de estos modelos y las diferentes variables respuestas}
#'\item{CME.GREG}{El cuadrado medio del error obtenido en la estimación}}
#'@examples
#'data("CAL_MUNI")
#'Datos<-REstudiante$ETC
#'Var.jk.IE(Datos,PESOS.ESTU = "PESO",V_PLAUS = c("RESULTADO"))
#'Var.jk.IE(subset(Datos,ZONA=="Urbana"),PESOS.ESTU = "PESO",V_PLAUS = c("RESULTADO"))
#'Var.jk.IE(subset(Datos,SECTOR!="Oficial"),PESOS.ESTU = "PESO",V_PLAUS = c("RESULTADO"))
#'Var.jk.IE(subset(Datos,SECTOR=="Oficial"),PESOS.ESTU = "PESO",V_PLAUS = c("RESULTADO"))

#'@seealso \code{\link{E.GREG.IE}}
#' @export
Var.jk.IE <- function(x,PESOS.ESTU,V_PLAUS){
  n <- nrow(x)
  if(n<6){return(data.frame(PROM.IE =NA,PROM.sd=NA,JK.sd=NA,SD=NA,cve=NA))}

  theta2<-paste0("theta2<-data.frame(",paste0("theta",1:length(V_PLAUS),sep="=NA",collapse = ","),")")
  eval(parse(text=theta2))

  if(length(V_PLAUS)==1){
    bar.HT   <- sum(x[[PESOS.ESTU]]*x[,V_PLAUS])/sum(x[[PESOS.ESTU]])
    sd.bar.HT    <-  0
    for (i in 1:n){
      theta2[i,]<-sum(x[-i,PESOS.ESTU]*x[-i,V_PLAUS],na.rm = T)/sum(x[-i,PESOS.ESTU],na.rm = T)
    }

  }else{
    bar.HT   <- colSums((x[[PESOS.ESTU]]*x[,V_PLAUS])/sum(x[[PESOS.ESTU]]))
    sd.bar.HT   <-  sd(bar.HT)
    bar.HT <- mean(bar.HT)
    for (i in 1:n){
      theta2[i,]<-mean(colSums(x[-i,PESOS.ESTU]*x[-i,V_PLAUS],na.rm = T)/sum(x[-i,PESOS.ESTU],na.rm = T))
    }
  }

  a <- (n-1)/n
  diff2<-a*(theta2-bar.HT)^2
  sd.HT=sqrt(mean(colSums(diff2)))

  data.frame(cbind(PROM.IE   =  mean(bar.HT),
                   PROM.sd   =  sd.bar.HT,
                   JK.sd     =  sd.HT,
                   SD        =  sqrt(sd.HT^2+1.2*(sd.bar.HT^2)),
                   cve       =  sqrt(sd.HT^2+1.2*(sd.bar.HT^2))/mean(bar.HT)))
}

