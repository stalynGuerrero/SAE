############################################################################################
###### Función que se emplea para el calcualo de la varianza de los estimadores de    ######
###### Horvitz–Thompson (HT) y Estimador de Regresion Generalizado (GREG) utiliazando ######
###### utilizando el método de Jackknife con y sin Estrato.                           ######
############################################################################################

## Elaborado: Stalyn Guerrero 
## Proyecto SAE 14-01 - 2016
## Subdirección de Estadística ICFES

############################################################################################
Media.HT <- function(data,estrato,v.plaus,pesos){
##### Argumentos  
# 
  
  num <- colSums(plaus$pesos*plaus[,c("pv1","pv2","pv3","pv4","pv5")])
  den <- sum(plaus$pesos)
  X.bar <- num/den
  n <- nrow(plaus)
  ###############
  
# Jackknife estratificado
USM<-aggregate(plaus$pesos,list(Estrato=plaus$estrato,
                                 Establecimiento=plaus$establecimiento),length)
UPM <-aggregate(USM$Establecimiento,list(Estrato=USM$Estrato),length)

    if(nrow(USM)==1||min(UPM$x)==1){
    nEst <- sum(UPM$x)
    a <- (nEst-1)/nEst
    plaus$pesos <-subset(plaus,select = pesos) /a 
    U <- data_frame(thetaj1=NA,thetaj2=NA,thetaj3=NA,thetaj4=NA,thetaj5=NA)
    for(i in 1:nrow(USM)){
      ii = plaus$establecimiento!=USM$Establecimiento[i]
      U[i,] <- colSums(plaus$pesos[ii,]*plaus[ii,c("pv1","pv2","pv3","pv4","pv5")])/sum(plaus$pesos[ii,])  
    }
    matmean <- matrix(rep(X.bar,nrow(USM)),byrow = T,ncol=5) 
    diff2<-a*(U-matmean)^2
    Estrato=FALSE
    sd=sqrt(mean(colSums(diff2)))
  }else{

UPM$a <-(UPM$x-1)/UPM$x 

UM <-merge(USM[,-3],UPM,by.x = "Estrato", by.y = "Estrato")

U <- data_frame(thetaj1=NA,thetaj2=NA,thetaj3=NA,thetaj4=NA,thetaj5=NA)
for(i in 1:nrow(UM)){
  xx <- plaus
  wh <-subset(plaus,estrato==UM[i,1],select = pesos) /UM[i,4] 
  xx$pesos[xx$estrato==UM[i,1]] <-wh$pesos 
  xx <-subset(xx,establecimiento!=UM[i,2])
  U[i,] <- colSums(xx$pesos*xx[,c("pv1","pv2","pv3","pv4","pv5")])/sum(xx$pesos)
}
matmean <- matrix(rep(X.bar,nrow(UM)),byrow = T,ncol=5) 
diff2<-UM$a*(U-matmean)^2
Estrato =TRUE
sd = sqrt(mean(colSums(aggregate(diff2,list(Estrato=UM$Estrato),sum)[,-1])))
}

round(c(EnteTerr=plaus$enteTerr[1],
  nEstabs=sum(UPM$x),
  n =n,
  Media=mean(X.bar),
  Sd.bar =sd(X.bar),
Sd.Jackknife = sd,
Sd =sqrt(sd^2+1.2*var(X.bar)),
Estrato.Jackknife=Estrato),2)
}

