
rm(list=ls())
mutar<- function (p,i,j){
  p[c(i,j)] <- p[rev(c(i,j))]
  p
}


aptitud <- function (s) {
  colisiones <- 0
  for (i in 1:(nRei-1))
    for (j in (i+1):nRei)
      colisiones <- colisiones + ((j - i) == abs (s[i] - s[j]))
    1 / (1 + colisiones)
}
mostrar <- function (s) {
  tablero <- matrix (".", nRei, nRei)
  tablero[cbind(1:nRei,s)] <- "R"
  apply (tablero, 1, function (fila) cat (fila, "\n"))
}

voraz<-function(hijo1){
  muthijos<-NULL
  for(i in 1:nRei){
    for(j in 1:nRei){
      muthijos<-rbind(muthijos,mutar(hijo1,i,j))
    }
  } 
  punt<-apply(muthijos,1,aptitud)
  while(any(punt>aptitud(hijo1))) {
    hijo1<-muthijos[order(punt,decreasing = T)[1],]
    muthijos<-NULL
    for(i in 1:nRei){
      for(j in 1:nRei){
        muthijos<-rbind(muthijos,mutar(hijo1,i,j))
      }
    } 
    punt<-apply(muthijos,1,aptitud)
  }
  hijo1
}


nRei    <- 40
nPob    <- 100
tasaCan <- 0.05
nIte    <- 100000
tasaMut <- 1

pob <- lapply (1:nPob, function (i) sample (nRei))


for (i in 1:nIte) {
  iCandidatos  <- sample (nPob, round(tasaCan*nPob))
  aptitudesCan <- sapply (iCandidatos, function (i) aptitud (pob [[i]]))
  iPadres      <- order (aptitudesCan, decreasing = TRUE) [1:2]
  padres       <- pob [iCandidatos] [iPadres]
  pos          <- sample (nRei, 1)
  hijo1        <- c (padres[[1]][1:pos],
                     setdiff (padres[[2]], padres[[1]][1:pos])) 
  hijo2        <- c (padres[[2]][1:pos],
                     setdiff (padres[[1]], padres[[2]][1:pos]))
  hijo1<-voraz(hijo1)
  hijo2<-voraz(hijo2)
  pob       <- c (pob, list (hijo1, hijo2))
  aptitudes <- sapply (pob, aptitud)
  iMejores  <- order (aptitudes, decreasing=TRUE) [1:nPob]
  pob       <- pob[iMejores]
  aptitud.mejor     <- aptitud (pob[[1]])
  if (aptitud.mejor == 1) break
}
cat ("La mejor solucion es", pob[[1]], "cuya aptitud es", aptitud.mejor, ".\n")
