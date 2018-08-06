library(Rcpp)


cppFunction('double colisiones(NumericVector x){
int n = x.size();
double colisiones = 0;
for(int i=1; i  <= (n-1); ++i){
for(int j=(i+1); j <= n; ++j ){
int  suma = abs(x[i]-x[j]) == (j-i);
colisiones = colisiones + suma;
}
}
return colisiones;
}')

otra <- function (p)
{
  ij    <- sample (n, 2)
  p[ij] <- p[rev(ij)]
  p
}


n<-1000
maxiter<-1000000
temp <- n
iter <- 0
sol0 <- sample (n)
val0 <- colisiones (sol0)


while (iter<maxiter && val0>0)
{
  sol1 <- otra (sol0)
  val1 <- colisiones (sol1)
  if (val1 <= val0) alfa <- 1 else alfa <- exp ((val0-val1)/temp)
  if (rbinom(1,1,alfa)) {sol0 <- sol1; val0 <- val1}
  iter <- iter + 1
  temp <- temp * 0.995
}
cat ("Mejor solucio'n:", sol0, ". Mejor resultado:", val0, "\n")


