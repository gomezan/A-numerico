
# Metodo de secante
# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) log(x+2)-sin(x)
F1x <- function(x) (1/(x+2))-cos(x)
# Halla la raiz de Fx
secante <- function(x0,x1) {
  x<-seq(-2,0,0.001)
  plot(x,Fx(x),type="l",col="blue")
  abline(h=0,col="blue")
  x<-(Fx(x1)*x0-Fx(x0)*x1)
  y<-(Fx(x1)-Fx(x0))
  x<-x/y
  error <-1
  while (error > 1.e-7) {
    x0<-x1
    x1<-x
    x<-(Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
   if (Fx(x) == 0) break
    error<-abs(Fx(x)/F1x(x))
    points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    cat("X=",x,"\t","E=",error,"\n")
  }
}
secante(-2,0)
