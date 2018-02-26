rm(list=ls())
# convierte los numeros binarios a decimales
decimal <- function(a,b) {
tam<-length(a)
#Parte entera
j<-0
sum<-0
res<-0
while(0<tam)
{
  res<-((a[c(tam)]))
  if(res!=0)
  {
    sum=sum+(res*2^j) 
  }
  tam<-tam-1
  j<-j+1
}
#cat(sum,".")
#parte decimal
tam<-length(b)
j<-1
while((j<=tam))
{
  res<-(b[c(j)])
  if(res!=0)
  {
    sum=sum+(res*2^((j)*(-1))) 
  }
  j<-j+1
}
cat(sum)
}
a<-c(1,1,1)
b<-c(1,1,1,1,1,1)
decimal(a,b)


