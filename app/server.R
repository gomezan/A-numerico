
library(pracma)
#rm(list=ls())

repartidor=function(text,malla)
{
  texto_split = strsplit(text, split=",")
  texto_columnas = matrix(unlist(texto_split))
  texto_columnas2=0
  tam=dim(texto_columnas)
  i=1
  while(i<=tam[1])
  { 
    texto_split = strsplit(texto_columnas[i], split="=")
    texto_columnas2 = matrix(unlist(texto_split))
    malla=agregarComponente(strtoi(texto_columnas2[2]),malla,texto_columnas2[1])
    i=i+1
  }
  return(malla)
}

agregarComponente=function(a,malla,type)
{
  pos=length(malla)
  malla[pos+1]=a
  names(malla)[pos+1] <- type
  return(malla)
  
}

integracion=function(vec){
  tam=length(vec)
  i=1
  sum=0
  final=0
  while(i<tam)
  {
    particion=abs(vec[i]-vec[i+1])
    base=(vec[i]+vec[i+1])/2
    res=base*particion
    sum=sum+res
    final[i]=sum
    i=i+1
  }
  final[i]=sum
  return(final)
}

inductor=function(a,b,h,I,E,R,L){
  j=1
  i=a
  res=0
  if(L!=0)
  {
    res[c(j)]=I+h*((E/L)-(R/L)*I)
    while(i<(b-h))
    {
      i=i+h
      res[c(j+1)]=res[c(j)]+h*((E/L)-(R/L)*res[c(j)])
      j=j+1
    }
  }else{
    res=matrix(0,ncol = ((b-a)/h)+1)
  }
  return(res)
}


capacitor=function(a,b,h,I,E,R,C){
  i=a
  j=1
  res=0
  if(C!=0)
  {
    res[c(j)]=I+h*((E/R*C)-(1/R*C)*I)
    while(i<(b-h))
    {
      i=i+h
      res[c(j+1)]=res[c(j)]+h*((E/R*C)-(1/R*C)*res[c(j)])
      j=j+1
    }
  }else{
    res=matrix(0,ncol = ((b-a)/h)+1)
  }
  return(res)
}


jacobi <- function(A,b) {
  tam=length(b)
  v0<-matrix(0,nrow=tam,ncol=tam)
  D<-Diag(Diag(A))
  L<--1*tril(A,-1)
  U<--1*triu(A,1)
  I<-inv(D)
  i<-0
  x1=I*b
  x2=I%*%(L+U)
  while(i<15)
  {
    res<-x1+(x2%*%v0)
    j=1
    while(j<=tam)
    {
      v0[j,j]<-sum(res[j,])
      j=j+1
    }
    i<-i+1
  }
  return(Diag(v0))
}

mallaEquivalente=function(malla)
{
  aux=c(0,0,0,0,0)
  tam=length(malla)
  i=1
  while(i<=tam)
  {
    if((names(malla[i])=="R"))
    {
      aux[3]=aux[3]+malla[i]
    }else if(names(malla[i])=="L")
    {
      aux[5]=aux[5]+malla[i]
    }else if(names(malla[i])=="V") 
    {
      aux[1]=aux[1]+malla[i]
    }else if(names(malla[i])=="I") 
    {
      aux[2]=aux[2]+malla[i]
    }else  
    {
      aux[4]=1/(aux[4]+malla[i])
    }
    i=i+1
  }
  return(aux)
  
}

matrizCkto=function(ckto)
{
  aux=matrix(nrow=2,ncol=3)
  aux[1,3]=ckto[1,1]-ckto[2,1]
  aux[1,1]=ckto[1,3]+ckto[2,3]
  aux[1,2]=-ckto[2,3]
  aux[2,3]=ckto[2,1]-ckto[3,1]
  aux[2,2]=ckto[2,3]+ckto[3,3]
  aux[2,1]=-ckto[2,3]
  
  
  return(aux)
  
}


darResistencia=function(ckto,ubic)
{
  res=0
  if((ckto[1,3]!=0)||(ckto[2,3]!=0)||(ckto[3,3]!=0))
  {
    if(ubic==1)
    {
      res=(ckto[2,3]*ckto[3,3])/(ckto[2,3]+ckto[3,3])
      res=res+ckto[1,3]
      
    }else if (ubic==2)
    {
      res=(ckto[1,3]*ckto[3,3])/(ckto[1,3]+ckto[3,3])
      res=res+ckto[2,3]
      
    } else if (ubic==3)
    {
      res=(ckto[1,3]*ckto[2,3])/(ckto[1,3]+ckto[2,3])
      res=res+ckto[3,3]
    }
  }
  return(res)
  
}


darVoltaje=function(ckto,ubic)
{
  res=0
  fte=0
  r=1
  aux=0
  extra=0
  if((ckto[1,3]!=0)||(ckto[2,3]!=0)||(ckto[3,3]!=0))
  {
    if(ubic==1)
    {
      fte=ckto[2,1]-ckto[3,1]
      r=ckto[2,3]+ckto[3,3]
      aux=ckto[3,3]
      extra=ckto[1,1]
      
    }else if (ubic==2)
    {
      fte=ckto[1,1]-ckto[3,1]
      r=ckto[1,3]+ckto[3,3]
      aux=ckto[3,3]
      extra=ckto[2,1]
      
    } else if (ubic==3)
    {
      fte=ckto[1,1]-ckto[2,1]
      r=ckto[1,3]+ckto[2,3]
      aux=ckto[1,1]
      extra=ckto[3,1]
    }
    
    res=(fte*aux/r)+extra
  }
  
  return(res)
  
}

#malla1 <- c(V = 40, R = 4)
#malla12 <- c(R = 2, V = 10)
#malla2 <- c(R = 6, L = 5)
malla1 <-NULL
malla12 <-NULL
malla2 <-NULL

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  #corrientes=jacobi(rbind(M[,1],M[,2]),M[,3])
  
  valorNominal <- eventReactive(input$nuevo, {
    
    if(input$componente== 'capacitor')
    {
      input$capacitancia
    }
    else if (input$componente=='inductor')  
    {
      input$inductancia
    }else
    {
      0
    }
  }) 
  
  ubicMalla <- eventReactive(input$nuevo, {
    
    if(input$posPrincipal== 'malla1')
    {
      1
    }
    else if(input$posPrincipal== 'malla2')
    {
      3
    }
    else if(input$posPrincipal== 'ambas')
    {
      2
    }
  })
  
  mallas1 <- eventReactive(input$nuevo, {
    malla1=repartidor(input$m1,malla1) 
  })
  mallas2 <- eventReactive(input$nuevo, {
    malla2=repartidor(input$m2,malla2) 
  })
  mBoth <- eventReactive(input$nuevo, {
    malla12=repartidor(input$both,malla12) 
  })
  
  vFormulaBase <- eventReactive(input$nuevo, {
    ckt=matrix(nrow=3,ncol=5)
    ckt[1,]<- mallaEquivalente(mallas1())
    ckt[2,]<- mallaEquivalente(mBoth())
    ckt[3,] <- mallaEquivalente(mallas2())
    
    M=matrizCkto(ckt)
    r=darResistencia(ckt,ubicMalla())
    v=darVoltaje(ckt,ubicMalla())
    
    if(input$componente== 'capacitor')
    {
      formulaBase=capacitor(0,input$tiempo,0.0001,input$capacitor,v,r,valorNominal())
    }
    else if (input$componente=='inductor')  
    {
      formulaBase=inductor(0,input$tiempo,0.0001,input$inductor,v,r,valorNominal())
    }else
    {
      formulaBase=matrix(0,ncol = ((input$tiempo)/0.0001)+1)
    }
  })  
  
  
  vFormulaExtra <- eventReactive(input$nuevo, {
    formulaBase=vFormulaBase()
    formulaExtra=(1/valorNominal())*integracion(formulaBase)
  })  
  
  
  vEnergia <- eventReactive(input$nuevo, {
    formulaBase=vFormulaBase()
    energia=0.5*valorNominal()*(formulaBase)^2
  })  
  
  vPotencia <- eventReactive(input$nuevo, {
    formulaBase=vFormulaBase()
    formulaExtra=vFormulaExtra()
    potencia=formulaExtra*formulaBase
  })  
  
  
  vZ <- eventReactive(input$nuevo, {
    z=seq(0,input$tiempo,0.0001)
  }) 
  #r=darResistencia(ckt,target)
  #v=darVoltaje(ckt,target)
  #if(target[2]==5)
  #{ 
  # formulaBase=inductor(a,b,0.0001,I,v,r,n)
  #}else if(target[2]==4)
  #{
  # formulaBase=capacitor(a,b,0.0001,I,v,r,n)
  #} else
  #{
  #  formulaBase=matrix(0,ncol = ((b-a)/0.0001)+1)
  #}
  
  #energia=0.5*n*(formulaBase)^2
  #energia=seq(a,b,0.0001)
  #formulaExtra=integracion(formulaBase)
  #formulaExtra=seq(a,b,0.0001)
  #potencia=formulaExtra*formulaBase
  #potencia=seq(a,b,0.0001)
  #z=seq(a,b,0.0001)
  
  output$graficoBase <- renderPlot({
    
    formulaBase=vFormulaBase()
    z=vZ()
    plot(z,formulaBase,type="l",col="blue" )
    abline(h=0,col="blue")
    
    
  })
  
  output$graficoExtra <- renderPlot({
    
    formulaExtra=vFormulaExtra()
    z=vZ()
    plot(z,formulaExtra,type="l",col="blue" )
    abline(h=0,col="blue")
    
  })
  
  
  output$potencia <- renderPlot({
    potencia=vPotencia()
    z=vZ()
    plot(z,potencia,type="l",col="blue" )
    abline(h=0,col="blue")
    
  })
  
  
  output$energia <- renderPlot({
    energia=vEnergia()
    z=vZ()
    plot(z,energia,type="l",col="blue" )
    abline(h=0,col="blue")
    
  })
  
  output$titulo1 <- renderText({
    if(input$componente=='capacitor'){
      paste("     VOLTAJE DEL CAPACITOR ")
    }else if(input$componente=='inductor')
    {
      paste("     CORRIENTE DEL INDUCTOR ")
    }
    
  })
  
  output$titulo2 <- renderText({
    if(input$componente=='capacitor'){
      paste("     CORRIENTE DEL CAPACITOR ")
    }else if(input$componente=='inductor')
    {
      paste("     VOLTAJE DEL INDUCTOR ")
    }
  })
  
  output$titulo4 <- renderText({
    if(input$componente=='capacitor'){
      paste("     ENERGIA DEL CAPACITOR ")
    }else if(input$componente=='inductor')
    {
      paste("     ENERGIA DEL INDUCTOR ")
    }
  })
  
  output$titulo3 <- renderText({
    if(input$componente=='capacitor'){
      paste("     POTENCIA DEL CAPACITOR ")
    }else if(input$componente=='inductor')
    {
      paste("     POTENCIA DEL INDUCTOR ")
    }
  })
  
  output$instrucciones <- renderText({
    paste("ingrese los componentes del circuito en las mallas correspondientes, marque las fuentes de energia como V y las resistencias con R, evite los espacios y siga el modelo planteado en los ejemplos por defecto")
  })
  
  output$Malla1 <- renderTable({
    malla12=mBoth()
    malla1=mallas1()
    tabla = data.frame (cbind(c(malla1,malla12),c(names(malla1),names(malla12))))
  })
  
  output$Malla2 <- renderTable({
    malla12=mBoth()
    malla2=mallas2()
    tabla = data.frame (cbind(c(malla12,malla2),c(names(malla12),names(malla2))))
    
  })
  
  output$espacio <- renderText({
    paste(" ")
    
  })
  
  output$tituloMalla1 <- renderText({
    paste("malla 1")
    
  })
  
  output$tituloMalla2 <- renderText({
    paste("malla 2")
    
  })
  
  
  
})

