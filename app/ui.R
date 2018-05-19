
library(shiny)
library(pracma)

# Define UI 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Simulador de circuitos"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      sliderInput("tiempo",
                  "linea de tiempo (s):",
                  min = 6,
                  max = 20,
                  value = 10),
      selectInput("componente", "seleccionar el componente deseado", 
                  choices =  c("  ","capacitor","inductor")),
      conditionalPanel(condition="input.componente== 'capacitor'",
                       numericInput("capacitor","ingrese el voltaje inicial del capacitor (V)",0),
                       numericInput("capacitancia","ingrese la capacitancia del condensador (F)",0)),
      
      conditionalPanel(condition="input.componente=='inductor'", 
                       numericInput("inductor","ingrese el voltaje inicial del inductor (V)",0),
                       numericInput("inductancia","ingrese la inductancia de la bobina (H)",0)),
      textOutput("instrucciones"),
      textInput("m1","ingrese en la casilla elementos que componen la malla 1  ","R=5,V=20"),
      textInput("m2","ingrese en la casilla elementos que componen la malla 2  ","R=5,V=20"),
      textInput("both","ingrese en la casilla elementos que comparten ambas mallas  ","R=5,V=20"),
      
      conditionalPanel(condition="input.componente!='  '", 
                       selectInput("posPrincipal","posicion del componente en el circuito", 
                                   choices =  c("  ","malla1","malla2","ambas"))),
      
      conditionalPanel(condition="input.posPrincipal!='  '",
                       actionButton("nuevo","simular"))
      
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      textOutput("tituloMalla1"),
      tableOutput("Malla1"),
      textOutput("tituloMalla2"),
      tableOutput("Malla2"),
      textOutput("espacio"),
      textOutput("titulo1"),
      plotOutput("graficoBase"),
      textOutput("titulo2"),
      plotOutput("graficoExtra"),
      textOutput("titulo3"),
      plotOutput("potencia"),
      textOutput("titulo4"),
      plotOutput("energia") 
      
      
    )
  )
))
