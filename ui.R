ui = 
  fluidPage( 
    title = "Analisis de Rescisiones de contratos",
    sidebarPanel(
      numericInput("desde", "Trabajadores desde",0,width = 80),
      numericInput("hasta", "hasta",10,width = 80),
      selectInput("cohorte", "cohorte",selected  = 2012, choices = c(2012,2013,2014,2015,2016,2017,2018),width = 80),
      selectInput("aumento", "Comunicacion",multiple = TRUE,selected = c("Sin aumento", "Con aumento"), choices = c("Sin aumento", "Con aumento")),width = 3,
      textOutput("vidamedia")),
    mainPanel(  
      tabsetPanel(
        tabPanel("Tabla","Tabla de decremento multiple. V: Traspaso; R: falta de pago;
                qx: probabilidad de rescision total; qxV:Probabilidad de rescision por traspaso",
          tableOutput('table')
                  ),
        tabPanel("Grafico",
          plotOutput("plot_off",click = "plot_click"),
          verbatimTextOutput("info")
              ),
        tabPanel("Observaciones", htmlOutput("text")
              )
        )
      )
    )
