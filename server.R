server = function(input, output) {
  
  output$table <- renderTable(zzzz(), digits = 3)
  
  rescisiones.grrr <- reactive({
    a <- rescisiones.gr2 %>% filter(TRABAJADORES >= input$desde & TRABAJADORES <= input$hasta ) %>% 
      filter (aumento %in% input$aumento)
    return(a)})
  zz <- reactive( { 
    
    contratos.s.d<-  contratos.s.d %>% filter(TRABAJADORES >= input$desde & TRABAJADORES <= input$hasta ) %>% 
      filter ((as.numeric(substring(INIVIG,1,4)) == input$cohorte))  %>% 
      filter (aumento %in% input$aumento) %>% 
      group_by (CONTRATO) %>% summarise(q = n())
    z <- sum(contratos.s.d$q)
    return(z)
  })
  zzz <- reactive( { 
    z <- rescisiones.grrr() %>% filter ((as.numeric(substring(INIVIG,1,4)) == input$cohorte)) %>% 
      group_by(fecha,DETALLE_MOTIVO) %>% summarise( q = n())
    return(z)           } )
  
  zzzz <-  reactive({
    
    a <- dcast(zzz(),fecha ~ DETALLE_MOTIVO, fill = 0)  %>% 
      select(fecha, R,V) %>% 
      mutate ( 
        dx =    R + V,
        lx = zz() ,
        qx = round(dx/lx,3) ,
        qxV = round(V/lx,3)) %>% head(-2)
    
    for (i in 2:nrow(a)) {
      a[i,"lx" ] <- a[i-1,"lx"] - a[i-1,"dx"]
    }
    return(a)
  })
  
  
  output$plot_off <- renderPlot({
    O <-ggplot(data = zzzz(), aes(x=fecha,y=qxV,group=1)) +
      
      
      geom_line() +
      
      ggtitle("Probalidad de Rescision por traspaso") +
      labs(x="Tiempo",y="QxV")+
      theme(axis.text.x = element_text(angle = 70))
    theme_classic() 
    O })
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  vidamedia <- reactive({ 
    a <- zzzz()
    for (i in 1:nrow(a)) {
      a[i,"px"] <-  1 -a[i,"qx"] 
    }
    for (i in 2) {
      a[i,"px2"] <-  a[i-1,"px"]*a[i,"px"]
    }
    for (i in 3:nrow(a)) {
      a[i,"px2"] <-  a[i-1,"px2"]*a[i,"px"]
    }
    a[1,"px2"] <-  a[1,"px"]
    b <- round(sum(a$px2)/2,2)
    b
    
    
  })
  
  output$vidamedia <- renderText({paste("Vida Media condicionada ( E[X/X<Y]", vidamedia(), "Años")})
  
  
  
  output$text <- renderText({   "Observaciones.
  <br>1) Las probabilidades de rescision decrecen a medidad que los contratos tienen mas antiguedad. Pareciera que existe un proceso de depuracion.
      El primer año del contrato, por motivos legales no se pueden traspasar, por lo tanto la probabilidad es baja. 
     <br> 2 )La probabilidad de rescision por traspaso es mas grande en cuentas de mas trabajadores.
      <br>3 )La probabilidad de rescision por traspaso  es el decremento de mayor importancia en las cuentas mas grande.
      es decir estas no rescinden por falta de pago
      <br>4 )La probabilidad de rescision por traspaso  es casi igual a la probabilidad de rescision por falta de pago en cuentas
      de 1 a 10.
      <br>5) Es dificil observar la probalidad de que rescindan los contratos sin comunicaciones de aumento. Ya que el hecho de que no
      tengan comunicaciones de aumentos es porque estan rescindidos dentro de los primeros años.
      "
  })
}