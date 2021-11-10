#### Proyecto 4 - Brando Alberto Toribio García

##########    Librerías    #####################################################
library(actuar) #Modificaciones en la v.a de pérdida X
library(ggplot2) #Plots
library(shiny) #Dashboards
library(bslib) #Temas de shiny
library(fitdistrplus) #Ajuste de curvas de probabilidad
library(goftest) #Pruebas de bondad de ajuste

##########    Diseño de tablero    #############################################

ui <- navbarPage("Modelos de Frecuencia",
                 theme = bs_theme(bootswatch = "united"),
########## Página 1 ##########             
                 tabPanel("Distribuciones de frecuencia", icon = icon("chart-bar"),
                          tabsetPanel(
                            tabPanel("Densidad", icon = icon("chart-bar"),
                            fluidRow(align="center",
                                     column(4, plotOutput('plot1')),
                                     column(4, plotOutput('plot2')),
                                     column(4, plotOutput('plot3'))
                                     )
                            ),
                            tabPanel("Distribución", icon = icon("chart-area"),
                                     fluidRow(align="center",
                                              column(4, plotOutput('plot4')),
                                              column(4, plotOutput('plot5')),
                                              column(4, plotOutput('plot6'))
                                     )
                            ),
                            tabPanel("Sobrevivencia", icon = icon("chart-line"),
                                     fluidRow(align="center",
                                              column(4, plotOutput('plot7')),
                                              column(4, plotOutput('plot8')),
                                              column(4, plotOutput('plot9'))
                                     )
                            )

                          ),
                          wellPanel(h5("Seleccione los parámetros"),
                                    fluidRow(column(3,
                                                    selectInput('dist',label="Distribución",
                                                                choices = c("Binomial negativa","Poisson",
                                                                            "Geométrica","Binomial"))),
                                             column(3,
                                                    sliderInput(inputId = "p1",label="Parámetro 1",
                                                                value=10,min=0,max=15,step=0.1)),
                                             column(3,
                                                    sliderInput(inputId = "p2",label="Parámetro 2",
                                                                value=0.3,min=0,max=15,step=0.1)),
                                             column(3, 
                                                    sliderInput(inputId = "p0",label="Probabilidad en 0",
                                                                   value=0.20,min=0,max=1,step=0.01))
                                    )
                          ),
                          h5("Resumen de los datos"),
                          verbatimTextOutput("t1"),
                          h5("Resumen de los datos cero modificada"),
                          verbatimTextOutput("t2"),
                          h5("Resumen de los datos cero truncada"),
                          verbatimTextOutput("t3"),
                 ),
########## Página 2 ########## 
                 tabPanel("Ajuste de Distribuciones", icon = icon("project-diagram"),
                          fluidPage(
                            sidebarLayout(position = "right",
                                          
                                          sidebarPanel(h5("Seleccione el tipo de variable"),
                                                       h6(""),
                                                       radioButtons("rb", label = NULL,
                                                                    choiceNames = list(
                                                                      "Variable original",
                                                                      "Variable modificada",
                                                                      "Variable truncada"
                                                                    ),
                                                                    choiceValues = list(
                                                                      "vo", "vm", "vt"
                                                                    )),
                                                       h5("Seleccione la(s) distribucion(es)"),
                                                       checkboxInput("cbbin", "Binomial", TRUE),
                                                       sliderInput(inputId = "pp1",label="Argumento fijo: n",
                                                                                   value=60,min=0,max=100,step=1),
                                                       checkboxInput("cbpoi", "Poisson", TRUE),
                                                       checkboxInput("cbgeo", "Geométrica", TRUE),
                                                       checkboxInput("cbnbin", "Binomial negativa", TRUE),
                                                       sliderInput(inputId = "pp5",label="Argumento fijo: r",
                                                                                   value=10,min=0,max=100,step=1)
                                          ),
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Densidad", plotOutput("g1"),
h6("Los datos a ajustar son aquellos de la primera pestaña (Distribuciones de frecuencia) para cada variable aleatoria."),
h5("Resultado de pruebas de ajuste"),
verbatimTextOutput("t4")),
                                              tabPanel("Distribución", plotOutput("g2"),
h6("Los datos a ajustar son aquellos de la primera pestaña (Distribuciones de frecuencia) para cada variable aleatoria."),
h5("Resultado de pruebas de ajuste"),
verbatimTextOutput("t5")),
                                              tabPanel("Gráfico Q-Q", plotOutput("g3"),
h6("Los datos a ajustar son aquellos de la primera pestaña (Distribuciones de frecuencia) para cada variable aleatoria."),
h5("Resultado de pruebas de ajuste"),
verbatimTextOutput("t6")),
                                              tabPanel("Gráfico P-P", plotOutput("g4"),
h6("Los datos a ajustar son aquellos de la primera pestaña (Distribuciones de frecuencia) para cada variable aleatoria."),
h5("Resultado de pruebas de ajuste"),
verbatimTextOutput("t7"))                                                      
                                            )
                                          )
                            )
                          )
                 ),
########## Página 3 ##########
tabPanel("Sobre el autor", icon = icon("user-alt"),
         fluidPage(
           h5("Brando Alberto Toribio García"),
           h5("brando.tg24@hotmail.com")
         )

)
)

##########     SERVIDOR     ####################################################
server<-function(input,output){
  ########## Página 1 Valores fijos ##########
  
  no <- c(1:100) #Número de simulaciones
  tema <- theme(plot.title = element_text(size = (14), colour="orangered",hjust = 0.5,face = "bold"), 
                legend.title = element_text(colour = "orangered"), 
                legend.text = element_text(colour="orangered"), 
                axis.title = element_text(size = (12), colour = "orangered",face = "bold"),
                axis.text = element_text(colour = "black", size = (12)))
  
  ########## Página 1 Valores variables ##########
  fr<-reactive({
    if(input$dist=="Binomial"){ #2
      rbinom(no,input$p1,input$p2)
    }
    else if(input$dist=="Poisson"){ #1
      rpois(no,input$p1)
    }
    else if(input$dist=="Geométrica"){ #1
      rgeom(no,input$p1)
    }
    else if(input$dist=="Binomial negativa"){ #2
      rnbinom(no,input$p1,input$p2)
    }
  })
  
  intervalo <- reactive(seq(0,as.integer(max(fr())),by=1))
  
  fd<-reactive({
    if(input$dist=="Binomial"){ #2
      dbinom(intervalo(),input$p1,input$p2)
    }
    else if(input$dist=="Poisson"){ #1
      dpois(intervalo(),input$p1)
    }
    else if(input$dist=="Geométrica"){ #1
      dgeom(intervalo(),input$p1)
    }
    else if(input$dist=="Binomial negativa"){ #2
      dnbinom(intervalo(),input$p1,input$p2)
    }
  })
  
  fd2<-reactive({
    if(input$dist=="Binomial"){ #2
      dbinom(fr(),input$p1,input$p2)
    }
    else if(input$dist=="Poisson"){ #1
      dpois(fr(),input$p1)
    }
    else if(input$dist=="Geométrica"){ #1
      dgeom(fr(),input$p1)
    }
    else if(input$dist=="Binomial negativa"){ #2
      dnbinom(fr(),input$p1,input$p2)
    }
  })
  
  gp<-reactive({
    if(input$dist=="Binomial"){ #2
      ggplot(data1(), aes(x=f,pbinom(fr(),input$p1,input$p2)))
    }
    else if(input$dist=="Poisson"){ #1
      ggplot(data1(), aes(x=f,ppois(fr(),input$p1)))
    }
    else if(input$dist=="Geométrica"){ #1
      ggplot(data1(), aes(x=f,pgeom(fr(),input$p1)))
    }
    else if(input$dist=="Binomial negativa"){ #2
      ggplot(data1(), aes(x=f,pnbinom(fr(),input$p1,input$p2)))
    }
  })
  
  p<-reactive({
    if(input$dist=="Binomial"){ #2
      data.frame(x = seq(0,as.integer(max(fr())),by=1),
                 y = pbinom(seq(0,as.integer(max(fr())),by=1),
                           input$p1,input$p2))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(0,as.integer(max(fr())),by=1),
                 y = ppois(seq(0,as.integer(max(fr())),by=1),
                            input$p1))
    }
    else if(input$dist=="Geométrica"){ #1
      data.frame(x = seq(0,as.integer(max(fr())),by=1),
                 y = pgeom(seq(0,as.integer(max(fr())),by=1),
                            input$p1))
    }
    else if(input$dist=="Binomial negativa"){ #2
      data.frame(x = seq(0,as.integer(max(fr())),by=1),
                 y = pnbinom(seq(0,as.integer(max(fr())),by=1),
                            input$p1,input$p2))
    }
  })
  
  gs<-reactive({
    if(input$dist=="Binomial"){ #2
      ggplot(data1(), aes(x=f,1-pbinom(fr(),input$p1,input$p2)))
    }
    else if(input$dist=="Poisson"){ #1
      ggplot(data1(), aes(x=f,1-ppois(fr(),input$p1)))
    }
    else if(input$dist=="Geométrica"){ #1
      ggplot(data1(), aes(x=f,1-pgeom(fr(),input$p1)))
    }
    else if(input$dist=="Binomial negativa"){ #2
      ggplot(data1(), aes(x=f,1-pnbinom(fr(),input$p1,input$p2)))
    }
  })
  
  s<-reactive({
    if(input$dist=="Binomial"){ #2
      data.frame(x = seq(0,as.integer(max(fr())),by=1),
                 y = 1-pbinom(seq(0,as.integer(max(fr())),by=1),
                            input$p1,input$p2))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(0,as.integer(max(fr())),by=1),
                 y = 1-ppois(seq(0,as.integer(max(fr())),by=1),
                           input$p1))
    }
    else if(input$dist=="Geométrica"){ #1
      data.frame(x = seq(0,as.integer(max(fr())),by=1),
                 y = 1-pgeom(seq(0,as.integer(max(fr())),by=1),
                           input$p1))
    }
    else if(input$dist=="Binomial negativa"){ #2
      data.frame(x = seq(0,as.integer(max(fr())),by=1),
                 y = 1-pnbinom(seq(0,as.integer(max(fr())),by=1),
                             input$p1,input$p2))
    }
  })
  
  data1 <- reactive(data.frame(nn = no, f = fr())) #Empíricos
  data2 <- reactive(data.frame(x = intervalo(), y = fd())) #Teóricos 
  ########## Página 1 Valores variables ZM ##########

  frzm<-reactive({
    if(input$dist=="Binomial"){ #2
      rzmbinom(no,input$p1,input$p2,input$p0)
    }
    else if(input$dist=="Poisson"){ #1
      rzmpois(no,input$p1,input$p0)
    }
    else if(input$dist=="Geométrica"){ #1
      rzmgeom(no,input$p1,input$p0)
    }
    else if(input$dist=="Binomial negativa"){ #2
      rzmnbinom(no,input$p1,input$p2,input$p0)
    }
  })
  
  intervalozm <- reactive(seq(0,as.integer(max(frzm())),by=1))
  
  fdzm<-reactive({
    if(input$dist=="Binomial"){ #2
      dzmbinom(intervalozm(),input$p1,input$p2,input$p0)
    }
    else if(input$dist=="Poisson"){ #1
      dzmpois(intervalozm(),input$p1,input$p0)
    }
    else if(input$dist=="Geométrica"){ #1
      dzmgeom(intervalozm(),input$p1,input$p0)
    }
    else if(input$dist=="Binomial negativa"){ #2
      dzmnbinom(intervalozm(),input$p1,input$p2,input$p0)
    }
  })
  
  fd2zm<-reactive({
    if(input$dist=="Binomial"){ #2
      dzmbinom(frzm(),input$p1,input$p2,input$p0)
    }
    else if(input$dist=="Poisson"){ #1
      dzmpois(frzm(),input$p1,input$p0)
    }
    else if(input$dist=="Geométrica"){ #1
      dzmgeom(frzm(),input$p1,input$p0)
    }
    else if(input$dist=="Binomial negativa"){ #2
      dzmnbinom(frzm(),input$p1,input$p2,input$p0)
    }
  })
  
  gpzm<-reactive({
    if(input$dist=="Binomial"){ #2
      ggplot(data1zm(), aes(x=f,pzmbinom(frzm(),input$p1,input$p2,input$p0)))
    }
    else if(input$dist=="Poisson"){ #1
      ggplot(data1zm(), aes(x=f,pzmpois(frzm(),input$p1,input$p0)))
    }
    else if(input$dist=="Geométrica"){ #1
      ggplot(data1zm(), aes(x=f,pzmgeom(frzm(),input$p1,input$p0)))
    }
    else if(input$dist=="Binomial negativa"){ #2
      ggplot(data1zm(), aes(x=f,pzmnbinom(frzm(),input$p1,input$p2,input$p0)))
    }
  })
  
  pzm<-reactive({
    if(input$dist=="Binomial"){ #2
      data.frame(x = seq(0,as.integer(max(frzm())),by=1),
                 y = pzmbinom(seq(0,as.integer(max(frzm())),by=1),
                            input$p1,input$p2,input$p0))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(0,as.integer(max(frzm())),by=1),
                 y = pzmpois(seq(0,as.integer(max(frzm())),by=1),
                           input$p1,input$p0))
    }
    else if(input$dist=="Geométrica"){ #1
      data.frame(x = seq(0,as.integer(max(frzm())),by=1),
                 y = pzmgeom(seq(0,as.integer(max(frzm())),by=1),
                           input$p1,input$p0))
    }
    else if(input$dist=="Binomial negativa"){ #2
      data.frame(x = seq(0,as.integer(max(frzm())),by=1),
                 y = pzmnbinom(seq(0,as.integer(max(frzm())),by=1),
                             input$p1,input$p2,input$p0))
    }
  })
  
  gszm<-reactive({
    if(input$dist=="Binomial"){ #2
      ggplot(data1zm(), aes(x=f,1-pzmbinom(frzm(),input$p1,input$p2,input$p0)))
    }
    else if(input$dist=="Poisson"){ #1
      ggplot(data1zm(), aes(x=f,1-pzmpois(frzm(),input$p1,input$p0)))
    }
    else if(input$dist=="Geométrica"){ #1
      ggplot(data1zm(), aes(x=f,1-pzmgeom(frzm(),input$p1,input$p0)))
    }
    else if(input$dist=="Binomial negativa"){ #2
      ggplot(data1zm(), aes(x=f,1-pzmnbinom(frzm(),input$p1,input$p2,input$p0)))
    }
  })
  
  szm<-reactive({
    if(input$dist=="Binomial"){ #2
      data.frame(x = seq(0,as.integer(max(frzm())),by=1),
                 y = 1-pzmbinom(seq(0,as.integer(max(frzm())),by=1),
                              input$p1,input$p2,input$p0))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(0,as.integer(max(frzm())),by=1),
                 y = 1-pzmpois(seq(0,as.integer(max(frzm())),by=1),
                             input$p1,input$p0))
    }
    else if(input$dist=="Geométrica"){ #1
      data.frame(x = seq(0,as.integer(max(frzm())),by=1),
                 y = 1-pzmgeom(seq(0,as.integer(max(frzm())),by=1),
                             input$p1,input$p0))
    }
    else if(input$dist=="Binomial negativa"){ #2
      data.frame(x = seq(0,as.integer(max(frzm())),by=1),
                 y = 1-pzmnbinom(seq(0,as.integer(max(frzm())),by=1),
                               input$p1,input$p2,input$p0))
    }
  })
  
  data1zm <- reactive(data.frame(nn = no, f = frzm())) #Empíricos
  data2zm <- reactive(data.frame(x = intervalozm(), y = fdzm())) #Teóricos
  
  ########## Página 1 Valores variables ZT ##########
  
  frzt<-reactive({
    if(input$dist=="Binomial"){ #2
      rztbinom(no,input$p1,input$p2)
    }
    else if(input$dist=="Poisson"){ #1
      rztpois(no,input$p1)
    }
    else if(input$dist=="Geométrica"){ #1
      rztgeom(no,input$p1)
    }
    else if(input$dist=="Binomial negativa"){ #2
      rztnbinom(no,input$p1,input$p2)
    }
  })
  
  intervalozt <- reactive(seq(0,as.integer(max(frzt())),by=1))
  
  fdzt<-reactive({
    if(input$dist=="Binomial"){ #2
      dztbinom(intervalozt(),input$p1,input$p2)
    }
    else if(input$dist=="Poisson"){ #1
      dztpois(intervalozt(),input$p1)
    }
    else if(input$dist=="Geométrica"){ #1
      dztgeom(intervalozt(),input$p1)
    }
    else if(input$dist=="Binomial negativa"){ #2
      dztnbinom(intervalozt(),input$p1,input$p2)
    }
  })
  
  fd2zt<-reactive({
    if(input$dist=="Binomial"){ #2
      dztbinom(frzt(),input$p1,input$p2)
    }
    else if(input$dist=="Poisson"){ #1
      dztpois(frzt(),input$p1)
    }
    else if(input$dist=="Geométrica"){ #1
      dztgeom(frzt(),input$p1)
    }
    else if(input$dist=="Binomial negativa"){ #2
      dztnbinom(frzt(),input$p1,input$p2)
    }
  })
  
  gpzt<-reactive({
    if(input$dist=="Binomial"){ #2
      ggplot(data1zt(), aes(x=f,pztbinom(frzt(),input$p1,input$p2)))
    }
    else if(input$dist=="Poisson"){ #1
      ggplot(data1zt(), aes(x=f,pztpois(frzt(),input$p1)))
    }
    else if(input$dist=="Geométrica"){ #1
      ggplot(data1zt(), aes(x=f,pztgeom(frzt(),input$p1)))
    }
    else if(input$dist=="Binomial negativa"){ #2
      ggplot(data1zt(), aes(x=f,pztnbinom(frzt(),input$p1,input$p2)))
    }
  })
  
  pzt<-reactive({
    if(input$dist=="Binomial"){ #2
      data.frame(x = seq(0,as.integer(max(frzt())),by=1),
                 y = pztbinom(seq(0,as.integer(max(frzt())),by=1),
                              input$p1,input$p2))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(0,as.integer(max(frzt())),by=1),
                 y = pztpois(seq(0,as.integer(max(frzt())),by=1),
                             input$p1))
    }
    else if(input$dist=="Geométrica"){ #1
      data.frame(x = seq(0,as.integer(max(frzt())),by=1),
                 y = pztgeom(seq(0,as.integer(max(frzt())),by=1),
                             input$p1))
    }
    else if(input$dist=="Binomial negativa"){ #2
      data.frame(x = seq(0,as.integer(max(frzt())),by=1),
                 y = pztnbinom(seq(0,as.integer(max(frzt())),by=1),
                               input$p1,input$p2))
    }
  })
  
  gszt<-reactive({
    if(input$dist=="Binomial"){ #2
      ggplot(data1zt(), aes(x=f,1-pztbinom(frzt(),input$p1,input$p2)))
    }
    else if(input$dist=="Poisson"){ #1
      ggplot(data1zt(), aes(x=f,1-pztpois(frzt(),input$p1)))
    }
    else if(input$dist=="Geométrica"){ #1
      ggplot(data1zt(), aes(x=f,1-pztgeom(frzt(),input$p1)))
    }
    else if(input$dist=="Binomial negativa"){ #2
      ggplot(data1zt(), aes(x=f,1-pztnbinom(frzt(),input$p1,input$p2)))
    }
  })
  
  szt<-reactive({
    if(input$dist=="Binomial"){ #2
      data.frame(x = seq(0,as.integer(max(frzt())),by=1),
                 y = 1-pztbinom(seq(0,as.integer(max(frzt())),by=1),
                              input$p1,input$p2))
    }
    else if(input$dist=="Poisson"){ #1
      data.frame(x = seq(0,as.integer(max(frzt())),by=1),
                 y = 1-pztpois(seq(0,as.integer(max(frzt())),by=1),
                             input$p1))
    }
    else if(input$dist=="Geométrica"){ #1
      data.frame(x = seq(0,as.integer(max(frzt())),by=1),
                 y = 1-pztgeom(seq(0,as.integer(max(frzt())),by=1),
                             input$p1))
    }
    else if(input$dist=="Binomial negativa"){ #2
      data.frame(x = seq(0,as.integer(max(frzt())),by=1),
                 y = 1-pztnbinom(seq(0,as.integer(max(frzt())),by=1),
                               input$p1,input$p2))
    }
  })
  
  data1zt <- reactive(data.frame(nn = no, f = frzt())) #Empíricos
  data2zt <- reactive(data.frame(x = intervalozt(), y = fdzt())) #Teóricos
  
  ########## Página 1 Plots ########## 
  
  output$plot1<-renderPlot({
    ggplot(data1(), aes(x=f)) + 
      geom_histogram(aes(y=..density..), binwidth=1, colour="white", fill="red3", alpha = 0.75) +
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) + 
      ggtitle(paste("Función",as.character(input$dist))) +
      tema +
      geom_area(data=data2(),aes(x = x, y = y), color = "black",alpha=.2, fill="yellow") +
      geom_point(aes(y = fd2()), color = "black",alpha=.7,size = 2) + # 
      geom_vline(aes(xintercept=summary(fr())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(fr())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(fr())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(fr())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$plot2<-renderPlot({
    ggplot(data1zm(), aes(x=f)) + 
      geom_histogram(aes(y=..density..), binwidth=1, colour="white", fill="blue3", alpha = 0.75) +
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) + 
      ggtitle(paste("Cero modificada",as.character(input$dist))) +
      tema +
      geom_area(data=data2zm(),aes(x = x, y = y), color = "black",alpha=.2, fill="yellow") +
      geom_point(aes(y = fd2zm()), color = "black",alpha=.7,size = 2) + # 
      geom_vline(aes(xintercept=summary(frzm())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(frzm())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzm())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzm())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$plot3<-renderPlot({
    ggplot(data1zt(), aes(x=f)) + 
      geom_histogram(aes(y=..density..), binwidth=1, colour="white", fill="green3", alpha = 0.75) +
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) + 
      ggtitle(paste("Cero truncada",as.character(input$dist))) +
      tema +
      geom_area(data=data2zt(),aes(x = x, y = y), color = "black",alpha=.2, fill="yellow") +
      geom_point(aes(y = fd2zt()), color = "black",alpha=.7,size = 2) + # 
      geom_vline(aes(xintercept=summary(frzt())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(frzt())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzt())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzt())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$plot4<-renderPlot({
      gp() + 
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) +
      ggtitle(paste("Función",as.character(input$dist))) +
      tema +
      geom_line(data=p(),aes(x = x, y = y), color = "red3",size=1) +
      geom_vline(aes(xintercept=summary(fr())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(fr())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(fr())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(fr())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$plot5<-renderPlot({
      gpzm() + 
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) +
      ggtitle(paste("Cero modificada",as.character(input$dist))) +
      tema +
      geom_line(data=pzm(),aes(x = x, y = y), color = "blue3",size=1) +
      geom_vline(aes(xintercept=summary(frzm())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(frzm())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzm())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzm())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$plot6<-renderPlot({
      gpzt() + 
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) +
      ggtitle(paste("Cero truncada",as.character(input$dist))) +
      tema +
      geom_line(data=pzt(),aes(x = x, y = y), color = "green3",size=1) +
      geom_vline(aes(xintercept=summary(frzt())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(frzt())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzt())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzt())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$plot7<-renderPlot({
    gs() + 
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) +
      ggtitle(paste("Función",as.character(input$dist))) +
      tema +
      geom_line(data=s(),aes(x = x, y = y), color = "red3",size=1) +
      geom_vline(aes(xintercept=summary(fr())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(fr())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(fr())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(fr())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$plot8<-renderPlot({
    gszm() + 
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) +
      ggtitle(paste("Cero modificada",as.character(input$dist))) +
      tema +
      geom_line(data=szm(),aes(x = x, y = y), color = "blue3",size=1) +
      geom_vline(aes(xintercept=summary(frzm())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(frzm())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzm())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzm())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  output$plot9<-renderPlot({
    gszt() + 
      labs(y="Densidad", colour = "NA") +
      theme(axis.title.x=element_blank()) +
      ggtitle(paste("Cero truncada",as.character(input$dist))) +
      tema +
      geom_line(data=szt(),aes(x = x, y = y), color = "green3",size=1) +
      geom_vline(aes(xintercept=summary(frzt())[4]), linetype="dashed", size=1, colour="blue") +
      geom_vline(aes(xintercept=summary(frzt())[2]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzt())[3]), linetype="dashed", size=1, colour="maroon4")+
      geom_vline(aes(xintercept=summary(frzt())[5]), linetype="dashed", size=1, colour="maroon4")
  })
  
  ########## Página 1 Texto ##########
  output$t1 <- renderPrint({
    paste("Min:",summary(fr())[1],"  ",
          "Primer Cuartil:",summary(fr())[2],"  ",
          "Mediana:",summary(fr())[3],"  ",
          "Tercer Cuartil:",summary(fr())[5],"  ",
          "Max:",summary(fr())[6],"  ",
          "Media:",summary(fr())[4])
  })
  output$t2 <- renderPrint({
    paste("Min:",summary(frzm())[1],"  ",
          "Primer Cuartil:",summary(frzm())[2],"  ",
          "Mediana:",summary(frzm())[3],"  ",
          "Tercer Cuartil:",summary(frzm())[5],"  ",
          "Max:",summary(frzm())[6],"  ",
          "Media:",summary(frzm())[4])
  })
  output$t3 <- renderPrint({
    paste("Min:",summary(frzt())[1],"  ",
          "Primer Cuartil:",summary(frzt())[2],"  ",
          "Mediana:",summary(frzt())[3],"  ",
          "Tercer Cuartil:",summary(frzt())[5],"  ",
          "Max:",summary(frzt())[6],"  ",
          "Media:",summary(frzt())[4])
  })
  ########## Página 2 Valores fijos ##########
  
  colores <- c("red","gold2","blue","limegreen")
  tema2 <- theme(plot.title = element_text(size = (14), colour="orangered",hjust = 0.5,face = "bold"), 
                 legend.title = element_text(colour = "orangered"), 
                 legend.text = element_text(colour="orangered"), 
                 axis.title = element_text(size = (12), colour = "orangered",face = "bold"),
                 axis.text = element_text(colour = "black", size = (12)),
                 legend.position = "right")
  ########## Página 2 Valores variables ##########
  
  fbinom<-reactive({
    if(input$rb=="vo"){
      fitdist(fr(),"binom",method = "mle", discrete = T, 
              start = list(prob=0.3), fix.arg = list(size=input$pp1))
    }else if(input$rb=="vm"){
      fitdist(frzm(),"zmbinom",method = "mle",discrete = T,
              start=list(prob=0.3), fix.arg = list(size=input$pp1,p0=input$p0))
    }else if(input$rb=="vt"){
      fitdist(frzt(),"ztbinom",method = "mle",discrete = T,
              start=list(prob=0.3), fix.arg = list(size=input$pp1))
    }
  })
  
  fpois<-reactive({
    if(input$rb=="vo"){
      fitdist(fr(),"pois",method = "mle", discrete = T, 
              start = list(lambda=2))
    }else if(input$rb=="vm"){
      fitdist(frzm(),"zmpois",method = "mle",discrete = T,
              start = list(lambda=2),fix.arg=list(p0=input$p0))
    }else if(input$rb=="vt"){
      fitdist(frzt(),"ztpois",method = "mle",discrete = T,
              start = list(lambda=2))
    }
  })
  
  fgeom<-reactive({
    if(input$rb=="vo"){
      fitdist(fr(),"geom",method = "mle", discrete = T, 
              start = list(prob=0.3))
    }else if(input$rb=="vm"){
      fitdist(frzm(),"zmgeom",method = "mle",discrete = T,
              start = list(prob=0.3),fix.arg=list(p0=input$p0))
    }else if(input$rb=="vt"){
      fitdist(frzt(),"ztgeom",method = "mle",discrete = T,
              start = list(prob=0.3))
    }
  })
  
  fnbinom<-reactive({
    if(input$rb=="vo"){
      fitdist(fr(),"nbinom",method = "mle", discrete = T, 
              start = list(prob=0.3), fix.arg = list(size=input$pp5))
    }else if(input$rb=="vm"){
      fitdist(frzm(),"zmnbinom",method = "mle",discrete = T,
              start = list(prob=0.3), fix.arg = list(size=input$pp5,p0=input$p0))
    }else if(input$rb=="vt"){
      fitdist(frzt(),"ztnbinom",method = "mle",discrete = T,
              start = list(prob=0.3), fix.arg = list(size=input$pp5))
    }
  })

  a1 <- reactive({if(input$cbbin==TRUE){fbinom()}else{NULL}})
  a2 <- reactive({if(input$cbpoi==TRUE){fpois()}else{NULL}})
  a3 <- reactive({if(input$cbgeo==TRUE){fgeom()}else{NULL}})
  a4 <- reactive({if(input$cbnbin==TRUE){fnbinom()}else{NULL}})
  
  model <- reactive(list(a1(),a2(),a3(),a4()))
  modelos <- reactive(model()[!sapply(model(),is.null)])
  
  b1<-reactive({if(input$cbbin==TRUE){c("Binomial")}else{NA}})
  b2<-reactive({if(input$cbpoi==TRUE){c("Poisson")}else{NA}})
  b3<-reactive({if(input$cbgeo==TRUE){c("Geométrica")}else{NA}})
  b4<-reactive({if(input$cbnbin==TRUE){c("Binomial negativa")}else{NA}})
  
  ley <- reactive(c(c(b1()),c(b2()),c(b3()),c(b4())))
  leyenda <- reactive(ley()[!is.na(ley())])
  
  ########## Página 2 Plots ########## 
  
  output$g1<-renderPlot({
    denscomp(modelos(), legendtext = leyenda(), fitcol = colores , fitlty = 1, fitlwd = 2,
             ylab = "Densidad", datacol = "lightcyan1",
             plotstyle = "ggplot", addlegend = FALSE, demp = TRUE) + 
      ggplot2::ggtitle("") + tema2 + theme(axis.title.x=element_blank())
      
  })
  
  output$g2<-renderPlot({
    cdfcomp(modelos(), legendtext = leyenda(), fitcol = colores , fitlty = 1,
            ylab = "CDF",
            plotstyle = "ggplot", addlegend = FALSE) + 
      ggplot2::ggtitle("") + tema2 + theme(axis.title.x=element_blank())
  })

  output$g3<-renderPlot({
    qqcomp(modelos(), legendtext = leyenda(), fitcol = colores,
           xlab = "Cuantiles teóricos", ylab = "Cuantiles empíricos",xlegend = "topright", 
           fitlwd = 3, plotstyle = "ggplot", addlegend = FALSE) + 
      ggplot2::ggtitle("") + tema2 + theme(axis.title.x=element_blank())
  })

  output$g4<-renderPlot({
    ppcomp(modelos(), legendtext = leyenda(), fitcol = colores , fitlty = 1,
           xlab = "Probabilidad teórica", ylab = "Probabilidad empírica",xlegend = "topright", 
           fitlwd = 3, plotstyle = "ggplot", addlegend = FALSE) + 
      ggplot2::ggtitle("") + tema2 + theme(axis.title.x=element_blank())
  })
  
  ########## Página 2 Texto ########## 

  output$t4 <- renderPrint({gofstat(f=modelos(), fitnames = leyenda())})
  output$t5 <- renderPrint({gofstat(f=modelos(), fitnames = leyenda())})
  output$t6 <- renderPrint({gofstat(f=modelos(), fitnames = leyenda())})
  output$t7 <- renderPrint({gofstat(f=modelos(), fitnames = leyenda())})
  
}

##########     PUBLICACIÓN     #################################################
shinyApp(ui=ui,server=server)







