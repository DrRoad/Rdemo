library(shiny)
library(e1071)
library(dataseries)
library(forecast)
library(ggplot2)
#library(shinythemes)
source('R/medidas.R')

#<<<<<<< HEAD
#ui <- fluidPage(theme = shinytheme("flatly"),
  #titlePanel("Ajuste para Series de Tiempo"),
#=======
ui <- fluidPage(
  titlePanel("Time Series Estimation"),
#>>>>>>> 77a651031ff21ba36382240a3f8ad2ba235e4760
  navbarPage("",
             tabPanel("Data",
                      sidebarPanel(
                        fileInput('file','File Load'),
                        tags$hr(),
                        h5(helpText("Select the read.table parameters below")),
                        checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                        br(),
                        radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')

                      ),
                      mainPanel(tableOutput("contents"))
             ),
             tabPanel("Exploration",sidebarLayout(

               # Sidebar panel for inputs ----
               sidebarPanel(

                 # Input: Slider for the number of bins ----
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),


               # Main panel for displaying outputs ----
               mainPanel(

                 # Output: Tabset w/ plot, summary, and table ----
                 tabsetPanel(type = "tabs",
                             tabPanel("Time Series", plotOutput("tseries")),
                             tabPanel("Histogram", plotOutput("distPlot")),
                             tabPanel("Density", plotOutput("density")),
                             tabPanel("ECDF", plotOutput("ecdf")),
                             tabPanel("Summary", verbatimTextOutput("summary"))
                 )
               )
             )
             ),
             tabPanel("Regression Model",
                      sidebarPanel(
                        h5(helpText("Select the estimation parameters below")),
                        numericInput('frequency',"Frequency",12),
                        numericInput('year','Initial Year',2005),
                        numericInput('init','Initial Period',1),
                        numericInput('fore.period','Forecast Period',12)
                      ),
                      mainPanel(
                        tags$head(
                          tags$style(type='text/css',
                                     ".nav-tabs {font-size: 10px} ")),
                        tabsetPanel(type='tabs',
                        tabPanel('Linear',plotOutput('linear')),
                        tabPanel('Cuadratic'),
                        tabPanel('Cubic'),
                        tabPanel('Linear&season'),
                        tabPanel('Cuadratic&season'),
                        tabPanel('Cubic&season')


                      ))

             ),
             tabPanel("Residuals Analysis",
                      radioButtons('modelo', 'Seleccione el modelo de ajuste:',
                                   c(Lineal='1', Cuadratico='2', Cubico='3')),
                      plotOutput("resplot")
             )



  )
)
server <- function(input, output) {
  df <- c()

  output$contents <- renderTable({

    req(input$file)
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(head(df))

  })

  #Histograma
  output$distPlot <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]

    #bins <- seq(min(datos), max(datos), length.out = input$bins + 1)

    ggplot(data=df,aes(df[,ncol(df)]))+
      geom_histogram(breaks=seq(min(datos),max(datos),
                                by=((max(datos)-min(datos))/(input$bins + 1))),
                     col='blue4',
                     fill='deepskyblue2',
                     alpha=0.4)+
      labs(x='Datos',y='Count')

    #hist(datos, breaks = bins, col = "#75AADB", border = "white",
    #     xlab = "x",
    #     main = "f(x)")
  })
  #Densidad
  output$density <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]

    #bins <- seq(min(datos), max(datos), length.out = input$bins + 1)

    #hist(datos, breaks = bins, col = "#75AADB", border = "white",
    #     xlab = "x",
    #     main = "f(x)",
    #     probability = TRUE)
    #lines(density(datos),col='red')})

    ggplot(data=df,aes(df[,ncol(df)]))+
      geom_histogram(aes(y=..density..),
                     breaks=seq(min(datos),max(datos),
                                by=((max(datos)-min(datos))/(input$bins + 1))),
                     col='blue4',
                     fill='deepskyblue2',
                     alpha=0.4)+
      geom_density(col=1)+
      labs(x='Datos',y='Count')
  })

  #Time Series Plot
  output$tseries <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])

    obser <- data.frame(fechas,datos)

    ggplot(obser, aes(fechas, datos)) + geom_line(aes(fechas,datos),color='blue4') +
      xlab("Seq Time") + ylab("Data")


    #plot(seq(1:length(datos)),panel.first = grid(),datos,type='l',col='blue',xlab='Seq Time',ylab='Data')}
    #plot(vfechas,x, xaxt="n", panel.first = grid(),type='l',ylab='produccion.mes.')}

  }
    )

  #ECDF
  output$ecdf <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]

    #plot(ecdf(datos),col='magenta')

    ggplot(df, aes(df[,ncol(df)]))+
      stat_ecdf(geom='point',color='blue4')+
      labs(x='Datos')

  })

  #Summary
  output$summary <- renderPrint({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]

    data.frame(medidas(datos))
  })

  output$linear <- renderPlot({

    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]

    y <- ts(datos,freq=input$frequency, start = c(input$year,input$init))

    m <- input$fore.period
    n <- length(y)
    frequency <- input$frequency

    yi <- ts(y[1:(n-m)],frequency)
    ti <- seq(1:length(yi))

    linear.model <- lm(yi ~ ti)
    linear.fit <- linear.model$fitted.values

    #plot(ti,yi,type='l')
    #lines(linear.fit,col='red')

    ggplot(data.frame(ti,yi,linear.fit),aes(ti,yi))+
      geom_line(aes(ti,yi),col='blue')+
      geom_line(aes(ti,linear.fit),col='red')

  }
  )

}

runApp(shinyApp(
  ui = ui,
  server = server
))
