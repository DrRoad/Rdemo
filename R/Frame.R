library(shiny)
library(e1071)
library(dataseries)
library(forecast)
library(ggplot2)
library(shinythemes)
library(gdata)
source('R/medidas.R')
source('R/plot.model.R')
source('R/medidas.reg.R')
source('R/acc.model.R')
source('R/residuals.reg.R')

callinterface <- function(){
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Time Series Estimation"),

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
                        #numericInput('year','Initial Year',2005),
                        #numericInput('init','Initial Period',1),
                        numericInput('fore.period','Forecast Period',12)
                      ),
                      mainPanel(
                        tags$head(
                          tags$style(type='text/css',
                                     ".nav-tabs {font-size: 10px} ")),
                        tabsetPanel(type='tabs',
                                    tabPanel('Linear',plotOutput('linear'),
                                             verbatimTextOutput('linear.reg')),
                                    tabPanel('Cuadratic',plotOutput('cuadratic'),
                                             verbatimTextOutput('cuad.reg')),
                                    tabPanel('Cubic',plotOutput('cubic'),
                                             verbatimTextOutput('cubic.reg')),
                                    tabPanel('Linear&season',plotOutput('linearseason'),
                                             verbatimTextOutput('linses.reg')),
                                    tabPanel('Cuadratic&season',plotOutput('cuadseason'),
                                             verbatimTextOutput('cuadses.reg')),
                                    tabPanel('Cubic&season',plotOutput('cubseason'),
                                             verbatimTextOutput('cubses.reg'))


                        ))

             ),
             tabPanel("Residuals Analysis",
                      mainPanel(
                        tabsetPanel(type='tabs',
                                    tabPanel('Linear',plotOutput('Reslinear'),
                                             verbatimTextOutput('acclinear')),
                                    tabPanel('Cuadratic',plotOutput('Rescuadratic'),
                                             verbatimTextOutput('acccuad')),
                                    tabPanel('Cubic',plotOutput('Rescubic'),
                                             verbatimTextOutput('acccub')),
                                    tabPanel('Linear&season',plotOutput('Reslinearseason'),
                                             verbatimTextOutput('acclinsea')),
                                    tabPanel('Cuadratic&season',plotOutput('Rescuadseason'),
                                             verbatimTextOutput('acccuasea')),
                                    tabPanel('Cubic&season',plotOutput('Rescubseason'),
                                             verbatimTextOutput('acccubsea'))
                      )
             )



  )
))
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

  })
  #Densidad
  output$density <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]

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
      xlab("Time") + ylab("Data")

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


  #Modelo de Regresion - Lineal
  output$linear <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))
    number.model <- 1

    plot.model(
      datos,
      fechas,
      frequency,
      m,
      ano.inicio,
      periodo.inicio,
      number.model
    )

  }
  )

  #Modelo de Regresion - Cuadratic
  output$cuadratic <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))
    number.model <- 2

    plot.model(
      datos,
      fechas,
      frequency,
      m,
      ano.inicio,
      periodo.inicio,
      number.model
    )

  }
  )

  #Modelo de Regresion - Cubica
  output$cubic <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))
    number.model <- 3

    plot.model(
      datos,
      fechas,
      frequency,
      m,
      ano.inicio,
      periodo.inicio,
      number.model
    )

  }
  )

  #Modelo de Regresion - Linealseason
  output$linearseason <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))
    number.model <- 4

    plot.model(
      datos,
      fechas,
      frequency,
      m,
      ano.inicio,
      periodo.inicio,
      number.model
    )

  }
  )

  #Modelo de Regresion - cuadseason
  output$cuadseason <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))
    number.model <- 5

    plot.model(
      datos,
      fechas,
      frequency,
      m,
      ano.inicio,
      periodo.inicio,
      number.model
    )

  }
  )

  #Modelo de Regresion - cubseason
  output$cubseason <- renderPlot({

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))
    number.model <- 6

    plot.model(
      datos,
      fechas,
      frequency,
      m,
      ano.inicio,
      periodo.inicio,
      number.model
    )

  }
  )

 output$linear.reg <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 1

   medidas.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$cuad.reg <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 2

   medidas.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$cubic.reg <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 3

   medidas.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$linses.reg <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 4

   medidas.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$cuadses.reg <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 5

   medidas.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$cubses.reg <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 6

   medidas.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$acclinear <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 1

   acc.model(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$acccuad <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 2

   acc.model(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$acccub <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 3

   acc.model(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$acclinsea <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 4

   acc.model(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$acccuasea <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 5

   acc.model(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$acccubsea <- renderPrint({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 6

   acc.model(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$Reslinear <- renderPlot({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 1

   residuals.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$Rescuadratic <- renderPlot({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 2

   residuals.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$Rescubic <- renderPlot({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 3

   residuals.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$Reslinearseason <- renderPlot({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 4

   residuals.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$Rescuadseason <- renderPlot({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 5

   residuals.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })

 output$Rescubseason <- renderPlot({
   req(input$file)
   df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
   datos <- df[,ncol(df)]
   fechas <- as.Date(df[,ncol(df)-1])
   frequency <- input$frequency
   m <- input$fore.period
   ano.inicio <- as.numeric(getYear(fechas[1]))
   periodo.inicio <- as.numeric(getMonth(fechas[1]))
   number.model <- 6

   residuals.reg(
     datos,
     fechas,
     frequency,
     m,
     ano.inicio,
     periodo.inicio,
     number.model
   )
 })
}


runApp(shinyApp(
  ui = ui,
  server = server
))


}
