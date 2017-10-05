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
             tabPanel("Regression & Residuals Analisys",
                      sidebarPanel(

                        h5(helpText("Select the estimation parameters below")),

                        #Menu para Seleccion del Modelo de Regresion

                        selectInput(inputId = 'RegModel',
                                    label = 'Regression Model',
                                    choices = c('Linear','Cuadratic','Cubic',
                                                'Linear & Seassons',
                                                'Cuadratic & Seassons',
                                                'Cubic & Seassons')),

                        numericInput('frequency',"Frequency",12),
                        numericInput('fore.period','Forecast Period',12)

                      ),
                      mainPanel(tabsetPanel(type='tabs',
                                            tabPanel('Regression',plotOutput('plot.reg'),
                                            verbatimTextOutput('tab.reg')),
                                            tabPanel('Residual Analysis',plotOutput('plot.res'),
                                            verbatimTextOutput('tab.res')))



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

  #Funcion para graficas de modelo de regresion y pronosticos

  output$plot.reg <- renderPlot({

    if (input$RegModel=='Linear'){
      number.model=1}
    else if (input$RegModel=='Cuadratic'){
      number.model=2}
    else if (input$RegModel=='Cubic'){
      number.model=3}
    else if (input$RegModel=='Linear & Seassons'){
      number.model=4}
    else if (input$RegModel=='Cuadratic & Seassons'){
      number.model=5}
    else {
      number.model=6}

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))

    plot.model(
      datos,
      fechas,
      frequency,
      m,
      ano.inicio,
      periodo.inicio,
      number.model
    )

    #Tabla de Salida de Estadisticos para Modelo de Regresion

  })

  output$tab.reg <- renderPrint({

    if (input$RegModel=='Linear'){
      number.model=1}
    else if (input$RegModel=='Cuadratic'){
      number.model=2}
    else if (input$RegModel=='Cubic'){
      number.model=3}
    else if (input$RegModel=='Linear & Seassons'){
      number.model=4}
    else if (input$RegModel=='Cuadratic & Seassons'){
      number.model=5}
    else {
      number.model=6}

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))

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

  #Funcion para graficas de modelo Analisis de Residuales

  output$plot.res <- renderPlot({

    if (input$RegModel=='Linear'){
      number.model=1}
    else if (input$RegModel=='Cuadratic'){
      number.model=2}
    else if (input$RegModel=='Cubic'){
      number.model=3}
    else if (input$RegModel=='Linear & Seassons'){
      number.model=4}
    else if (input$RegModel=='Cuadratic & Seassons'){
      number.model=5}
    else {
      number.model=6}

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))

    residuals.reg(
      datos,
      fechas,
      frequency,
      m,
      ano.inicio,
      periodo.inicio,
      number.model
    )

    #Tabla de Salida de Estadisticos Residuales

  })

  output$tab.res <- renderPrint({

    if (input$RegModel=='Linear'){
      number.model=1}
    else if (input$RegModel=='Cuadratic'){
      number.model=2}
    else if (input$RegModel=='Cubic'){
      number.model=3}
    else if (input$RegModel=='Linear & Seassons'){
      number.model=4}
    else if (input$RegModel=='Cuadratic & Seassons'){
      number.model=5}
    else {
      number.model=6}

    req(input$file)
    df <- read.csv(input$file$datapath,header = input$header, sep=input$sep)
    datos <- df[,ncol(df)]
    fechas <- as.Date(df[,ncol(df)-1])
    frequency <- input$frequency
    m <- input$fore.period
    ano.inicio <- as.numeric(getYear(fechas[1]))
    periodo.inicio <- as.numeric(getMonth(fechas[1]))

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

}


runApp(shinyApp(
  ui = ui,
  server = server
))


}
