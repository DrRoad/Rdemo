library(shiny)
library(e1071)
library(dataseries)
library(forecast)
source('R/medidas.R')

ui <- fluidPage(
  titlePanel("Ajuste para Series de Tiempo"),
  navbarPage("",
             tabPanel("Datos",
                      sidebarPanel(
                        fileInput('file','Carga del archivo'),
                        helpText("Default max. file size is 5MB"),
                        tags$hr(),
                        h5(helpText("Select the read.table parameters below")),
                        checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                        checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                        br(),
                        radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')

                      ),
                      mainPanel(tableOutput("contents"))
             ),
             tabPanel("Exploracion",sidebarLayout(

               # Sidebar panel for inputs ----
               sidebarPanel(

                 sliderInput(inputId = "rangets",
                             label = "Time Range",
                             min =1,
                             max =50,
                             value = c(20,30)),

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
             tabPanel("Modelo de Regresión"
             ),
             tabPanel("Análisis de Residuales"
             )



)
)
server <- function(input, output) {
  #datos <- rnorm(1000)

  output$contents <- renderTable({

    req(input$file)
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
      return(head(df))

  })


  D <- dataseries::ds("TOU.OVR.D")
  x <- D$TOU.OVR.D
  fechas <- D$time
  datos <- x

  #Histograma
  output$distPlot <- renderPlot({

    bins <- seq(min(datos), max(datos), length.out = input$bins + 1)

    hist(datos, breaks = bins, col = "#75AADB", border = "white",
         xlab = "x",
         main = "f(x)")
  })
  #Densidad
  output$density <- renderPlot({

    bins <- seq(min(datos), max(datos), length.out = input$bins + 1)

    hist(datos, breaks = bins, col = "#75AADB", border = "white",
         xlab = "x",
         main = "f(x)",
         probability = TRUE)
    lines(density(datos),col='red')})

  #Time Series Plot
  output$tseries <- renderPlot({
    #n1 <- min(rangets)
    #n2 <- max(rangets)

    #x <- x[n1:n2]
    #fechas <- fechas[n1:n2]


    plot(fechas,x,type='l',col='blue')}

  )

  #ECDF
  output$ecdf <- renderPlot({

    plot(ecdf(datos),col='magenta')

  })

  #Summary
  output$summary <- renderPrint({
    medidas(datos)
  })

}

runApp(shinyApp(
  ui = ui,
  server = server
))
