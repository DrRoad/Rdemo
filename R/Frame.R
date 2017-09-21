library(shiny)
library(e1071)
library(dataseries)
library(forecast)
source('R/medidas.R')

ui <- fluidPage(
  titlePanel("Ajuste para Series de Tiempo"),
  navbarPage("",
             tabPanel("Datos"
             ),
             tabPanel("Exploracion",sidebarLayout(

               # Sidebar panel for inputs ----
               sidebarPanel(

                 sliderInput(inputId = "rangets",
                             label = "Time Range",
                             min =fechas[1],
                             max =fechas[length(fechas)],
                             value = c(fechas[20],fechas[40])),

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

  D <- dataseries::ds("TOU.OVR.D")
  x <- D$TOU.OVR.D
  fechas = D$time
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
