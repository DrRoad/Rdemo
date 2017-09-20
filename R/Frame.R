library(shiny)
library(e1071)
source('R/medidas.R')

ui <- fluidPage(
  titlePanel("Ajuste para Series de Tiempo"),
  navbarPage("",
             tabPanel("Datos"
             ),
             tabPanel("Exploracion",sidebarLayout(

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
  datos <- rnorm(1000)
  output$distPlot <- renderPlot({

    bins <- seq(min(datos), max(datos), length.out = input$bins + 1)

    hist(datos, breaks = bins, col = "#75AADB", border = "white",
         xlab = "x",
         main = "f(x)")
  })
  output$density <- renderPlot({

    bins <- seq(min(datos), max(datos), length.out = input$bins + 1)

    hist(datos, breaks = bins, col = "#75AADB", border = "white",
         xlab = "x",
         main = "f(x)",
         probability = TRUE)
    lines(density(datos),col='red')})

  output$ecdf <- renderPlot({

    plot(ecdf(datos),col='magenta')

  })

  output$summary <- renderPrint({
    medidas(datos)
  })

}

runApp(shinyApp(
  ui = ui,
  server = server
))
