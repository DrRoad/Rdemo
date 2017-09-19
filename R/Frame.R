library(markdown)
library(shiny)

ui <- fluidPage(
  titlePanel("Ajuste para Series de Tiempo"),
  navbarPage("",
             tabPanel("Datos"
             ),
             tabPanel("Exploracion",
                      tabsetPanel(
                        tabPanel("Histogram", plotOutput("distPlot")),
                        tabPanel("Density"),
                        tabPanel("ECDG"),
                        tabPanel("QQ-Plot"),
                        tabPanel("Summary")
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
    
    #bins <- seq(min(datos), max(datos), length.out = input$bins + 1)
    
    hist(datos, breaks = 50, col = "#75AADB", border = "white",
         xlab = "x",
         main = "f(x)")
  })
  
}

runApp(shinyApp(
  ui = ui,
  server = server
))