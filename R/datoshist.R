library(shiny)
library(e1071)
source('medidas.R')

datos <- c() #inicializacion del vector de datos

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Distribucion de Datos"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

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
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

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
    lines(density(datos),col='red')

  })

  output$ecdf <- renderPlot({

    plot(ecdf(datos),col='magenta')

  })

  output$summary <- renderPrint({
    medidas(datos)
  })


}

datoshist <- function(x){
  datos <<- x
  runApp(shinyApp(
    ui = ui,
    server = server
  ))
}
