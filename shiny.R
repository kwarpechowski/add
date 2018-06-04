library(shiny)
source("test.R")

wig <- read.csv(file="/Users/kamilw/add/wig20_d.csv", header=TRUE, sep=",")
wig$Data <- as.Date(wig$Data, "%Y-%m-%d")


wig$Roznica <- round((1 - wig$Zamkniecie / wig$Otwarcie) * 100, digits = 1);
if(FALSE) {
  wig$Roznica <- apply(wig, 1, FUN = function(x) {
    d <- as.numeric(x[7]);
    if (d >= 1) return (1)
    else if (d <= -1) return (-1)
    return (0);
  });
}
ui <- fluidPage(
  
  # App title ----
  titlePanel("PREDYKCJA WIG 20"),
  
  sidebarLayout(
    
    sidebarPanel(
      dateInput("date1", "Date:", value = "2012-01-01"),
      
      sliderInput("obs", "Ilość obserwacji:",
                  min = 1, max = 1000, value = 150
      ),
      sliderInput("days", "Ilość dni do przewidzenia:",
                  min = 2, max = 30, value = 2, step = 1
      ),
      sliderInput("mldays", "Ilość dni uczących wstecz (tylko dla ML):",
                  min = 1, max = 30, value = 2, step = 1
      ),
      selectInput("type", "Alogrytm:",
                  c("ETS" = 'ets', "ARIMA" = 'arima', "fracdiff" = "fracdiff",
                    "HoltWinters" = 'HoltWinters', "StructTS" = "StructTS", "Machine Learning" = "ml"))
    ),
    mainPanel(
      textOutput("selected_var"),
      plotOutput("plot")
    )
  )
)


server <- function(input, output) {
  
  df <-reactive({
    
    if (input$type == "ml") {
      df <- 'ml'
    } else if (input$type == "HoltWinters") {
      df <- HoltWinters
    }
    else if (input$type == "ets") {
      df <-ets
    }
    else if (input$type == "arima") {
      df <-auto.arima
    }
    else if (input$type == "fracdiff") {
      df <-fracdiff
    }
    else if (input$type == "StructTS") {
      df <-StructTS
    }
    else {
      df <- HoltWinters
    }
    return(df)
  })
  
  output$plot = renderPlot({
    testFunction(wig, input$date1, input$obs, input$days, df(), input$mldays)
  })
  
}


shinyApp(ui = ui, server = server)