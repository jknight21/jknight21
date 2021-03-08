# 02-two-outputs.R
# ~ 01:00:00

library(shiny)

ui <- fluidPage(
  navlistPanel(              
    tabPanel(title = "Normal data",
             plotOutput("norm"),
             actionButton("renorm", "Resample")
    ),
    tabPanel(title = "Uniform data",
             plotOutput("unif"),
             actionButton("reunif", "Resample")
    ),
    tabPanel(title = "Chi Squared data",
             plotOutput("chisq"),
             actionButton("rechisq", "Resample")
    )
  ),
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num), main = input$title)
  })
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)
