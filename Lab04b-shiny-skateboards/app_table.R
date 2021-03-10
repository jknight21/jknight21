library(shiny)
library(tidyverse)
library(DT)

###############
# import data #
###############
skateboards <- read_csv("electric_skateboards.txt")

###################################################
# define choice values and labels for user inputs #
###################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)

# for selectizeInput choices for company name, pull directly from data
cmpy_choices <- unique(skateboards$company)

############
#    ui    #
############
ui <- navbarPage(
  
  title="Electric Skateboards",
  
  tabPanel(
    title = "Table",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "cmpy"
                       , label = "Choose one or more companies:"
                       , choices = cmpy_choices
                       , selected = "DIYElectric"
                       , multiple = TRUE)
      ),
      mainPanel(
        DT::dataTableOutput(outputId = "table")
      )
    )
  )
)

############
# server   #
############
server <- function(input,output){
  
  # TAB 3: TABLE
  data_for_table <- reactive({
    data <- filter(skateboards, company %in% input$cmpy)
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)