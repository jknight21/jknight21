library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)

###############
# import data #
###############
skateboards <- read_csv("electric_skateboards.txt")

#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################

hist_choice_values <- c("price","range","top_speed","weight","battery")
hist_choice_names <- c("Price","Range","Top Speed","Weight","Battery")
names(hist_choice_values) <- hist_choice_names

drv_choices <-  unique(skateboards$drive)

size_choice_values <- c("range", "top_speed", "battery")
size_choice_names <- c("Range", "Top Speed", "Battery")
names(size_choice_values) <- size_choice_names

name_choices <- unique(skateboards$board)

cmpy_choices <- unique(skateboards$company)

############
#    ui    #
############
ui <- navbarPage(
  
  title="Electric Skateboards",
  
  fluidPage(theme = shinytheme("cerulean")),
  
  tabPanel(
    title = "Histogram",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "histvar"
                    , label = "Choose a variable of interest to plot:"
                    , choices = hist_choice_values
                    , selected = "price"),
        checkboxGroupInput(inputId = "drv"
                           , label = "Include drive types:"
                           , choices = drv_choices
                           , selected = drv_choices
                           , inline = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "hist")
      )
    )
  ),
  
  tabPanel(
    title = "Scatterplot",
    
    sidebarLayout(
      
      sidebarPanel(
        radioButtons(inputId = "pt_size"
                     , label = "Size points by:"
                     , choices = size_choice_values
                     , selected = "weight"),
        selectizeInput(inputId = "id_name"
                    , label = "Identify skateboard(s) in the scatterplot:"
                    , choices = name_choices
                    , selected = NULL
                    , multiple = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "scatter")
      )
    )
  ),
  
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
  ),
  tabPanel(
    title = "Original Graph",
    
    sidebarLayout(
      sidebarPanel(
        tags$div(
          HTML(paste("Original figure was presented by "
                     , tags$a(href="https://www.electricskateboardhq.com/boards-comparison/", "HQ Skateboard")
                     , sep = ""))
        )
      ),
      mainPanel(
        h3("Information overload!"),
        plotOutput(outputId = "original")
      )
    )
  )
)

############
# server   #
############
server <- function(input,output){
  
  data_for_hist <- reactive({
    data <- filter(skateboards, drive %in% input$drv)
  })
  
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), aes_string(x = input$histvar)) +
      geom_histogram(color = "#2c7fb8", fill = "#7fcdbb", alpha = 0.7) +
      labs(x = hist_choice_names[hist_choice_values == input$histvar]
           , y = "Number of Skateboards")
  })
  
  output$scatter <- renderPlot({
    skateboards %>%
      filter(drive != "Direct") %>%
    ggplot(aes_string(x="range", y="top_speed", size = input$pt_size)) +
      geom_point(color = "#2c7fb8") +
      labs(x = "Range (miles)", y = "Top Speed (mph)"
           , title = "Electric Skateboards", subtitle = "August 2018"
           , size = size_choice_names[size_choice_values == input$pt_size]) +
      geom_label_repel(data = filter(skateboards, board %in% input$id_name)
                       , aes(label = board), show.legend = FALSE) +
      facet_grid(~drive) 
  })
  
  data_for_table <- reactive({
    data <- filter(skateboards, company %in% input$cmpy)
  })
  
  output$table <- DT::renderDataTable({ 
    data_for_table()
  })
  
  output$original <- renderPlot({
    ggplot(data = skateboards, aes(x=range, y=top_speed, color=company
                                   , shape = drive, size = weight)) +
      geom_point() +
      geom_text(aes(label = board), hjust = 0, nudge_x = 0.05, size=3) +
      labs(x = "Range (miles)", y = "Top Speed (mph)"
           , title = "Electric Skateboards", subtitle = "August 2018"
           , shape = "Drive type", size = "Weight of board") +
      guides(color = FALSE)
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
