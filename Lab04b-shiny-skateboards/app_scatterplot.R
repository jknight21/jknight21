# this code reproduces the scatterplot panel only from the electric skateboards app
library(shiny)
library(tidyverse)
library(ggrepel)

###############
# import data #
###############
skateboards <- read_csv("electric_skateboards.txt")

###################################################
# define choice values and labels for user inputs #
###################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)

# for radio button, can be separate 
# (have choiceValues and choiceNames options, rather than just choices)
size_choice_values <- c("price", "weight", "battery")
size_choice_names <- c("Price", "Weight", "Battery")
names(size_choice_values) <- size_choice_names

# for selectizeInput choices for skateboard name, pull directly from data
name_choices <- unique(skateboards$board)


############
#    ui    #
############
ui <- navbarPage(
  
  title="Electric Skateboards",
  
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
  )
)

############
# server   #
############
server <- function(input,output){
  
  # TAB 2: INTERACTIVE SCATTERPLOT 
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
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)