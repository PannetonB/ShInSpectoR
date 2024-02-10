#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- basicPage(
  textOutput("text"),
  tabsetPanel(id = "tabs",
              tabPanel(
                "Signal", id = "signal", plotOutput("signalplot", height=1000)
              ),
              tabPanel("low", id = "low", plotOutput("looseplot", height=1000)),
              tabPanel("medium", id = "medium", plotOutput("mediumplot", height=1000)),
              tabPanel("tight", id = "tight", plotOutput("tightplot", height=1000))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$signalplot<-renderPlot({
    plot(c(6,7,8),c(5,7,5))
  })
  output$looseplot<-renderPlot({
    plot(c(7,7,8),c(5,0,5))
  })
}


