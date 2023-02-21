library(shiny)
library(DT)
mydt <- data.frame(
  EchId = paste("Ech",1:5,sep="_"),
  Doit = c(FALSE,TRUE,FALSE,TRUE,FALSE),
  W=c(3,6,4,2,6)
)
shinyApp(
  ui = fluidPage(fluidRow(column(12, DTOutput('tbl', width='500px')))),
  server = function(input, output) {
    output$tbl = renderDT(
      mydt, options = list(lengthChange = FALSE),
      editable=T
    )
  }
)
