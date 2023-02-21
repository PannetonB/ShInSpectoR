library(shiny)

library(shiny)



ui <- fluidPage(
  selectInput("Xs","Spectra files",
              choices=list("EX300","EX400","EX500"), 
              selected="EX300",
              multiple = T),
  h4('Dynamic UI section'),
  
  tags$div(id = 'placeholder1'),
  hr(),
  verbatimTextOutput('feedback',placeholder = T)
)

server <- function(input, output, session) {
  
  insertedUI <<- reactiveVal(c())
  
  observe({
    XDataList <<- sort(input$Xs)
    isolate(if (length(XDataList>0)){
      for (k in insertedUI()){
        id <- k
        removeUI(selector = paste0('#', id))
      }
      insertedUI(c())
      
      for (kk in XDataList){
        id <- kk
        insertedUI(c(insertedUI(),id))
        insertUI(
          selector = "#placeholder1",
          where = "beforeEnd",
          ui = tags$div(id=id,
                        flowLayout(
                          radioButtons(paste0(id,"_A"),
                                       kk,
                                       choiceNames = list(
                                         "None",
                                         "Closure (mean=1)",
                                         "Waveband"
                                       ),
                                       selected = "closure",
                                       choiceValues = list(
                                         "none", "closure", "waveband"
                                       ),
                                       inline = T),
                          numericInput(paste0(id,"_B"),"A number",value=1,min=1,max=10,step=1)
                        )
          )
        )
      }
    })
  })
  
  observe({
    outtxt <- character()
    for (k in insertedUI()){
      outtxt <- paste0(outtxt,k,": ")
      for (j in LETTERS[1:7])
        outtxt <- paste(outtxt,
                        input[[paste(k,j,sep="_")]])
      outtxt <- paste0(outtxt,"\n")
    }
    output$feedback<-renderText(outtxt)
  })
  
}

shinyApp(ui, server)