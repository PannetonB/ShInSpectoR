#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries ----
library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(plotly)
library(dplyr)
library(shinyjqui)
library(rhandsontable)
library(reactlog)
library(ggplot2)
library(ggthemes)
library(pls)
#reactlog_enable()

# Define UI for application that draws a histogram

# The interface ----

shinyUI(fluidPage(
    useShinyjs(),
    ##Darker horizontal line ----
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}")),
        tags$style(HTML("table {table-layout: fixed;}"))    #For setting column widths,
        
    ),

    ## Application title ----
    fluidRow(
        column(6,titlePanel("InSpecteuR on Shiny 0.1"),div(style = "height:10px;")),
        column(6,align="right", div(style = "height:10px;"), 
               img(src="InSpecteuR_Logo_Small.png"))
    ),
        
    ## Main tabs ----
    navbarPage("Main tabs",
               id="tabs",
               ### Data tab ----
               tabPanel("Data",
                    sidebarLayout(
                        sidebarPanel(width=2,
                            fileInput("files", NULL,
                                      buttonLabel = "Fichiers X et Y",
                                      accept = c(".txt"),
                                      multiple=T),
                            
                            h4("Y data file name: "),
                            textOutput("yFileName"),
                            hr(),
                                     
                            selectInput("Xs","Spectra files",
                                        choices=character(0L), multiple = T),
                            hr(),
                            h2("PCA"),
                            selectInput("PCA_data","X data for PCA",
                                        choices=character(0L), multiple=F),
                            sliderInput("npcs","Number of PCs",1,20,2,1),
                            selectInput("pc1","PC on horizontal axis",
                                        choices="PC1", multiple=F),
                            
                            selectInput("pc2","PC on vertital axis",
                                        choices="PC2", multiple=F),
                            
                            selectInput("pcaPtColorBy", "Pick factor for point coloring",
                                        choices="ID", multiple=F),
                            actionButton('plotloadings', 'Plot loadings')
                            ),
                        
                        
                        mainPanel(width = 10,
                             # Show a plot of the generated distribution
                            column(4, 
                                   actionButton('clearRows', 'Clear Selection'),
                                   actionButton("deleteRows", "Delete Selected Rows"),
                                   actionButton("restoreOriData", "Restore data set"),
                                   hr(style = "border-top: 1px solid #FFFFFF;"),
                                   fluidRow(DT::dataTableOutput('Ys',
                                                                height='auto',
                                                                width='900px'
                                                                )
                                            )
                                   ),
                            column(6, offset=2, 
                                   fluidRow(plotlyOutput("spectraPlots", 
                                                       height="400px",
                                                       width="auto")
                                            ),
                                   fluidRow(plotlyOutput("acpPlots",
                                                            height="400px",
                                                            width="auto")
                                            )
                                
                            )
                        )
                    ),
                    bsModal("modalExample", 
                            "Loading Plots", 
                            "plotloadings", # <----set the observer to the right button
                            size = "large",
                            plotlyOutput("loadingPlots")
                    ) 
                    
               ),
               ### Preprocessing tab ----
               tabPanel("Preprocessing",
                        hr(),
                        h3("TRUNCATION"),
                        DT::dataTableOutput("PreProsTrunc", width = '800px'),
                        hr(),
                        h3("PER SPECTRUM NORMALIZATION"),
                        tags$div(id = 'placeholder1'),
                        tags$div(id = 'placeholder2'),
                        hr(),
                        h3("Action buttons"),
                        actionButton("applyPrePro","Apply"),
                        actionButton("savePrePro", "Save"),
                        actionButton("loadPrePro", "Load"),
                        hr()
               ),
               
               
               ### PCA tab ----
               tabPanel("PCA"),
               
               ### PLSDA tab ----
               tabPanel("PLSDA"),
               
               ### PLS tab ----
               tabPanel("PLS",
                        sidebarLayout(
                          sidebarPanel(width=2,
                                       hr(),
                                       h3('Modeling definitions'),
                                       hr(),
                                       selectInput("XsForPLS","Spectra files for PLS",
                                                   choices=character(0L), multiple = T),
                                       selectInput("YForPLS","Variable to predict",
                                                   choices=character(0L), multiple = F),
                                       hr(),
                                       sidebarLayout(
                                         sidebarPanel(width=12,
                                                      h4('Train control options'),
                                                      selectInput("ResamplingForPLS", 'Resampling method',
                                                                  choices=c('None','CV','LOO'),
                                                                  selected='CV'),
                                                      numericInput("NbLVForPLS", "Nb of LVs(max)",
                                                                  min=1, max=25, step=1, value=5),
                                                      hr(),
                                                      selectInput("AggregateForPLS","Aggregation method",
                                                                 choices=c('Concatenate spectra','Average predictions'),
                                                                 selected = 'Concatenate spectra')
                                         ),
                                          mainPanel(width=0)
                                      ),
                                      hr(),
                                      actionButton('ComputePLS',strong("Compute model")),
                                      actionButton('SavePLS',strong('Save model'))
                          ),
                          
                          
                          mainPanel(width = 10,
                                    column(2,
                                           actionButton('PLSvalidationPlot', 'Plot validation'),
                                           numericInput('NbLVPLS_Sel', 'Pick a number of LVs',
                                                        min=1,max=25,step=1,value=1),
                                           hr(),
                                           h4('Prediction plots'),
                                           actionButton("PlotPLSPred","Plot"),
                                           radioButtons("PredPlotTypePLS",'',
                                                        choices=c("train","validation"),
                                                        selected="validation"),
                                           selectInput("PLSPredPlotColorBy", "Color by",
                                                       choices=c()),
                                           selectInput("PLSPredPlotLabel", "Label with",
                                                       choices=c()),
                                           hr(),
                                           actionButton('PLSBCoeffPlot',"B-coeff plot"),
                                           hr(),
                                           h4('Score plot'),
                                           actionButton('PLSScorePlot','Plot'),
                                           numericInput('PLSScorePlotFirstLV', 'LV on x-axis',
                                                       min=1, max=2, step=1, value=1),
                                           numericInput('PLSScorePlotSecondLV', 'LV on y-axis',
                                                        min=1, max=2, step=1, value=1),
                                           selectInput("PLSScorePlotColorBy", "Color by",
                                                       choices=c()),
                                           hr(),
                                           actionButton('ShowPLSPredTable',"Show prediction table"),
                                           bsModal("PLSPreds", "PLS predictions", "ShowPLSPredTable", size = "large",
                                                   dataTableOutput("PlsPredTable"),
                                                   actionButton('savePLSPreds','Save'))
                                           
                                    ),
                                    column(6, offset=2, 
                                           textOutput("PLSPlotID"),
                                           tags$head(tags$style("#PLSPlotID{
                                                                 font-size: 20px;
                                                                 font-style: bold;
                                                                 }"
                                                                           )
                                                        ),
                                           plotlyOutput("PLSPlots", 
                                                                 height="600px",
                                                                 width="auto"
                                                        ),
                                           verbatimTextOutput('PLSConsole',placeholder = T),
                                           tags$head(tags$style("#PLSConsole{
                                                color:blue;
                                                font-size:14px; 
                                                overflow-y:scroll; max-height: 300px; background: ghostwhite;}"))
                                           ),
                                    
                                           
                                    )
                          )
                        ),
               
               ### Apply models tab ----
               tabPanel("Apply models")
               
               
    )
))
