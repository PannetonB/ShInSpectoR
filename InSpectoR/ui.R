#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram

# Load libraries ----

lesLibrairies <-
  c(  "shiny",
      "shinyjs",
      "shinyBS",
      "shinyFiles",
      "shinythemes",
      "DT",
      "plotly",
      "dplyr",
      "shinyjqui",
      "rhandsontable",
      "reactlog",
      "ggplot2",
      "ggthemes",
      "pls",
      "waiter",
      "GGally",
      "ggpubr",
      "prospectr",
      "caret",
      "paletteer",
      "here",
      "grid",
      "gridExtra"
  )

cat("Loading libraires!")

lapply(lesLibrairies, require, character.only = TRUE)
# The interface ----

shinyUI(fluidPage(
    theme = shinytheme("cerulean"),
    useShinyjs(),
    use_waiter(),
    ##Darker horizontal line ----
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}")),
        #For setting column widths,
        tags$style(HTML("table {table-layout: fixed;}")), 
        #Size of modal windows
        tags$head(tags$style(".modal-body{ min-height:800px}")),
        tags$style(HTML('.modal-lg {width: 1200px;}')),
        #For x scroll fix in datatable filtering
        tags$style("
              .datatables-scroll {
                overflow-x: auto;
                width: 100%;
                max-height: 500px;
              }
              .datatables-scroll .dataTable thead tr th,
              .datatables-scroll thead tr td {
                position: sticky;
                background-color: #FFFFFF;
              }
              .datatables-scroll thead tr th {
                top: 0;
              }
              .datatables-scroll thead tr td {
                top: 2.45em;
              }
            ")
        
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
                                      buttonLabel = "Fichier Y",
                                      accept = c(".txt"),
                                      multiple=F),
                            
                            h4("Y data file name: "),
                            textOutput("yFileName"),
                            hr(),
                                     
                            selectInput("Xs","Spectra files",
                                        choices=character(0L), multiple = T),
                            hr(),
                            h2("PCA"),
                            selectInput("PCA_data","X data for PCA",
                                        choices=character(0L), multiple=F),
                            selectInput("npcs","Number of PCs",
                                        choices = 1:2),
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
                            column(5, 
                                   actionButton('clearRows', 'Clear Selection'),
                                   actionButton("deleteRows", "Delete Selected"),
                                   actionButton("restoreOriData", "Restore data set"),
                                   actionButton("clearAllFilters","Clear filters"),
                                   hr(),
                                   actionButton("selectAll","Select all shown"),
                                   actionButton("deleteAll", "Delete all shown"),
                                   actionButton("flipSelection","Set selection complement"),
                                   actionButton("saveEdited","Save edited data"),
                                   hr(style = "border-top: 1px solid #FFFFFF;"),
                                   fluidRow(DT::dataTableOutput('Ys',
                                                                height='auto',
                                                                width='auto'
                                                                )
                                            )
                                   ),
                            column(6, offset=1, 
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
                            plotlyOutput("loadingPlots", height="800px")
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
                        shinySaveButton("FSavePrePro", "Save file", "Save file as ...", 
                                        filetype=list(RData="RData")),
                        shinyFilesButton("FLoadPrePro","Load",
                                         "Select a preprocessing parameter file",
                                         multiple = F,
                                         filetype=list(RData="RData")),
                        # actionButton("loadPrePro", "Load"),
                        hr(),
                        h4('Per spectrum option feedback'),
                        verbatimTextOutput('feedback',placeholder = T)
               ),
               
               
               ### PCA tab ----
               tabPanel("PCA",
                        sidebarLayout(
                          sidebarPanel(width=3,
                                         h4(strong('Data options - spectra are concatenated')),
                                         selectInput('XsforPCA','Spectrum types for PCA',
                                                     choices=character(0L),multiple=T),
                                         selectInput("NPCsforPCA", "Select number of PCs",
                                                     choices=2:2),
                                         hr(),
                                         h4(strong('Plotting options')),
                                         selectInput('PCATopPlotType','Choose plot type',
                                                     choices = c('Scores','Loadings','Screeplot','OD_SD'),
                                                     selected = 'Screeplot'),
                                         selectInput('XAxisPCAPlot','Pick PC for X-axis score plot or 1rst loading',
                                                     choices = paste0("PC",c(1:2)),
                                                     selected="PC1"),
                                         selectInput('YAxisPCAPlot','Pick PC for Y-axis score plot or 1rst loading',
                                                     choices = paste0("PC",c(1:2)),
                                                     selected="PC2"),
                                         selectInput("PCAPlotColorBy", "Color by",
                                                     choices=c()),
                                         hr(),
                                         h4(strong('Saving results')),
                                         shinySaveButton("PCAScoresSave", strong("Save Scores"),
                                                         "Define file name", 
                                                         filetype=list(txt = "txt")),
                                         shinySaveButton("PCAModelSave", strong("Save model"),
                                                         "Define file name", 
                                                         filetype=list(RData = "RDATA")),
                                         h4(strong('Infos for saving PCA model')),
                                         textAreaInput('PCADescript', "Short description", 
                                                       value="Description",
                                                       width="300px", height="150px")
                                       
                                       ),
                          mainPanel(width=9,
                                    plotlyOutput("PCATopPlot", 
                                                 height="800px",
                                                 width="auto")
                                    )
                         )
                       ),
               
               ### PLSDA tab ----
               tabPanel("PLSDA",
                        sidebarLayout(
                          sidebarPanel(width=3,
                                       h3('Modeling definitions'),
                                       hr(),
                                       selectInput("XsForPLSDA","Spectra files for PLS",
                                                   choices=character(0L), multiple = T),
                                       selectInput("YForPLSDA","Variable to predict",
                                                   choices=character(0L), multiple = F),
                                       hr(),
                                       h4('Train control options'),
                                       
                                       fluidRow(
                                         column(6,
                                            selectInput("PropTrainingForPLSDA",
                                                        "Prop. of data for training",
                                                        choices = as.character(seq(0.5,0.85,0.05)),
                                                        selected = "0.6"),
                                            selectInput("ResamplingForPLSDA", 'Resampling method',
                                                        choices=c('none','cv','repeatedcv','LOOCV'),
                                                        selected='cv'),
                                            selectInput("NbFoldsForPLSDA","Number of folds",
                                                        choices=as.character(2:10),
                                                        selected=3),
                                            selectInput("NbRepetitionsForPLSDA",'Number of repetitions',
                                                        choices=as.character(1:5),
                                                        selected = 1)
                                         ),
                                         column(6,
                                            selectInput("NbLVForPLSDA", "Nb of LVs(max)",
                                                       choices = 1:20,
                                                       selected=2),
                                            selectInput('PreproForPLSDA', 'PreProcessing',
                                                        choices = c('None','Center')),
                                            selectInput('PredictMethodForPLSDA','Prediction method',
                                                        choices=c('softmax')),
                                            selectInput('PerfMetricForPLSDA','Performance metric',
                                                        choices=c('Kappa','Accuracy'))
                                         )
                                       ),
                                       hr(),
                                       selectInput('AggregOpForPLSDA','Aggregation operator',
                                                   choices=c("concatenate","median","max","prod","mean")),
                                       hr(),
                                       actionButton('ComputePLSDA',strong("Compute model"))
                               ),
                          
                          
                          mainPanel(width = 9,
                                    column(2,
                                           actionButton('PLSDAvalidationPlot', strong('Plot validation')),
                                           hr(),
                                           actionButton('PLSDAConfMatPlot', strong('Plot confusion matrix')),
                                           hr(),
                                           actionButton('PLSDAProbBoxPlot', strong('Prob. boxplots')),
                                           hr(),
                                           actionButton('PLSDAProbBiPlot', strong('Prob. biplots')),
                                           hr(),
                                           radioButtons('PLSDATrainTestBut', 'Select model type',
                                                        choices=c('Validation','Test')),
                                           hr(),
                                           actionButton('PLSDABCoeffPlot', strong('Plot B-coeffs')),
                                           hr(),
                                           actionButton('ShowPLSDAPredTable',strong("Show prediction table")),
                                           bsModal("PLSDAPreds", "PLSDA predictions",
                                                   "ShowPLSDAPredTable", size = "large",
                                                   dataTableOutput("PlsDAPredTable"),
                                                   actionButton('savePLSDAPreds',strong("Save"))),
                                                  
                                           hr(),
                                           shinySaveButton("FSavePLSDA", strong("Save model"),
                                                           "Save PLS model to file", 
                                                           filetype=list(RData="RData")),
                                           h4(strong('Infos for saving PLSDA model')),
                                           textAreaInput('PLSDADescript', "Short description", 
                                                         value="Description",
                                                         width="600px", height="auto")
                                           
                                    ),
                                    column(9, offset=1, 
                                           plotlyOutput("PLSDAPlots", 
                                                        height="600px",
                                                        width="auto"
                                           ),
                                           verbatimTextOutput('PLSDAConsole',placeholder = T),
                                           tags$head(tags$style("#PLSDAConsole{
                                                color:blue;
                                                font-size:14px; 
                                                overflow-y:scroll; max-height: 500px; background: ghostwhite;}"))
                                    )
                                    
                                    
                              )
                            ),
                            bsModal("PLSDABiPlots", 
                                    "Probability biplots", 
                                    "PLSDAProbBiPlot", # <----set the observer to the right button
                                    size = "large",
                                    plotOutput("plsdaProbBiPlots", height="800px"),
                                    actionButton("savePLSDAProbBiplot","Save")
                            ) 
                        ),
               
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
                                                        selectInput("NbLVForPLS", "Nb of LVs(max)",
                                                                    choices=1:25,
                                                                    selected=5),
                                                        hr(),
                                                        selectInput("AggregateForPLS","Aggregation method",
                                                                   choices=c('concatenate spectra'),
                                                                   selected = 'concatenate spectra')
                                           ),
                                            mainPanel(width=0)
                                        ),
                                        hr(),
                                        actionButton('ComputePLS',strong("Compute model")),
                                        shinySaveButton("FSavePLS", strong("Save model"),
                                                        "Save PLS model to file", 
                                                        filetype=list(RData="RData")),
                                        h4(strong('Infos for saving PLS model')),
                                        textAreaInput('PLSDescript', "Short description", 
                                                      value="Description",
                                                      width="300px", height="150px")
                            ),
                          
                          
                            mainPanel(width = 10,
                                      column(2,
                                               actionButton('PLSvalidationPlot', 'Plot validation'),
                                               selectInput('NbLVPLS_Sel', 'Pick a number of LVs',
                                                            choices=1:25,
                                                            selected=2),
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
                                               selectInput('PLSScorePlotFirstLV', 'LV on x-axis',
                                                           choices=1:2),
                                               selectInput('PLSScorePlotSecondLV', 'LV on y-axis',
                                                            choices=1:2),
                                               selectInput("PLSScorePlotColorBy", "Color by",
                                                           choices=c()),
                                               hr(),
                                               actionButton('ShowPLSPredTable',"Show prediction table"),
                                               bsModal("PLSPreds", "PLS predictions", "ShowPLSPredTable", size = "large",
                                                       dataTableOutput("PlsPredTable"),
                                                       actionButton('savePLSPreds',strong('Save')))
                                      ),
                                      column(9, offset=1,
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
                                               )  
                                  )
                            )
                        ),
               
               ### Apply models tab ----
               tabPanel("Apply models",
                        sidebarLayout(
                          sidebarPanel(width=2,
                                         hr(),
                                         h3('APPLY MODELS'),
                                         hr(),
                                         shinyFilesButton("FLoadModel",strong("Load model"),
                                                          "Select a model file",
                                                          multiple = F,
                                                          filetype=list(RData="RData")),
                                         hr(),
                                         h4("Model type"),
                                         textOutput("modelType"),
                                         tags$head(tags$style(HTML("
                                                                  #modelType {
                                                                    font-size: 18px;
                                                                    color: blue;
                                                                  }
                                                                  "))),
                                         hr(),
                                         h4(strong("Model description")),
                                         textOutput('modelDescOnApply'),
                                         tags$head(tags$style(HTML("
                                                                  #modelDescOnApply {
                                                                    font-size: 16px;
                                                                    color: blue;
                                                                  }
                                                                  "))),
                                         hr(),
                                         h4(strong("Spectrum type requirements")),
                                         verbatimTextOutput('dataTypeOnApply',
                                                            placeholder = T),
                                         tags$head(tags$style(HTML("
                                                                  #dataTypeOnApply {
                                                                    font-size: 18px;
                                                                    color: blue;
                                                                  }
                                                                  "))),
                                         hr(),
                                         selectInput('FirstPCApplyPCA', 'First PC',
                                                     choices = "1"),
                                       
                                         selectInput('LastPCApplyPCA', 'Last PC',
                                                     choices = "1"),
                                         selectInput("factorsToShow","Factor for table output",
                                                     choice=NULL,
                                                     multiple=T),
                                         actionButton("applyModel",strong("Apply")),
                                         shinySaveButton("saveModelResults", strong("Save results"),
                                                         "Define file name", 
                                                         filetype=list(txt = "txt"))
                                      ),


                            mainPanel(width = 10, 

                                      column(6,
                                                plotOutput("modelPlot",
                                                                height="800px",
                                                                width="auto"
                                                )
                                            ),
                                        column(6,
                                                 DT::dataTableOutput('modelTable',
                                                                     height='800',
                                                                     width='auto'
                                                 )
                                               )
                                      )
                       )  
             )
          )
     )
)
