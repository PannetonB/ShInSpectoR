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
               img(src="InSpecteuR_Logo_Small.png"),)
    ),
        
    ## Main tabs ----
    navbarPage("Main tabs",
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
                                        choices=character(0L), multiple=F),
                            
                            selectInput("pc2","PC on vertital axis",
                                        choices=character(0L), multiple=F),
                            
                            selectInput("pcaPtColorBy", "Pick factor for point coloring",
                                        choices=character(0L), multiple=F),
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
                        h3("TRUNCATION"),
                        DT::dataTableOutput("PreProsTrunc", width = '800px'),
                        h3("PER SPECTRUM NORMALIZATION"),
                        DT::dataTableOutput('PreProsPerSpectra'),
                        h3("SAVITZKY-GOLAY"),
                        DT::dataTableOutput('PreProsSavgol')),
               
               
               ### PCA tab ----
               tabPanel("PCA"),
               
               ### PLSDA tab ----
               tabPanel("PLSDA"),
               
               ### PLS tab ----
               tabPanel("PLS"),
               
               ### Apply models tab ----
               tabPanel("Apply models")
               
               
    )
))
