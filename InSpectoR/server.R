#
#Server for ShInSpectoR

#Increase upload limit to 300 Megs

options(shiny.maxRequestSize=300*1024^2) 




shinyServer(function(input, output, session) {
  
  session$onSessionEnded(function(){
      stopApp()
  })

  
  #SET UP PROJECT PATH
  projectDir <<- rstudioapi::selectDirectory(
    caption="Select root working directory for this session."
  )
  
  # leFichier <- here("InSpectoR","www","defPath.RData")
  # load(leFichier)

    # To make modal window for loading plots on Data tab draggable ----
    jqui_draggable('#modalExample')
  
    # *********************************************************************
    
    #Hide all tabs but Data
    hideTab(inputId = "tabs", target = 'Preprocessing')
    hideTab(inputId = "tabs", target = 'PCA')
    hideTab(inputId = "tabs", target = 'PLS')
    hideTab(inputId = "tabs", target = 'PLSDA')
    hideTab(inputId = "tabs", target = 'Apply models')
    
    #Hide some UI elements
    shinyjs::hide("PLSDescript")
    shinyjs::hide("FSavePLS")
    shinyjs::hide("FSavePLSDA")
    shinyjs::hide("PLSDADescript")
    shinyjs::hide("applyModel")
    shinyjs::hide("saveModelResults")
    # *********************************************************************
    
    
    # Defines table contents as reactive and create proxys ----
    ## On Data tab ----
    Yvalues <- reactiveValues(dfWorking = Ys_df)
    output$Ys <- renderDT({
      Yvalues$dfWorking
    })
    proxy_Ys = dataTableProxy('Ys')
    
    # *********************************************************************
    
    ## On Prepro tab ----
    PPvaluesTrunc <- reactiveValues(dfWorking = data.frame())
    output$PreProsTrunc <- renderDataTable({
        PPvaluesTrunc$dfWorking
    })

    proxy_PreProTrunc = dataTableProxy('PreProsTrunc')
    
    # *********************************************************************
    
    #Placeholder for per spectrum prepro options UI
    inserted_perSpectrumOptions <<- reactiveVal(c())
    
    # *********************************************************************
    
    # On Data tab ----
    
    # *********************************************************************
    
    # Data tab ----
    ## Reacts to Data tab activation ----
    
    observeEvent(input$tabs,{
      if(input$tabs == "Data"){ #set up the pages
        
        #In case user did not apply prepros
        preproParams <- collectPreProParams(PPvaluesTrunc,input)
        Apply_PrePro(preproParams,input)
        lesChoix <- computePCAonRaw(as.numeric(input$npcs),doRayleigh = FALSE)
        
        #Force redraw
        s <- unique(input$Ys_rows_selected)
        if (!is.null(s)){
          proxy_Ys %>% selectRows(NULL)
          proxy_Ys %>% selectRows(s)
        }
      }
    }, ignoreInit = TRUE)
        
    # *********************************************************************
    
    
    ## Shows Y filename. Triggered by input$files ----
    output$yFileName <- renderText({
            inFile <- input$files
            if (is.null(inFile)){
                return("None selected")
            }
            indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
            if (length(indi)==0){
              return("No valid Y file selected")
            }
        })
    
    # *********************************************************************
    
    ## Plot spectra ----
    # Triggered by selection in Ys
    output$spectraPlots <- renderPlotly({
        s <- unique(input$Ys_rows_selected)
        if (length(s)) {
            mycolors <- colorRampPalette(mesCouleurs)(length(mesCouleurs))
            s <- sort(s)
            dfs_plotly <- dfForPlotly(XDataList,XData_p,s)
            nColor <- length(unique(dfs_plotly[["ID"]]))
            p <- plotly::plot_ly(dfs_plotly,x=~X,y=~Spectra,
                                 color = ~ID, colors = mycolors[1:nColor],
                                 type = 'scatter', mode = 'lines',
                                 linetype = ~DType, 
                                 name=~names) %>%
                layout(showlegend=T,
                       legend=list(title=list(text='<b>Data type - Sample ID</b>'),
                    #  x=0.75, y=0.98),
                       xaxis = list(title = 'Wavelength [nm] or Wavenumber[cm<sup>-1</sup>]'), 
                       yaxis = list(title = 'Spectra [A.U.]')))
            
        }
    })
    
    # *********************************************************************
    
    ## Plot PCA ----
    # Triggered by selection in Ys or changes in npcs, PC1, PC2, pctPtColoyBy, PCA_Data
    output$acpPlots <- renderPlotly({
        s <- unique(input$Ys_rows_selected)
        dum <- as.numeric(input$npcs)
        mycolors <- colorRampPalette(mesCouleurs)(length(mesCouleurs))
        t <- list(
          family = "sans serif",
          size = 14,
          color = toRGB("red"))
        
        lePCA <- input$PCA_data
        lepc1 <- as.formula(paste0("~",input$pc1))
        lepc2 <- as.formula(paste0("~",input$pc2))
        ptColor <- as.formula(paste0("~",input$pcaPtColorBy))
        dfsPCA <- as.data.frame(PCAsDT[[lePCA]]$x)
        dfsPCA <- cbind(dfsPCA,Ys_df[input$pcaPtColorBy])
        nColor <- length(unique(dfsPCA[[input$pcaPtColorBy]]))
        suppressWarnings(colIndices <- (1:10 + rep(0,nColor))[1:nColor])
        
        ### Something was selected ----
        if (length(s)) {
            s <- sort(s)
            
            dfsPCA_unsel <- dfsPCA[-s,]
            dfsPCA_sel <- dfsPCA[s,]
            
            
            p <- plotly::plot_ly(source="pcaPlot"
                                 ) %>%
                add_markers(data=dfsPCA,          #Plot all points
                            x=lepc1, y=lepc2,
                            type = "scatter", mode = "markers",
                            color=ptColor,
                            colors = mycolors[colIndices],
                            size=8,
                            text=I(as.character(Ys_df[[1]])),
                            hovertext=as.character(Ys_df[,input$pcaPtColorBy]),
                            hovertemplate = paste('EchID: %{text}')) %>%
                add_markers(data=dfsPCA_sel,     #Circle selected in red
                            x=lepc1,y=lepc2,
                            type = "scatter", mode = "markers",
                            hoverinfo='skip',
                            marker = list(
                                color = toRGB("red"),
                                opacity = 1.0,
                                symbol = "circle-open",
                                size=12,
                                line=list(width=2)),
                            showlegend=FALSE) %>%
                add_text(data=dfsPCA_sel,           #Place sample ID near selected points
                         x=lepc1,y=lepc2,
                         text = paste0("  ",rownames(dfsPCA_sel)),
                         textfont = t, textposition = "top right",
                         showlegend=FALSE) %>% 
                layout(dragmode = "select") %>%
                event_register("plotly_click") %>%
                event_register("plotly_brushing") %>%
                event_register("plotly_doubleclick")
            
            # Add confidence limits for SDist and/or ODist
            p <- switch(input$pc1,
                   SDist = p %>% layout(shapes = list(vline(PCAsdt_dds_crit[[lePCA]][1]))),
                   ODist = p %>% layout(shapes = list(vline(PCAsdt_dds_crit[[lePCA]][2]))),
                   p
            )
            p <- switch(input$pc2,
                    SDist = p %>% layout(shapes = list(hline(PCAsdt_dds_crit[[lePCA]][1]))),
                    ODist = p %>% layout(shapes = list(hline(PCAsdt_dds_crit[[lePCA]][2]))),
                    p
            )
            
        }else   
        ### Nothing was selected ----
          if (nrow(dfsPCA)){
            p <- plotly::plot_ly(source="pcaPlot"
            ) %>%
            add_markers(data=dfsPCA,          #Plot all points
                        x=lepc1, y=lepc2,
                        type = "scatter", mode = "markers",
                        color=ptColor,
                        colors = mycolors[colIndices],
                        size=8,
                        text=I(as.character(Ys_df[[1]])),
                        hovertext=as.character(Ys_df[,input$pcaPtColorBy]),
                        hovertemplate = paste('EchID: %{text}')) %>% 
            layout(dragmode = "select") %>%
            event_register("plotly_click") %>%
            event_register("plotly_brushing") %>%
            event_register("plotly_doubleclick")
          
          # Add confidence limits for SDist and/or ODist
          p <- switch(input$pc1,
                      SDist = p %>% layout(shapes = list(vline(PCAsdt_dds_crit[[lePCA]][1]))),
                      ODist = p %>% layout(shapes = list(vline(PCAsdt_dds_crit[[lePCA]][2]))),
                      p
          )
          p <- switch(input$pc2,
                      SDist = p %>% layout(shapes = list(hline(PCAsdt_dds_crit[[lePCA]][1]))),
                      ODist = p %>% layout(shapes = list(hline(PCAsdt_dds_crit[[lePCA]][2]))),
                      p
          )
          
          }else  #no data in dfsPCA
          {
            plotly_empty(type='scatter',mode="markers",source="pcaPlot") %>%
            event_register("plotly_click") %>%
            event_register("plotly_brushing") %>%
            event_register("plotly_selected")
          }
    })
    
    # *********************************************************************
    
    ## Plot loadings in a modal window ----
    observeEvent(input$plotloadings,{
      output$loadingPlots <- renderPlotly({
          if (all(stringr::str_detect(c(input$pc1,input$pc2),"PC"))){
              lePCA <- PCAsDT[[input$PCA_data]]
              whichData <- which(input$PCA_data==names(PCAsDT))
              colPC1 <- which(colnames(lePCA$x)==input$pc1)
              colPC2 <- which(colnames(lePCA$x)==input$pc2)
              ys1 <- lePCA$rotation[,colPC1]  #First PC loadings
              ys2 <-lePCA$rotation[,colPC2]  #Second PC loadings
              xs <- as.numeric(rownames(lePCA$rotation))
              plotdf <- data.frame(Wavelength=xs, Loadings1=ys1,Loadings2=ys2)
              p1 <- plot_ly(plotdf, x = ~Wavelength, y = ~Loadings1 ,type='scatter',mode="line" ) %>%
                  layout(title = input$PCA_data,
                         xaxis=list(title='Wavenumber or Wavelength'), 
                         yaxis=list(title=input$pc1))
              p2 <- plot_ly(plotdf, x = ~Wavelength, y = ~Loadings2 ,type='scatter',mode="line" , showlegend=F) %>%
                  layout(xaxis=list(title='Wavenumber or Wavelength'), 
                         yaxis=list(title=input$pc2))
              subplot(p1, p2, nrows=2, shareX = TRUE, titleY = TRUE)
          }else
          {
              plotly_empty(type='scatter',mode="markers")
          }
      })
    })
    
    
    # ********************************************************************* 
   
    ## Toggles Plot Loadings action button ---- 
    # Both pc1 and pc2 inputs must be a principal components for button to show
    observe({ toggle(id="plotloadings", 
                     condition=all(stringr::str_detect(c(input$pc1,input$pc2),"PC")))})
    
    
    # *********************************************************************
    
     ## Loads data ----
    observeEvent(input$files, {
        k=1
        inFile <<- input$files
        if (is.null(inFile))
          return(NULL)
        
        indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
        if (length(indi)==0){
            showModal(modalDialog(       #Not a Y file!
              title="WARNING", 
              paste("Must select a Y file!"),
              icon=("alert - warning")
            ))
            return(NULL)
        }
        datasetID <- tools::file_path_sans_ext(inFile$name)
        dum <- sub(".*_", "", datasetID) 
        xFiles <- choose.files(default=paste0(projectDir,"\\*_",dum,"*.txt"))
        dataTypes <- sub("w.*","",basename(xFiles))
        if (any(duplicated(dataTypes))){
          showModal(modalDialog(       #More than 1 file for each data type.
            title="WARNING", 
            paste("Duplicated data type not accepted. ABORT. 
                  "),
            icon=("alert - warning")
          ))
          return(NULL)
        }
        # xFiles <<- xFiles
        dataDir <<- dirname(xFiles[1])
        indi <- which(!stringr::str_detect(xFiles,glob2rx("Y_*.txt")))
        if (length(indi)==0) 
          return('NULL')
        #Load Y data
        indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
        Ys_df <<- read.table(file=inFile$datapath[indi],header=TRUE,sep="\t",dec=".",
                             na.strings = c("","NA"), stringsAsFactors = T)
        #make.unique to deal with repeated sample ID.
        Ys_df[,1] <<- as.factor(make.unique(as.character(Ys_df[,1])))
        Ys_df <<- cbind(Ys_df,data.frame(NoSeq=seq(1:nrow(Ys_df))))
        #load all XData in All_XData
        indi <- 1:length(xFiles)
        # indiDebug <<- indi
        for (ii in 1:length(indi)){
            if (indi[ii]){
                # dum1<-read.table(file=inFile$datapath[ii],sep="\t", dec=".",header=FALSE)
                dum1<-read.table(file=xFiles[ii],sep="\t", dec=".",header=FALSE)
                dum1[-1,1] <- make.unique(as.character(dum1[-1,1]))
                test1 <- length(dum1[-1,1])==length(Ys_df[,1])
                test2 <- TRUE
                if (test1) test2 <- any(as.character(dum1[-1,1])!=as.character(Ys_df[,1]))
               
                if (test2){ 
                    showModal(modalDialog(       #Not matching
                        title="WARNING", 
                        paste("File ",inFile$name[ii], " is not valid! Will be ignored.",sep=""),
                        icon=("alert - warning")
                    ))
                    
                    indi[ii] <- FALSE
                    
                }else
                {
                  leNom <- tools::file_path_sans_ext(basename(xFiles[ii]))  
                  All_XData[[leNom]] <<- dum1
                }
            }
        }
        
        #Remove rows where constant values are found
        rowInd <- numeric()
        for (k in 1:length(All_XData)){
          rowInd <- unique(c(rowInd,
                             which(apply(All_XData[[k]][,-1], 1, 
                                         function(i) length(unique(i))==1)
                                   )))
        }
        if (length(rowInd)>0){
          showModal(modalDialog(       #Some constant lines
            title="WARNING", 
            paste("Some spectra are constants. Corresponding sample will be removed."),
            icon=("alert - warning")
          ))
          Ys_df <<- Ys_df[-(rowInd-1),]
          All_XData <<- lapply(All_XData, function(x) x[-rowInd,])
        }
        if (length(rowInd)==nrow(Ys_df)){
          showModal(modalDialog(       #Something really wrong
            title="WARNING", 
            paste("Invalid data set!"),
            icon=("alert - warning")
          ))
          return()
        }
        
        
        Ys_df <- droplevels(Ys_df)
        
        #Flip All_XData to wavelength in increasing order!
        All_XData <<- lapply(All_XData, function(x){
          x[,-1] <- x[,-1][,order(as.numeric(x[1,-1]))]
          x
        })
        
        ORI_Ys_df <<- Ys_df
        ORI_XData <<- All_XData
        
        
        #files are loaded - remove extension from filenames
        xFiles<- tools::file_path_sans_ext((basename(xFiles[indi])))
        
        #Sets max nb of pcs to 20 or the number of samples-1 if smaller
        nSamples <- nrow(All_XData[[1]])-1
        maxnCP <- ifelse (nSamples < 21, nSamples-1, 20 )
        updateSelectInput(session,"npcs",choices=2:maxnCP,
                          selected=2)
        
        lesChoix <- computePCAonRaw(as.numeric(input$npcs),
                                    doRayleigh = input$doRayleigh,
                                    isFirst=TRUE)
        
        #Populate Xs file selection and select first by default
        updateSelectInput(session, "Xs",               
                          choices=xFiles,
                          selected=xFiles[1])
        ALLXDataList <<- xFiles
        
        #Populate PCA data selection
        updateSelectInput(session,"PCA_data",
                          choices=xFiles,
                          selected=xFiles[1])
        
        
        updateSelectInput(session,"pcaPtColorBy",
                          choices=names(Filter(is.factor,Ys_df))[-1])
        
          
        #Populate select CPs to plot
        updateSelectInput(session,"pc1",
                          choices = lesChoix,
                          selected="SDist")
        updateSelectInput(session,"pc2",
                          choices = lesChoix,
                          selected="ODist")
       
        
        #Table of Ys
        dtable <- DT::datatable(Ys_df, width = '900px',
                                options=list(
                                    server=TRUE,
                                    autoWidth=FALSE,
                                    dom = "<lf<\"datatables-scroll\"t>ipr>",
                                    rownames = FALSE,
                                    class="compact",
                                    lengthMenu = list(c(10, 15, 20, -1), c('10', '15', '20','All')),
                                    pageLength = 10,
                                    # scrollX = TRUE,
                                    style = "bootstrap",
                                    columnDefs = list(
                                      list(orderable = TRUE, targets = "_all"),
                                      list(width = '15px', targets = 0),
                                      list(width = '100px', targets = 1:(ncol(Ys_df)-1)),
                                      list(className = "dt-center", targets = "_all")
                                      #columnDefs = list(list(orderable = TRUE, targets = 0)
                                    )
                                ),
                                filter=list(position = 'top', clear = FALSE))
        dtable$x$data[[1]] <- as.numeric(dtable$x$data[[1]])-1
        #Force name of column for sample ID to ID
        colnames(dtable$x$data)[2] <- "ID"  #by default row number (0 based) on
                                            # first column
        Yvalues$dfWorking <- dtable
        
        
        ###Show tabs ----
        showTab(inputId = "tabs", target = 'Preprocessing')
        showTab(inputId = "tabs", target = 'PCA')
        showTab(inputId = "tabs", target = 'PLS')
        showTab(inputId = "tabs", target = 'PLSDA')
        showTab(inputId = "tabs", target = 'Apply models')
        
    
        },
        ignoreInit = T
    )
    
    
    # *********************************************************************
    
    ## Reacts to number of PCs ----
    observeEvent(input$npcs,{
       
       lesChoix <- computePCAonRaw(as.numeric(input$npcs),doRayleigh=FALSE)
       
        #Populate select CPs to plot
        updateSelectInput(session,"pc1",
                          choices = lesChoix,
                          selected="SDist")
        updateSelectInput(session,"pc2",
                          choices = lesChoix,
                          selected="ODist")
        proxy_Ys %>% selectRows(1L)
        proxy_Ys %>% selectRows(NULL)
    }, ignoreInit = T)
    
    
    # *********************************************************************
    
    ## Reacts to X selection ----
    observe({
         XDataList <<- sort(input$Xs)
        isolate(if (length(XDataList>0)){
          #Update prepro tables 
          #Truncation
          truncDF <- data.frame(
            Spectra = XDataList,
            # LowerLimit = unlist(lapply(as.list(XDataList), function(iii)
            #                                      min(XData_p[[iii]][1,-1]))),
            LowerLimit = unlist(RayleighCutoffs[XDataList]),
            HigherLimit = unlist(lapply(as.list(XDataList), function(iii)
              max(All_XData[[iii]][1,-1]))),
            RayleighCutoffs = unlist(RayleighCutoffs[XDataList])
          )
          dtable <- datatable(truncDF,rownames = F, width='800px',
                              options = list(dom = 't',
                                             scrollX=TRUE),
                              editable = list(target = "cell", 
                                              disable = list(columns = 0))
                              
          )
          isolate(PPvaluesTrunc$dfWorking <- dtable)
          
          #Update per spectrum prepro options
          #Remove all entries in the perSpectrumOptions section
          for (k in inserted_perSpectrumOptions()){
            id <- k
            removeUI(selector = paste0('#', id))
          }
          inserted_perSpectrumOptions(c())
          
          #Loop on all selected data types
         
          for (kk in XDataList){
            wl <- as.numeric(All_XData[[kk]][1,-1])
            hiWL <- max(wl)
            lowWL <- as.numeric(RayleighCutoffs[kk])
            ctr <- round((hiWL+lowWL)/2)
            id <- kk
            inserted_perSpectrumOptions(c(inserted_perSpectrumOptions(),id))
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
                              numericInput(paste0(id,"_B"), "Band Center",ctr,
                                           lowWL,hiWL,1),
                              numericInput(paste0(id,"_C"), 'Bandwidth', 1,1,25,2),
                              checkboxInput(paste0(id,"_D"), strong('SAVITSKY-GOLAY'), FALSE, width='20px'),
                              numericInput(paste0(id,'_E'), 'Bandwidth (odd number only!',5,5,105,2),
                              numericInput(paste0(id,'_F'), 'Polynomial order',3,1,10,1),
                              numericInput(paste0(id,'_G'), 'Derivative order',0,0,2,1)
                              
                              
                            )
              )
            )
          }
          
          #Necessary as these values are not yet available!
          #NOT SURE IF STILL NEEDED BUT WORKS AS IS...
          if (length(XDataList)>0){
            dummyInput <- list()
            for (id in XDataList){
              unInput <- list("closure",ctr,1,FALSE,5,3,0)
              names(unInput) <- c(paste(id,LETTERS[1:7],sep="_"))
              dummyInput <- c(dummyInput,unInput)
            }
          }
          
          
          #Apply default prepro
          preproParams <- collectPreProParams(PPvaluesTrunc,dummyInput)
          Apply_PrePro(preproParams,input)
          lesChoix <- computePCAonRaw(as.numeric(input$npcs),doRayleigh = FALSE)

          
          #Force redraw
          isolate({
            proxy_Ys %>% selectRows(1L)
            proxy_Ys %>% selectRows(NULL)
            })
        })
    })
    
    
    # *********************************************************************
    
    ## Reacts to clearRows button ----
    observeEvent(input$clearRows, {
        proxy_Ys %>% selectRows(1L)
        proxy_Ys %>% selectRows(NULL)
    })
    
    
    # *********************************************************************
    
    ## Reacts to selectAll button ----
    observeEvent(input$selectAll, {
      rangees <- input$Ys_rows_all
      proxy_Ys %>% selectRows(rangees)
    })
    
    
    # *********************************************************************
    
    ## Reacts to clearAllFilters button ----
    observeEvent(input$clearAllFilters, {
      proxy_Ys %>% clearSearch()
    })
    
    
    
    # *********************************************************************
    
    ## Reacts to flipSelection button ----
    observeEvent(input$flipSelection, {
      lesCols <- (input$Ys_search_columns) != ""
      if (sum(lesCols)==1){
        #Perform operation
        leFiltre <- input$Ys_search_columns[lesCols]
        leFiltre <- gsub('[^[:alnum:],_ ]','',leFiltre)
        leFiltre <- unlist(strsplit(leFiltre,','))
        allOptions <- levels(Ys_df[,lesCols])
        leFiltre <- sapply(leFiltre, FUN= function(x) which(x==allOptions))
        r_state <- list()
        r_state$dataviewer_search_columns <- as.list(rep("",length(lesCols)))
        leFiltre <- paste0('[',
                          paste0('\"',
                                 allOptions[-leFiltre],
                                 '\"',collapse = ",")
                          ,']')
        r_state$dataviewer_search_columns[[which(lesCols)+1]] <-leFiltre
        mknl <- function(x) list(search = x)
        
        dtable <- DT::datatable(Ys_df, width = '900px',
                                options=list(
                                  server=TRUE,
                                  autoWidth=FALSE,
                                  dom = "<lf<\"datatables-scroll\"t>ipr>",
                                  rownames = FALSE,
                                  class="compact",
                                  lengthMenu = list(c(10, 15, 20, -1), c('10', '15', '20','All')),
                                  pageLength = 10,
                                  # scrollX = TRUE,
                                  style = "bootstrap",
                                  searchCols = lapply(r_state$dataviewer_search_columns, mknl),
                                  columnDefs = list(
                                    list(orderable = TRUE, targets = 0),
                                    list(width = '15px', targets = 0),
                                    list(width = '100px', targets = 1:(ncol(Ys_df)-1)),
                                    list(className = "dt-center", targets = "_all")
                                    #columnDefs = list(list(orderable = TRUE, targets = 0)
                                  ),
                                  search=list()
                                ),
                                filter=list(position = 'top', clear = FALSE))
        dtable$x$data[[1]] <- as.numeric(dtable$x$data[[1]])-1
        #Force name of column for sample ID to ID
        colnames(dtable$x$data)[2] <- "ID"  #by default row number (0 based) on
        # first column
        Yvalues$dfWorking <- dtable
       
      }else
      {#display warning
        showModal(modalDialog(
          title = "WARNING",
          "Only one filtered column allowed!"
        ))
      }
      
    })
    
    # *********************************************************************
    
    
    ## Reacts to deleteRows button ----
    observeEvent(input$deleteRows, {
        if (!is.null(input$Ys_rows_selected)) {
            lesRows <- unique(as.numeric(input$Ys_rows_selected))
            #Remove rows in All_XData, .
            #Recompute PCAs and update PCAs, PCAsdt_dds and PCAsdt_dds_crit
            Yvalues$dfWorking$x$data <- Yvalues$dfWorking$x$data[-lesRows,]
            Yvalues$dfWorking$x$data <- droplevels(Yvalues$dfWorking$x$data)
            Ys_df <<- Yvalues$dfWorking$x$data[,-1]
            for (k in 1:length(All_XData)){
                All_XData[[k]] <<- All_XData[[k]][-(1+lesRows),]
            }
            
            for (k in 1:length(XData_p)){
              XData_p[[k]] <<- XData_p[[k]][-(1+lesRows),]
            }
            lesChoix <- computePCAonRaw(as.numeric(input$npcs),doRayleigh=FALSE)
        } 
        proxy_Ys %>% selectRows(1L)
        proxy_Ys %>% selectRows(NULL)
    })
    
    # *********************************************************************
    
    
    ## Reacts to deleteAll button ----
    observeEvent(input$deleteAll, {
      lesRows <- input$Ys_rows_all
      
      #Remove rows in All_XData, .
      #Recompute PCAs and update PCAs, PCAsdt_dds and PCAsdt_dds_crit
      Yvalues$dfWorking$x$data <- Yvalues$dfWorking$x$data[-lesRows,]
      Yvalues$dfWorking$x$data <- droplevels(Yvalues$dfWorking$x$data)
      Ys_df <<- Yvalues$dfWorking$x$data[,-1]
      for (k in 1:length(All_XData)){
        All_XData[[k]] <<- All_XData[[k]][-(1+lesRows),]
      }
      
      for (k in 1:length(XData_p)){
        XData_p[[k]] <<- XData_p[[k]][-(1+lesRows),]
      }
      lesChoix <- computePCAonRaw(as.numeric(input$npcs),doRayleigh=FALSE)
      proxy_Ys %>% selectRows(1L)
      proxy_Ys %>% selectRows(NULL)
    })
    # *********************************************************************
    
    
    ## Reacts to saveEdited button ----
    observeEvent(input$saveEdited, {
      #See if there is a "Edited00" directory in the X files directory
      lesDirs <- list.dirs(paste0(dataDir))
      if (length(lesDirs)>1){
        lesEdited <- grep("Edited",lesDirs)
        if (length(lesEdited)==0){
          lastOne <- 0
        }else
        {
          lesEdited <- lesDirs[lesEdited]
          lastOne <- max(as.numeric(sub(".*Edited","",lesEdited)))
        }
        newDir <- paste0("Edited",sprintf("%02d", (lastOne+1)))
        newDir <- file.path(dataDir,newDir)
      }else
      {
        newDir <- file.path(dataDir,"Edited00")
      }
      dir.create(newDir)
      
      #Write Y file
      yFile <- file.path(newDir,input$files$name)
      write.table(Ys_df,file=yFile,col.names = T, row.names = F,
                  sep="\t",dec=".")
      
      #Write X files
      for (unFichier in ALLXDataList){
        xFile <- file.path(newDir,paste0(unFichier,".txt"))
        dats <- All_XData[[unFichier]]
        write.table(dats, file=xFile,
                    row.names = F, col.names = F,
                    sep="\t", dec=".")
      }
      
    })
    # *********************************************************************
    
    
    ## Reacts to restoreOriData button ----
    observeEvent(input$restoreOriData, {
        Ys_df <<- ORI_Ys_df
        #Table of Ys
        dtable <- DT::datatable(Ys_df, width = '900px',
                                options=list(
                                  autoWidth=FALSE,
                                  lengthMenu = list(c(10, 15, 20, -1), c('10', '15', '20','All')),
                                  pageLength = 10,
                                  scrollX = TRUE,
                                  columnDefs = list(
                                    list(orderable = TRUE, targets = 0),
                                    list(width = '15px', targets = 0),
                                    list(width = '75px', targets = 1:(ncol(Ys_df)-1)),
                                    list(className = "dt-center", targets = "_all")
                                  )
                                ),
                                filter='top')
        dtable$x$data[[1]] <- as.numeric(dtable$x$data[[1]])-1
        #Force name of column for sample ID to ID
        colnames(dtable$x$data)[2] <- "ID"  #by default row number (0 based) on
        # first column
        Yvalues$dfWorking <- dtable
        
        
        All_XData <<- ORI_XData
        for (k in 1:length(All_XData)){
            leNom <- names(All_XData)[k]
            #Truncation
            wv <- All_XData[[leNom]][1,-1]
            i1 <- which(All_XData[[leNom]][1,-1]==RayleighCutoffs[leNom])
            i2 <- which(All_XData[[leNom]][1,-1]==max(wv))
            if (!is.null(XData_p[[leNom]])) XData_p[[leNom]] <<- All_XData[[leNom]][,1+c(0,i1:i2)]
        }
        
        
        lesChoix <- computePCAonRaw(as.numeric(input$npcs),doRayleigh=FALSE)
        proxy_Ys %>% selectRows(1)  #Trigger PCA plot refresh
        proxy_Ys %>% selectRows(NULL)
        
        #Populate select CPs to plot
        updateSelectInput(session,"pc1",
                          choices = lesChoix,
                          selected="SDist")
        updateSelectInput(session,"pc2",
                          choices = lesChoix,
                          selected="ODist")
        
    })
    
    # *********************************************************************
    
    ## Reacts to click on pcaPlot ----
    observeEvent(event_data("plotly_click",source ="pcaPlot"),
                 {
                       d <- event_data("plotly_click",source ="pcaPlot")
                       lePCA <- isolate(input$PCA_data)
                       dfsPCA <- as.data.frame(PCAsDT[[lePCA]]$x)
                       xall <- isolate(as.numeric(dfsPCA[input$pc1][,1]))
                       yall <-isolate( as.numeric(dfsPCA[input$pc2][,1]))
                       choisie <- input$Ys_rows_selected
                       laRow <- (myNearPoint(d$x,d$y,xall,yall))
                       proxy_Ys %>% selectRows(c(choisie,laRow))
                 },
                 ignoreInit = T
                 )
    
    # *********************************************************************
    
    ## Reacts to selection on pcaPlot ----
    observeEvent(event_data("plotly_selected",source ="pcaPlot"),
                 {
                     d <- event_data("plotly_selected",source ="pcaPlot")
                     # d$x and d$y : corners of selected box
                     # so find all points in a box
                     lePCA <- isolate(input$PCA_data)
                     dfsPCA <- as.data.frame(PCAsDT[[lePCA]]$x)
                     xall <- isolate(as.numeric(dfsPCA[input$pc1][,1]))
                     yall <-isolate( as.numeric(dfsPCA[input$pc2][,1]))
                     sels <- as.data.frame(cbind(d$x,d$y))
                     refs <- as.data.frame(cbind(xall,yall))
                     lesRows <- prodlim::row.match(sels,refs)
                     proxy_Ys %>% selectRows(lesRows)
                 },
                 ignoreInit = T
                )
    
    # *********************************************************************
    
    # On PrePro tab ----
    
    ## Reacts to Trunc_limits editing ----
    observeEvent(input$PreProsTrunc_cell_edit,{
      row  <- input$PreProsTrunc_cell_edit$row
      clmn <- input$PreProsTrunc_cell_edit$col+1
      laVal <- as.numeric(input$PreProsTrunc_cell_edit$value)
      if (clmn<4) {
        # First make sure it is within limits of the raw spectra
        # wvLimits <- range(XData_p[[row]][1,-1])  #RayleighCutoffs
        wvLimits <- range(All_XData[[XDataList[[row]]]][1,-1])    #Raw wavelenght range
        if (!between(laVal,wvLimits[1],wvLimits[2])){ #not in limit
          laVal <- PPvaluesTrunc$dfWorking$x$data[row, clmn]
        }
  
        # Check waveband norm. parameters and update
        PPvaluesTrunc$dfWorking$x$data[row, clmn] <- laVal
        lowWL <- PPvaluesTrunc$dfWorking$x$data[row, 2]
        hiWL <- PPvaluesTrunc$dfWorking$x$data[row, 3]
        id <- paste0(inserted_perSpectrumOptions()[row],"_C")
        WBand <- input[[id]]
        ctr <- round((hiWL+lowWL)/2)
        #Update values for ctr and waveband
        id <- paste0(inserted_perSpectrumOptions()[row],"_B")
        updateNumericInput(session,id,value=ctr,min=lowWL+WBand,max=hiWL-WBand)
      }else
      {
        PPvaluesTrunc$dfWorking$x$data[row, clmn] <- laVal
        RayleighCutoffs[[inserted_perSpectrumOptions()[row]]] <<- laVal
        #Make sur lower limit is greater than Rayleigh
        lolim <- PPvaluesTrunc$dfWorking$x$data[row, 2]
        if (lolim<laVal){ 
          PPvaluesTrunc$dfWorking$x$data[row, 2] <- laVal
        }
      }
      output$PreProsTrunc <- renderDataTable({
        PPvaluesTrunc$dfWorking
      })

    })
    
    # *********************************************************************
    
    ## Reacts to Apply on PrePro tab ----
    
    observeEvent(input$applyPrePro,{
      s <- unique(input$Ys_rows_selected)
      preproParams <- collectPreProParams(PPvaluesTrunc,input)
      Apply_PrePro(preproParams,input)
      #Force redraw
      proxy_Ys %>% selectRows(NULL)
      proxy_Ys %>% selectRows(s)
    })
    
    # *********************************************************************
    
    ## Check inputs and refresh per spectrum and SavGol inputs ----
    observe({
      outtxt <- character()
      for (k in inserted_perSpectrumOptions()){
        ctr <- input[[paste(k,"B",sep="_")]]
        req(ctr, cancelOutput = TRUE)
        # Check to set centre within limits
        # centre is "_B", waveband is "_C"
        isolate(wb <-  input[[paste(k,"C",sep="_")]])
        lo <- PPvaluesTrunc$dfWorking$x$data[k,2]
        hi <- PPvaluesTrunc$dfWorking$x$data[k,3]
        wb2 <- floor(wb/2)
        loctr <- lo+wb2
        hictr <- hi-wb2
        updateNumericInput(session,paste(k,"B",sep="_"),min=loctr,max=hictr)
        outtxt <- paste0(outtxt,k,": ")
        for (j in LETTERS[1:7])
          outtxt <- paste(outtxt,
                          input[[paste(k,j,sep="_")]])
        outtxt <- paste0(outtxt,"\n")
        
        if (ctr<loctr) updateNumericInput(session,paste(k,"B",sep="_"),value=loctr)
        if (ctr>hictr) updateNumericInput(session,paste(k,"B",sep="_"),value=hictr)
        
      }
      output$feedback<-renderText(outtxt)
    })
    
    # *********************************************************************
    
    ## Reacts to save prepro button ----
    observe({
      volumes <- c("UserFolder"=projectDir)
      shinyFileSave(input, "FSavePrePro", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$FSavePrePro)
      if (nrow(fileinfo) > 0) {
        isolate({
          leFichier <- fileinfo$datapath
          PP_params <- collectPreProParams(PPvaluesTrunc,input)
          PP_params <- stripPreProNames(PP_params)
          #For compatibility with InSpectoR and PolySpecteur, saves
          #parameters in InSpectoR format. 
          dum <- prepro_Shin_2_InSp(PP_params)
          model_descript <- dum$model_descript
          prepro_params <- dum$prepro_params
          save(list=c('model_descript','prepro_params'),file=leFichier)
        })
      }
    })
    
    # *********************************************************************
    
    ## Reacts to load prepro button ----
    observe({
      volumes <- c("UserFolder"=projectDir)
      shinyFileChoose(input, "FLoadPrePro", roots=volumes, session=session)
      fileinfo <- parseFilePaths(volumes, input$FLoadPrePro)
      if (nrow(fileinfo) > 0){
        leFichier <- fileinfo$datapath
        dum <- load(file=leFichier)   #in dum (character vector ), PP_params is the name of the list for ShInSpectoR
                                      # For InSpectoR dum = c("model_descript","prepro_params")
        # check if from InSpectoR
        if (dum[1]=="model_descript"){  #TRUE=InSpectoR, build PP_params
          PP_params <- list()
          PP_params$lesNoms <- model_descript$datatype
          i <- 0
          for (id in PP_params$lesNoms){
            i <- i+1
            PP_params$trunc_limits$lo <- prepro_params$trunc_limits[,1]
            PP_params$trunc_limits$hi <- prepro_params$trunc_limits[,2]
            PP_params$perSpecParams[[id]][1] <- 
                 c("none", "waveband", "closure")[prepro_params$byspectra_scaling_index[i]]
            PP_params$perSpecParams[[id]][2] <- as.character(prepro_params$cntr_n_w[i,1])
            PP_params$perSpecParams[[id]][3] <- as.character(prepro_params$cntr_n_w[i,2])
            PP_params$savgolParams$doSavGol[[id]] <- prepro_params$do_savgol[i]
            PP_params$savgolParams$w[[id]] <- prepro_params$w[i]
            PP_params$savgolParams$p[[id]] <- prepro_params$p[i]
            PP_params$savgolParams$m[[id]] <- prepro_params$m[i]
          }
          PP_params$trunc_limits <- data.frame(PP_params$trunc_limits)
          
        }
        
        PP_params <- buildPreProNames(PP_params)
        #Check spectrum types
        if ((length(XDataList) != length(PP_params$lesNoms)) || 
             !all(XDataList == PP_params$lesNoms)){   #spectrum types do not match
          showModal(modalDialog(
            title = "WARNING",
            "Selected spectrum types do not match!"
          ))
        }else
        {
          #Load truncation limits
          isolate(PPvaluesTrunc$dfWorking$x$data[,2:3] <- PP_params$trunc_limits[,1:2])
          
          #update per spectrum and SavGol options
          isolate({
            for (k in inserted_perSpectrumOptions()){
              for (id in PP_params$lesNoms){
                updateRadioButtons(session, paste0(id,"_A"), selected = PP_params$perSpecParams[[id]][1]) 
                updateNumericInput(session, paste0(id,"_B"), value = PP_params$perSpecParams[[id]][2])
                updateNumericInput(session, paste0(id,"_C"), value = PP_params$perSpecParams[[id]][3])
                updateCheckboxInput(session, paste0(id,"_D"), value = PP_params$savgolParams$doSavGol[[id]])
                updateNumericInput(session, paste0(id,'_E'), value = PP_params$savgolParams$w[[id]])
                updateNumericInput(session, paste0(id,'_F'), value = PP_params$savgolParams$p[[id]])
                updateNumericInput(session, paste0(id,'_G'), value = PP_params$savgolParams$m[[id]])
              }
            }
          })
        }
      }
    })
    
    # *********************************************************************
   
    # PLS tab ----
    ## Reacts to PLS tab activation ----
    observeEvent(input$tabs,{
      if(input$tabs == "PLS"){ #set up the pages
        isolate({
           updateSelectInput(session,"XsForPLS", choices=input$Xs,
                             selected=input$Xs[1])
          lesChoix <- names(Filter(function(x) !is.factor(x),Ys_df))
          updateSelectInput(session,"YForPLS",
                            choices = lesChoix ,
                            selected = lesChoix[1])
          nSamples <- nrow(Ys_df)
          updateSelectInput(session,"NbLVForPLS",
                            choices = 1:min(c((nSamples-1),20)))
          lesChoix <- names(Filter(function(x) is.factor(x),Ys_df))
          updateSelectInput(session,"PLSPredPlotColorBy",
                            choices = lesChoix[-1])
          
          updateSelectInput(session,"PLSPredPlotLabel",
                            choices = lesChoix ,
                            selected = lesChoix[1])
          updateSelectInput(session,"PLSScorePlotColorBy",
                            choices = lesChoix[-1])
          
          
          #Perform pre-processing so XData_p is inline with prepro options
          #Useful in case user did not Apply prepros
          preproParams <- collectPreProParams(PPvaluesTrunc,input)
          Apply_PrePro(preproParams,input)
          
          })
        output$PLSPlotID <-   renderText("PLOT TYPE ID")
        output$PLSPlots <- renderPlotly({
          text = "PLOT OUTPUT AREA"
          ggplotly(
            ggplot() + 
              annotate("text", x = 4, y = 25, size=8, label = text) + 
              theme_void()
          )
          
        })
        whichPLSPlot <<- "Error"
        output$PLSConsole <- renderPrint({
          dum <- "Console output area"
          write(dum, file="")
        })
      }
      
      
      
      shinyjs::hide("PLSDescript")
      shinyjs::hide("FSavePLS")
      
    })
    
    # *********************************************************************
    
    ## Verify in nb LV for showing results is within limits ----
    observeEvent(input$NbLVForPLS,{
      nbLVs <- as.numeric(input$NbLVForPLS)
      if (!is.null(nbLVs) & !is.na(nbLVs)){
        # nbSamples <- nrow(Ys_df)
        # maxi <- min(c(20,(nbSamples-1)))
        updateSelectInput(session,'PLSScorePlotFirstLV',
                            choices = 1:nbLVs)
        updateSelectInput(session,'PLSScorePlotSecondLV',
                            choices = 1:nbLVs)
        updateSelectInput(session,'NbLVPLS_Sel',
                          choices = 1:nbLVs)
      }
      
    })
    
    # *********************************************************************
    
    ## Verify in nb LV for plotting scores is within limits ----
    observeEvent(input$NbLVPLS_Sel,{
      nbLVs <- as.numeric(input$NbLVPLS_Sel)
      if (!is.null(nbLVs) & !is.na(nbLVs)){
        maxi <- as.numeric(input$NbLVForPLS)
        updateSelectInput(session,'PLSScorePlotFirstLV',
                          choices = 1:nbLVs)
        updateSelectInput(session,'PLSScorePlotSecondLV',
                          choices = 1:nbLVs)
      }
    })
    
    # *********************************************************************
    
    ##Reacts to Compute button ----
    observeEvent(input$ComputePLS,{
      showNotification("Computing - Be patient!")
      
      output$PLSPlots <- renderPlotly({
        text = "PLOT OUTPUT AREA"
        ggplotly(
          ggplot() + 
            annotate("text", x = 4, y = 25, size=8, label = text) + 
            theme_void()
        )
      })
      
      output$PLSConsole <- renderPrint({
        dum <- "Console output area"
        write(dum, file="")
      })
      
        
      #collect parameters
      YName <- input$YForPLS
      nbLV <- as.numeric(input$NbLVForPLS)
      aggr <- input$AggregateForPLS
      Y <- Ys_df[[YName]]
      XNames <- as.list(sort(input$XsForPLS))
      valid <- input$ResamplingForPLS
      
      if (aggr=="concatenate spectra"){
        y <- data.frame(V1=Ys_df[[YName]])
        for (k in XNames){
          spdf<-as.data.frame(XData_p[[k]][-1,-1])
          pre <- getShortType(k)
          # pre <- strsplit(k,"_")[[1]][1]
          colnames(spdf)<-paste(pre,as.character(XData_p[[k]][1,-1]),sep="_")
          y <- cbind(y,spdf)
        }
        pls_set <<- list(y)
      }
      plsFit <<- lapply(pls_set,function(x){
        pls::plsr(formula= V1~., data=x,
                  ncomp=nbLV,
                  validation = valid)
      })
      dum<-c()
      pls_txt_output<-lapply(plsFit,function(x){
       utils::capture.output(summary(x))
      })
      output$PLSConsole <- renderPrint({
        write(unlist(pls_txt_output),file="")
      })
      
      #Plot
      output$PLSPlotID <-   renderText("ERROR PLOT")
      output$PLSPlots <- renderPlotly({
        dats = pls::RMSEP(plsFit[[1]], estimate="all")
        df=as.data.frame(t(dats$val[,1,]))
        df$x=seq_len(dim(df)[1])-1
        plot_data <- reshape2::melt(df,id.var="x")
        ggplotly(
          ggplot2::ggplot(plot_data, ggplot2::aes(x=x,y=value,group=variable,colour=variable))+
            ggplot2::geom_point(size=4, shape=21, stroke=1.2)+
            ggplot2::geom_line(ggplot2::aes(lty=variable),lwd=1.2)+
            ggplot2::xlab("Nb of latent variables") + ggplot2::ylab("RMSE")
        )
      })
      shinyjs::show("PLSDescript")    
      shinyjs::show("FSavePLS")

      
    }, ignoreInit = T)
    
    # *********************************************************************
    
    ## Reacts to save PLS model button ----
    observe({
      volumes <- c("UserFolder"=projectDir)
      shinyFileSave(input, "FSavePLS", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$FSavePLS)
      if (nrow(fileinfo) > 0) {
        isolate({
          leFichier <- fileinfo$datapath
          PP_params <- collectPreProParams(PPvaluesTrunc,input)
          
          #Need to remove some as not all XDataList members are in XsForPLS
          toRemove <- setdiff(PP_params$lesNoms,sort(input$XsForPLS))
          if (length(toRemove)>0){
            removeInd <- unlist(lapply(toRemove, function(x) which(x==PP_params$lesNoms)))
            PP_params$lesNoms <- as.list(sort(input$XsForPLS))
            PP_params$trunc_limits <- PP_params$trunc_limits[-removeInd,]
            lapply(toRemove, function(id){
              PP_params$perSpecParams[[id]] <<- NULL
              PP_params$savgolParams$doSavGol[[id]] <<- NULL
              PP_params$savgolParams$w[[id]] <<- NULL
              PP_params$savgolParams$p[[id]] <<- NULL
              PP_params$savgolParams$m[[id]] <<- NULL
            })
          }
          nom_lesX <- sort(input$XsForPLS)
          shortNames <- getShortType(nom_lesX)
         
          model_descript <- list(
            type = "PLS",
            description = input$PLSDescript,
            datatype = shortNames,
            aggregation = input$AggregateForPLS
          )
          pls_ncomp <- as.numeric(input$NbLVPLS_Sel)
          PP_params <- stripPreProNames(PP_params)
          colorby <- input$PLSPredPlotColorBy
          #COnvert to InSpectoR format
          dum <- prepro_Shin_2_InSp(PP_params)
          prepro_params <- dum$prepro_params
          save(model_descript,prepro_params,plsFit,pls_ncomp,colorby,file=leFichier) 
        })
      }
    })
    
    # *********************************************************************
    
    ## Reacts to plot validation button ----
    observeEvent(input$PLSvalidationPlot,{
      whichPLSPlot <<- "Error"
      output$PLSPlotID <-   renderText("ERROR PLOT")
      output$PLSPlots <- renderPlotly({
        dats = pls::RMSEP(plsFit[[1]], estimate="all")
        df=as.data.frame(t(dats$val[,1,]))
        df$x=seq_len(dim(df)[1])-1
        plot_data <- reshape2::melt(df,id.var="x")
        ggplotly(
          ggplot2::ggplot(plot_data, ggplot2::aes(x=x,y=value,group=variable,colour=variable))+
            ggplot2::geom_point(size=4, shape=21, stroke=1.2)+
            ggplot2::geom_line(ggplot2::aes(lty=variable),lwd=1.2)+
            ggplot2::xlab("Nb of latent variables") + ggplot2::ylab("RMSE")
        )
      })
    })
    
    # *********************************************************************
    
    ## Reacts to plot predictions ----
    #Retrieve options (factor to apply color or to label)
    observeEvent(input$PlotPLSPred,
                 whichPLSPlot <<- "Pred"
    )
    observe({
      input$PlotPLSPred
      input$NbLVPLS_Sel
      input$PredPlotTypePLS
      colBy <- input$PLSPredPlotColorBy
      labelWith <- input$PLSPredPlotLabel
      if (whichPLSPlot=="Pred")
        isolate({
          nc=as.numeric(input$NbLVPLS_Sel)
          nc <- min(nc,as.numeric(input$NbLVForPLS))
          leType <- input$PredPlotTypePLS
          output$PLSPlotID <-   renderText(paste0("PREDICTION - ",leType))
          pl<-plot(plsFit[[1]],plottype = "prediction",
                             ncomp=nc,
                             which=leType, type="n")
          dev.off(dev.list()["RStudioGD"])
          
          pl <- as.data.frame(pl)
          
          mycolors <- colorRampPalette(mesCouleurs)(length(mesCouleurs))
          
          output$PLSPlots <-   renderPlotly({
            nc=as.numeric(input$NbLVPLS_Sel)
            plotly::plot_ly() %>%
            add_markers(data=pl,          #Plot all points
                        x=as.formula("~measured"),
                        y=as.formula("~predicted"),
                        type = "scatter", mode = "markers",
                        color=as.character(Ys_df[[colBy]]),
                        colors = mycolors,
                        size=8,
                        text=as.character(Ys_df[[labelWith]]),
                        hovertext=as.character(Ys_df[,1]),
                        hovertemplate = paste('EchID: %{text}'))
          })
        })
      
    })
    
    # *********************************************************************
    observeEvent(input$PLSBCoeffPlot,
                 whichPLSPlot <<- "BCoef"
    )
    ## Reacts to B-coeff plot----
    observe({
      input$PLSBCoeffPlot
      input$NbLVPLS_Sel
      if (whichPLSPlot=="BCoef")
        isolate({
          output$PLSPlotID <-   renderText("B - Coefficients")
          output$PLSPlots <- renderPlotly({
          
            x_id=rownames(plsFit[[1]]$coefficients)
            x_id=strsplit(x_id,"_")
            N_xlabels <- length(x_id)
            x_id=unlist(x_id)
            x_cls=x_id[seq(1,2*N_xlabels,2)]
            xlabels=as.numeric(x_id[seq(2,2*N_xlabels,2)])
            nc=as.numeric(input$NbLVPLS_Sel)
            nc <- min(nc,as.numeric(input$NbLVForPLS))
            df=data.frame(y=plsFit[[1]]$coefficients[,1,nc],
                          Cl=x_cls, 
                          xlabs=as.numeric(xlabels))
            
            ggplotly(
              ggplot2::ggplot(df, ggplot2::aes(x=xlabs,y=y,colour=Cl))+
              ggplot2::geom_line() + ggplot2::xlab("Wavelength or Wavenumber") + ggplot2::ylab("B-coefficients") +
              ggplot2::facet_wrap( ~ Cl, ncol=1, scales="free_y") + ggplot2::theme(legend.position="none") +
              ggplot2::theme(strip.background = ggplot2::element_rect(fill="grey80"))
            )
          })
        })
    })
    
    # *********************************************************************
    
    ## Reacts to Score plot button----
    observeEvent(input$PLSScorePlot ,
                 whichPLSPlot <<- "Score"
     )
    
    observe({
      input$PLSScorePlot
      input$NbLVPLS_Sel
      input$PLSScorePlotSecondLV
      input$PLSScorePlotFirstLV
      if (whichPLSPlot=="Score")
        isolate({
          output$PLSPlotID <- renderText("Score plot")
          dats <- scores(plsFit[[1]])
          pl <- data.frame(x=dats[,as.numeric(input$PLSScorePlotFirstLV)]
                           , y=dats[,as.numeric(input$PLSScorePlotSecondLV)])
          colnames(pl) <- c(paste0('LV',input$PLSScorePlotFirstLV),
                            paste0('LV',input$PLSScorePlotSecondLV))
          mycolors <- colorRampPalette(mesCouleurs)(length(mesCouleurs))
    
          output$PLSPlots <-   renderPlotly({
            colBy <- input$PLSScorePlotColorBy
            plotly::plot_ly() %>%
              add_markers(data=pl,          #Plot all points
                          x=as.formula(paste0("~",colnames(pl)[1])),
                          y=as.formula(paste0("~",colnames(pl)[2])),
                          type = "scatter", mode = "markers",
                          color=as.character(Ys_df[[colBy]]),
                          colors = mycolors,
                          size=8,
                          text=as.character(Ys_df[,1]),
                          hovertext=as.character(Ys_df[,1]),
                          hovertemplate = paste('EchID: %{text}'))
          })
        })
    })
    
    # *********************************************************************
    
    ## Reacts to ShowPLSPredTable button ----
    observeEvent(input$ShowPLSPredTable,{
    
      pl<-plot(plsFit[[1]],plottype = "prediction",
               ncomp=as.numeric(input$NbLVPLS_Sel),
               which="train")
      dev.off(dev.list()["RStudioGD"])
      dat_tbl=as.data.frame(pl)
      pl<-plot(plsFit[[1]],plottype = "prediction",
               ncomp=as.numeric(input$NbLVPLS_Sel),
               which="validation")
      dat_tbl=cbind(dat_tbl,as.data.frame(pl)[,2])
      dat_tbl=cbind(Ys_df[,1],dat_tbl,NoSeq=seq_len(nrow(dat_tbl)))
      colnames(dat_tbl)[c(1,3,4)]=c(colnames(Ys_df)[1],
                                    "Training","Validation")
      output$PlsPredTable = renderDataTable( dat_tbl)
    })
    
    # *********************************************************************
    
    ## Reacts to Save button on modal PlsPredTable ----
    observeEvent(input$savePLSPreds,{
      leFichier <- utils::choose.files(paste0(projectDir,"\\*.*"),
                                       multi = F, filters = Filters[c("txt"),])
      if (length(leFichier) > 0) {
        isolate({
          pl<-plot(plsFit[[1]],plottype = "prediction",
                   ncomp=as.numeric(input$NbLVPLS_Sel),
                   which="train")
          dev.off(dev.list()["RStudioGD"])
          dat_tbl=as.data.frame(pl)
          pl<-plot(plsFit[[1]],plottype = "prediction",
                   ncomp=as.numeric(input$NbLVPLS_Sel),
                   which="validation")
          dat_tbl=cbind(dat_tbl,as.data.frame(pl)[,2])
          dat_tbl=cbind(Ys_df,dat_tbl,NoSeq=seq_len(nrow(dat_tbl)))
          colnames(dat_tbl)[c(1,3,4)]=c(colnames(Ys_df)[1],
                                        "Training","Validation")
          
          utils::write.table(dat_tbl,
                             file=leFichier,
                             sep="\t",
                             dec=".",
                             row.names = FALSE,
                             quote=FALSE)
      })
    }
  })
  # PCA tab ----
    
    # *********************************************************************
    
    ## Reacts to PCA tab activation ----
    observeEvent(input$tabs,{
      if(input$tabs == "PCA"){ #set up the pages
        isolate({
          updateSelectInput(session,"XsforPCA", choices=input$Xs,
                            selected=input$Xs[1])
          nSamples <- nrow(Ys_df)
          lesChoix <- names(Filter(function(x) is.factor(x),Ys_df))[-1]
          updateSelectInput(session,"PCAPlotColorBy",
                            choices = lesChoix ,
                            selected = lesChoix[1])
          
          
          #Perform pre-processing so XData_p is inline with prepro options
          #Useful in case user did not Apply prepros
          preproParams <- collectPreProParams(PPvaluesTrunc,input)
          Apply_PrePro(preproParams,input)
          
          #Computes PCA on first spectrum in input$X to start with
          dum <- XData_p[[input$Xs[1]]]
          pre <- getShortType(input$Xs[1])
          colnames(dum)[-1] <- paste(pre,as.character(dum[1,-1]),sep="_")
          doPCA(dum)
          
          updateSelectInput(session, 'PCATopPlotType',
                              selected = 'Screeplot')
          updateSelectInput(session, "NPCsforPCA", choices=2:lePCA_NCPs)
          updateSelectInput(session, "XAxisPCAPlot", 
                            choices = paste0("PC",(1:lePCA_NCPs)))
          updateSelectInput(session, "YAxisPCAPlot",
                            choices = paste0("PC",(1:lePCA_NCPs)),
                            selected = "PC2")
          
        })
        #trigger plots
        input$XAxisPCAPlot
      }
      
    })
    
    # *********************************************************************
    ## Plot PCA ----
    output$PCATopPlot <- renderPlotly({
      req(input$XsforPCA, input$pcaPtColorBy)
      mycolors <- colorRampPalette(mesCouleurs)(length(mesCouleurs))
      
      lepc1 <- as.formula(paste0("~",input$XAxisPCAPlot))
      lepc2 <- as.formula(paste0("~",input$YAxisPCAPlot))
      ptColor <- as.formula(paste0("~",input$PCAPlotColorBy))
      dfsPCA <- as.data.frame(lePCA$x)
      dfsPCA <- cbind(dfsPCA,Ys_df[input$PCAPlotColorBy])
      nColor <- length(unique(dfsPCA[[input$PCAPlotColorBy]]))
      suppressWarnings(colIndices <- (1:10 + rep(0,nColor))[1:nColor])
     
      
      switch(input$PCATopPlotType,
             Scores = {
                   plotly::plot_ly() %>%
                   add_markers(data=dfsPCA,          #Plot all points
                               x=lepc1, y=lepc2,
                               type = "scatter", mode = "markers",
                               color=ptColor,
                               colors = mycolors[colIndices],
                               size=8,
                               text=I(as.character(Ys_df[[1]])),
                               hovertemplate = paste('EchID: %{text}'),
                               )
             },
             Loadings = {
               xs <- rownames(lePCA$rotation)
               x_id=strsplit(xs,"_")
               N_xlabels <- length(x_id)
               x_id=unlist(x_id)
               x_cls=x_id[seq(1,2*N_xlabels,2)]
               xlabels=as.numeric(x_id[seq(2,2*N_xlabels,2)])
               colonnes <- colnames(lePCA$rotation)
               col1 <- which(input$XAxisPCAPlot == colonnes)
               col2 <- which(input$YAxisPCAPlot == colonnes)
               df=data.frame(y=lePCA$rotation[,col1],
                             Cl=x_cls, 
                             xlabs=as.numeric(xlabels),
                             PC = paste0('PC',col1))
               if (col2>col1) for (k in (col1+1):col2){
                 df=rbind(df,data.frame(y=lePCA$rotation[,k],
                               Cl=x_cls, 
                               xlabs=as.numeric(xlabels),
                               PC=paste0("PC",k)))
               }
               ncols <- col2-col1+1
               
               ggplotly(
                 ggplot2::ggplot(df, ggplot2::aes(x=xlabs,y=y,colour=Cl))+
                   ggplot2::geom_line() + ggplot2::xlab("Wavelength or Wavenumber") + ggplot2::ylab("Loadings") +
                   ggplot2::facet_grid(PC ~ Cl, scales="free_y") + ggplot2::theme(legend.position="none") +
                   ggplot2::theme(strip.background = ggplot2::element_rect(fill="grey80"))
               )
             },
             Screeplot = {
               df <- data.frame(PC=1:lePCA_NCPs,
                                VarExp=PCA_var_explained[1:lePCA_NCPs])
               ggplotly(
                 ggplot2::ggplot(data=df, ggplot2::aes(x=PC, y=VarExp)) +
                   ggplot2::geom_line(color="blue")+
                   ggplot2::ylab("Fraction of variance explained")+
                   ggplot2::xlab("Principal component")
               )

             },
             OD_SD = {
               pca2<-pr_2_prin(lePCA)
               dd <- chemometrics::pcaDiagplot(dat_4_PCA,pca2,
                                               a=as.numeric(input$NPCsforPCA),
                                               plot=FALSE,scale=FALSE)
               df <- data.frame(Score=dd$SDist,Outside=dd$ODist)
               df <- cbind(df,Ys_df[input$PCAPlotColorBy])
               plotly::plot_ly() %>%
                 add_markers(data=df,          #Plot all points
                             x=as.formula(" ~ Score"),
                             y=as.formula(" ~ Outside"),
                             type = "scatter", mode = "markers",
                             color=ptColor,
                             colors = mycolors[colIndices],
                             size=8,
                             text=I(as.character(Ys_df[[1]])) ,
                             hovertemplate = paste('EchID: %{text}'),
                 ) %>%
                 layout(xaxis = list(title = list(text='In model distance (scores)',
                                                  font=list(size=20))), 
                        yaxis = list(title = list(text='Outside model distance',
                                                  font=list(size=20)))
                 )

             }
       )
      
    })
    
    # *********************************************************************
    ## Reacts to XsforPCA ----
    observe({
      req(input$XsforPCA)
      y <- NULL
      for (k in input$XsforPCA){
        spdf<-as.data.frame(XData_p[[k]][,-1])
        pre <- getShortType(k)
        colnames(spdf)<-paste(pre,as.character(XData_p[[k]][1,-1]),sep="_")
        if (is.null(y)){
          y <- spdf
        }else
        {
          y <- cbind(y,spdf)
        }
      }
      y <- cbind(data.frame(ID=c("ID",Ys_df[[1]])),y)
      doPCA(y)
      updateSelectInput(session, input$NPCsforPCA, choices=1:lePCA_NCPs)
      updateSelectInput(session, input$XAxisPCAPlot, 
                        choices = as.character(1:lePCA_NCPs))
      updateSelectInput(session, input$YAxisPCAPlot,
                        choices=as.character(1:lePCA_NCPs))
      #trigger plots
      input$XAxisPCAPlot
    })
    
    # *********************************************************************
    ## Reacts to Save scores ----
    observe({
      volumes <- c("UserFolder"=projectDir)
      shinyFileSave(input, "PCAScoresSave", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$PCAScoresSave)
      if (nrow(fileinfo) > 0) {
        isolate({
          scs <- lePCA$x[,1:input$NPCsforPCA]
          eigVals <- lePCA$sdev[1:input$NPCsforPCA] 
          midMat <- diag(1/eigVals^2)
          mySD <- sqrt(diag(scs %*% midMat %*% t(scs)))
          
          x <- as.matrix(dat_4_PCA-matrix(lePCA$center,
                                           nrow=nrow(dat_4_PCA),
                                           ncol=ncol(dat_4_PCA),
                                           byrow = T))
          lds <- lePCA$rotation[,1:input$NPCsforPCA]
          midMat <- diag(nrow=dim(x)[2]) - lds %*% t(lds)
          myOD <- sqrt(diag(x %*% midMat %*% t(x)))
          
          scores<-data.frame(ID=Ys_df[,1],lePCA$x[,1:input$NPCsforPCA],
                             SD=mySD, OD=myOD)
          # row.names(scores)=Ys_df[,1]  #Put sample IDs as row names.
          #Define column names
          shortNames <- lapply(strsplit(sort(input$XsforPCA),"_"), function(x) x[1])
          pre <- paste(unlist(shortNames),collapse="_")
          colonnes <- paste(pre,paste0("PC",1:input$NPCsforPCA),sep="_")
          colnames(scores)[1+1:as.numeric(input$NPCsforPCA)] <- colonnes
          utils::write.table(scores,file=fileinfo$datapath,sep="\t",
                             row.names=FALSE)
        })
      }
    })
    
    
    
    # *********************************************************************
    ## Reacts to Save model ----
    observe({
      volumes <- c("UserFolder"=projectDir)
      shinyFileSave(input, "PCAModelSave", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$PCAModelSave)
      if (nrow(fileinfo) > 0){
        isolate({
          nom_lesX <- sort(input$XsforPCA)
         
          shortNames <- getShortType(nom_lesX)
          model_descript=list(type="PCA",
                              source="ShInSpectoR",
                              description=input$PCADescript,
                              datatype=shortNames)
          colorby<-Ys_df[,input$PCAPlotColorBy]
          #Add distances to model output
          #pr_2_prin is for converting prcomp output to princomp output
          pca2<-pr_2_prin(lePCA)
          #Compute score and orthogonal distances
          dds <- chemometrics::pcaDiagplot(dat_4_PCA,
                                      pca2,a=as.numeric(input$NPCsforPCA),
                                      plot=FALSE,scale=FALSE)
            
          PP_params <- collectPreProParams(PPvaluesTrunc,input)
          
          toRemove <- setdiff(PP_params$lesNoms,sort(input$XsforPCA))
          if (length(toRemove)>0){
            removeInd <- unlist(lapply(toRemove, function(x) which(x==PP_params$lesNoms)))
            PP_params$lesNoms <- as.list(sort(input$XsforPCA))
            PP_params$trunc_limits <- PP_params$trunc_limits[-removeInd,]
            lapply(toRemove, function(id){
              PP_params$perSpecParams[[id]] <<- NULL
              PP_params$savgolParams$doSavGol[[id]] <<- NULL
              PP_params$savgolParams$w[[id]] <<- NULL
              PP_params$savgolParams$p[[id]] <<- NULL
              PP_params$savgolParams$m[[id]] <<- NULL
            })
          }
          NCPs <- input$NPCsforPCA
          PP_params <- stripPreProNames(PP_params)
          
          #COnvert to InSpectoR format
          dum <- prepro_Shin_2_InSp(PP_params)
          prepro_params <- dum$prepro_params
          
          save(model_descript,prepro_params,lePCA,NCPs,dds,colorby,
               file=fileinfo$datapath)
        })
      }
    })
    
    # *********************************************************************
    
    # PLSDA tab ----
    ## Reacts to PLSDA tab activation ----
    observeEvent(input$tabs,{
      if (input$tabs == "PLSDA"){ #set up the pages
        isolate({
          updateSelectInput(session,"XsForPLSDA", choices=input$Xs,
                            selected=input$Xs[1])
          lesChoix <- names(Filter(function(x) is.factor(x),Ys_df))[-1]
          updateSelectInput(session,"YForPLSDA",
                            choices = lesChoix ,
                            selected = lesChoix[1])
          nSamples <- nrow(Ys_df)
          leMax <- min(c((nSamples-1),25))
          updateSelectInput(session,"NbLVForPLSDA",choices=1:leMax)
          updateSelectInput(session,"PLSPredPlotColorBy",
                            choices = lesChoix[-1])
          updateSelectInput(session,input$ResamplingForPLSDA, selected = "cv")
          
          #Perform pre-processing so XData_p is inline with prepro options
          #Useful in case user did not Apply prepros
          preproParams <- collectPreProParams(PPvaluesTrunc,input)
          Apply_PrePro(preproParams,input)
        })
      }
    
    output$PLSDAPlots <- renderPlotly({
      text = "PLOT OUTPUT AREA"
      ggplotly(
        ggplot() + 
          annotate("text", x = 4, y = 25, size=8, label = text) + 
          theme_void()
      )
    })
    
    output$PLSDaAConsole <- renderPrint({
      dum <- "Console output area"
      write(dum, file="")
    })  
      
    shinyjs::hide("PLSDADescript")
    shinyjs::hide("FSavePLSDA")
    
    })
   
    # *********************************************************************
    
    ## Reacts to  Compute PLSDA model ----
    
    observeEvent(input$ComputePLSDA,{
      
      output$PLSDAPlots <- renderPlotly({
        text = "PLOT OUTPUT AREA"
        ggplotly(
          ggplot() + 
            annotate("text", x = 4, y = 25, size=8, label = text) + 
            theme_void()
        )
      })
      
      output$PLSDaAConsole <- renderPrint({
        dum <- "Console output area"
        write(dum, file="")
      })
    
    
      #Retrieve proportion of samples for training
      laprop <- as.numeric(input$PropTrainingForPLSDA)
      #ATTN : do not work with XData_p but with the selected items.
      plsda_inTrain <<- caret::createDataPartition(y=Ys_df[,input$YForPLSDA],
                                                   p=laprop, list=FALSE)
      
      
      
      
      Ys <- data.frame(Cl1=Ys_df[,input$YForPLSDA])
      Xs <- sort(input$XsForPLSDA)
      
      if (input$AggregOpForPLSDA=="concatenate"){
        #ATTN : do not work with XData_p but with the selected items.
        for (k in Xs){
          spdf<-as.data.frame(XData_p[[k]][-1,-1])
          pre <- getShortType(k)
          colnames(spdf)<-paste(pre,as.character(XData_p[[k]][1,-1]),sep="_")
          Ys <- cbind(Ys,spdf)
        }
        plsda_set <<- list(Ys)
      }else
      {
        plsda_set <<- lapply(as.list(Xs), function(ii){
          dum <- XData_p[[ii]]
          pre <- getShortType(ii)
          y<-data.frame(Cl1=Ys ,XData_p[[ii]][-1,-1])
          colnames(y)[-1]<-paste(pre,as.character(dum[1,-1]),sep="_")
          return(y)
        })
      }
      
      
      
      
      lambdas<-lapply(Xs, function(ii){
        return(XData_p[[ii]][1,-1])
      })
      
      training <- lapply(plsda_set, function(x) x[plsda_inTrain,])
      testing <- lapply (plsda_set, function(x) x[-plsda_inTrain,])
      
      
      
      
      #Training - may take some time
      waiter <- waiter::Waiter$new(html = spin_3(),  
                                   color = transparent(.75))
      waiter$show()
      on.exit(waiter$hide())
      
      if (input$ResamplingForPLSDA == 'repeatedcv'){
        ctrl<-caret::trainControl(method = input$ResamplingForPLSDA,
                                  number=as.integer(input$NbFoldsForPLSDA),
                                  repeats=as.integer(input$NbRepetitionsForPLSDA),
                                  classProbs = TRUE,
                                  returnData = TRUE,
                                  allowParallel = TRUE)
      }else
      {
        ctrl<-caret::trainControl(method = input$ResamplingForPLSDA,
                                  number=as.integer(input$NbFoldsForPLSDA),
                                  classProbs = TRUE,
                                  returnData = TRUE,
                                  allowParallel = TRUE)
      }
     
      
      
      #Fitting model - This may take some time
      
      if (input$PreproForPLSDA=="None"){
        prepro<-NULL
      }else prepro<-input$PreproForPLSDA 
      
      suppressWarnings(
        plsdaFit <<- lapply(training, function(x){
          caret::train(Cl1~.,
                       data=x,
                       method="pls",
                       tuneLength=as.integer(input$NbLVForPLSDA),
                       trControl=ctrl,
                       preProc=prepro,
                       probMethod=input$PredictMethodForPLSDA,
                       metric=input$PerfMetricForPLSDA)
        })
      )
      
      
      #Compute confusion matrices for train and test set for output
      N_modeles<-length(plsdaFit)
      val_pred_cl <-Predict_plsda(input$AggregOpForPLSDA, 
                                  plsdaFit,probs=FALSE)
      val_confusionmat<-caret::confusionMatrix(data=val_pred_cl,reference = plsdaFit[[1]]$trainingData[,1])
      
      test_pred_cl <- Predict_plsda(input$AggregOpForPLSDA, 
                                    plsdaFit,testing,probs=FALSE)
      test_confusionmat <- caret::confusionMatrix(data=test_pred_cl,reference = testing[[1]][,1])
      
      #Generate output to GUI console.
      lesnoms<-Xs
      dumind<-as.list(1:length(plsdaFit)) 
      plsda_txt_output<-lapply(dumind,function(x) utils::capture.output(print(plsdaFit[[x]])))
      if (input$AggregOpForPLSDA=="concatenate")
        lesnoms=as.list(paste(lesnoms,collapse=" + "))
      for (k in 1:length(plsdaFit)) plsda_txt_output[[k]][1] <- paste("\n******************\nPLSDA on ",
                                                                     lesnoms[[k]], sep = "")
      if (N_modeles>1){
        FUNC<-input$AggregOpForPLSDA 
        plsda_txt_output <- c(plsda_txt_output,
                               paste("*******\nConfusion matrix on training data for models aggregated with ",FUNC,
                                     ".\n",sep=""),
                               utils::capture.output(print(val_confusionmat)))
        plsda_txt_output <- c(plsda_txt_output,
                               paste("*******\nConfusion matrix on test data for models aggregated with ",FUNC,
                                     ".\n",sep=""),
                               utils::capture.output(print(test_confusionmat)))
      }else
      {  plsda_txt_output <- c(plsda_txt_output,
                                paste("*******\nConfusion matrix on training data for model on ",lesnoms[[1]],
                                      ".\n",sep=""),
                                utils::capture.output(print(val_confusionmat)))
      plsda_txt_output <- c(plsda_txt_output,
                             paste("*******\nConfusion matrix on test data for model on ",lesnoms[[1]],
                                   ".\n",sep=""),
                             utils::capture.output(print(test_confusionmat)))
      }
      
      output$PLSDAConsole <- renderPrint({
        dum <- unlist(plsda_txt_output)
        write(dum, file="")
      })
      
      #Plot
      nom_lesX <- sort(input$XsForPLSDA)
      accs <- lapply(plsdaFit,function(p) p$results$Accuracy)
      accsSD <- lapply(plsdaFit,function(p) p$results$AccuracySD)
      N <- length(accs[[1]])
      grp <- rep(unlist(nom_lesX),each=N)
        
      if (input$AggregOpForPLSDA == "concatenate")
        grp <- rep(paste(nom_lesX,collapse=" + "),N*length(accs))
      if (input$ResamplingForPLSDA=="LOOCV") 
        accsSD <- rep(0,length(accsSD))
      
      dfPl <- data.frame(GRP=as.factor(grp),
                         VLs=rep(1:N,times=length(accs)),
                         accs=unlist(accs),
                         accsSD=unlist(accsSD))
      
      dfPl <- cbind(dfPl, data.frame(Min=dfPl$accs-dfPl$accsSD))
      dfPl <- cbind(dfPl, data.frame(Max=dfPl$accs+dfPl$accsSD))
      output$PLSDAPlots <- renderPlotly({
        p <- ggplot2::ggplot(dfPl,ggplot2::aes(x=VLs,y=accs)) + 
          ggplot2::geom_errorbar(ggplot2::aes(ymin=Min,ymax=Max),width=0.2,color="mediumblue") + 
          ggplot2::geom_line(color="blue",lwd=1)
        if (length(levels(dfPl$GRP)) > 1){
          p <- p +  ggplot2::facet_grid(dfPl$GRP~.)
        }else
        {
          p <- p + ggplot2::ggtitle(levels(dfPl$GRP))
        }
        #workaround bug with geom_errorbar
        p$layers[[1]]$geom_params$flipped_aes <- FALSE
        ggplotly(p)
      })
      
      shinyjs::show("PLSDADescript")    
      shinyjs::show("FSavePLSDA")
      whichPLSDAPlot <<- "Validation"
    })
    
    #*******************************************************************
    ## Reacts to plot confusion matrix ----
   
    observeEvent(input$PLSDAConfMatPlot ,
                 whichPLSDAPlot <<- "ConfMat"
    )
    
    
    observe({
      input$PLSDAConfMatPlot
      input$PLSDATrainTestBut
      if (whichPLSDAPlot=="ConfMat"){
        isolate({
          
          if (input$PLSDATrainTestBut=="Validation"){
            pred_cl <- Predict_plsda(input$AggregOpForPLSDA,
                                     plsdaFit,probs=FALSE)
            confusionmat<-caret::confusionMatrix(data=pred_cl,reference = plsdaFit[[1]]$trainingData[,1])
          }else
          {
            testing <- lapply (plsda_set, function(x) x[-plsda_inTrain,])
            pred_cl <- Predict_plsda(input$AggregOpForPLSDA,
                                     plsdaFit,testing,probs=FALSE)
            confusionmat<-caret::confusionMatrix(data=pred_cl,reference = testing[[1]][,1])
          }
          
          
          p <- Plot_Confusion_Matrix(confusionmat$table)
          output$PLSDAPlots <- renderPlotly({
            p
          })
        })
      }
      
    })
   
    #*******************************************************************
    ## Reacts to plot PLSDA Validation plot ----
    
    observeEvent(input$PLSDAvalidationPlot ,
                 whichPLSDAPlot <<- "Validation"
    )
    
    
    observe({
      input$PLSDAvalidationPlot
      if (whichPLSDAPlot=="Validation"){
        isolate({
          nom_lesX <- sort(input$XsForPLSDA)
          accs <- lapply(plsdaFit,function(p) p$results$Accuracy)
          accsSD <- lapply(plsdaFit,function(p) p$results$AccuracySD)
          N <- length(accs[[1]])
          grp <- rep(unlist(nom_lesX),each=N)
          if (input$AggregOpForPLSDA == "concatenate")
            grp <- rep(paste(nom_lesX,collapse=" + "),N*length(accs))
          if (input$ResamplingForPLSDA=="LOOCV") 
            accsSD <- rep(0,length(accsSD))
          dfPl <- data.frame(GRP=as.factor(grp),
                        VLs=rep(1:N,times=length(accs)),
                        accs=unlist(accs),
                        accsSD=unlist(accsSD))
          
          dfPl <- cbind(dfPl, data.frame(Min=dfPl$accs-dfPl$accsSD))
          dfPl <- cbind(dfPl, data.frame(Max=dfPl$accs+dfPl$accsSD))
          output$PLSDAPlots <- renderPlotly({
            p <- ggplot2::ggplot(dfPl,ggplot2::aes(x=VLs,y=accs)) + 
              ggplot2::geom_errorbar(ggplot2::aes(ymin=Min,ymax=Max),width=0.2,color="mediumblue") + 
              ggplot2::geom_line(color="blue",lwd=1)
            if (length(levels(dfPl$GRP)) > 1){
              p <- p +  ggplot2::facet_grid(dfPl$GRP~.)
            }else
            {
              p <- p + ggplot2::ggtitle(levels(dfPl$GRP))
            }
            #workaround bug with geom_errorbar
            p$layers[[1]]$geom_params$flipped_aes <- FALSE
            ggplotly(p)
          })
        })
      }
    })
    
    
    
    #*******************************************************************
    ## Reacts to plot PLSDA Prob. boxplot ----
    
    observeEvent(input$PLSDAProbBoxPlot,
                 whichPLSDAPlot <<- "ProBoxplot"
    )
    
    
    observe({
      input$PLSDAProbBoxPlot
      input$PLSDATrainTestBut
      if (whichPLSDAPlot=="ProBoxplot"){
        isolate({
          
          
          if (input$PLSDATrainTestBut == "Validation"){
            pred_prob <- Predict_plsda(input$AggregOpForPLSDA, plsdaFit,probs=TRUE)
            dum1<-data.frame(cl=plsdaFit[[1]]$trainingData[,1],pred_prob)
            
          }else
          {
            testing <- lapply (plsda_set, function(x) x[-plsda_inTrain,])
            pred_prob <- Predict_plsda(input$AggregOpForPLSDA, plsdaFit,testing,probs=TRUE)
            dum1<-data.frame(cl=testing[[1]][,1],pred_prob)
            
          }
          dum2<-tidyr::gather(dum1,Pred,Prob,-cl,factor_key = TRUE)
          levels(dum2$cl)=paste("True: ",levels(dum2$cl),sep="")
          
          output$PLSDAPlots <- renderPlotly({
            ggplotly(
                ggplot2::ggplot(dum2,ggplot2::aes(Pred,Prob)) +
                ggplot2::geom_boxplot()+ggplot2::facet_wrap(~cl) +
                ggplot2::theme(text = element_text(size=12)) +
                ggplot2::theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1))
            )
          })
         
        })
      }
    })
    
    
    #*******************************************************************
        ## Reacts to plot PLSDA B-Coeff ----
    
    observeEvent(input$PLSDABCoeffPlot,
                 whichPLSDAPlot <<- "BCoeffs"
    )
    
    
    observe({
      input$PLSDABCoeffPlot
      if (whichPLSDAPlot=="BCoeffs"){
        isolate({
          if (input$AggregOpForPLSDA == "concatenate"){
            x_id=plsdaFit[[1]]$coefnames
            x_id=strsplit(x_id,"_")
            N_xlabels <- length(x_id)
            x_id=unlist(x_id)
            x_cls=x_id[seq(1,2*N_xlabels,2)]
            xlabels=as.numeric(x_id[seq(2,2*N_xlabels,2)])
            nc=plsdaFit[[1]]$bestTune$ncomp
           
            df=data.frame(Cl=x_cls, 
                          xlabs=as.numeric(xlabels))
            for (k in 1:length(plsdaFit[[1]]$levels)){
              eval(parse(text=paste0("tt=data.frame(",plsdaFit[[1]]$levels[k],"=plsdaFit[[1]]$finalModel$coefficients[,k,nc])")))
              df = cbind(df,tt)
            }
            
            df1 <- tidyr::gather(data=df,key="Class",value,-Cl,-xlabs)
            
            p <- ggplot2::ggplot(df1, ggplot2::aes(x=xlabs,y=value,colour=Class))+
              ggplot2::geom_smooth() + ggplot2::facet_wrap(~Cl, ncol=1,scales="free_y")
            
          }else
          {
            coeffs <- lapply(plsdaFit, function(x) coef(x$finalModel))
            nom_lesX <- as.list(sort(input$XsForPLSDA))
            lesNoms <- sapply(strsplit(unlist(nom_lesX),'_'),"[[",1)
            wl <- lapply(as.list(nom_lesX), function(ii) as.numeric(XData_p[[ii]][1,-1]))
            plotframe<-cbind(as.data.frame(coeffs[[1]][,,1]),
                             data.frame(wl=wl[[1]],
                                        source=rep(lesNoms[1],length(wl[[1]]))))
            N_Classes<-ncol(coeffs[[1]])
            colnames(plotframe)[1:N_Classes]<-colnames(coeffs[[1]])
            if (length(wl)>1){
              for (k in (2:length(wl))){
                dum<-cbind(coeffs[[k]][,,1],
                           data.frame(wl=wl[[k]],
                                      source=rep(lesNoms[k],length(wl[[k]]))))
                colnames(dum)[1:N_Classes]<-colnames(plotframe)[1:N_Classes]
                plotframe<-rbind(plotframe,dum)
              }
            }
            dum<-reshape2::melt(plotframe,id.vars=(N_Classes+c(1,2)),
                                variable.name="Class",
                                value.name="B_coeffs")
            p <- ggplot2::ggplot(dum,ggplot2::aes(x=wl,y=B_coeffs,colour=Class))
            p <- p + ggplot2::geom_smooth() + ggplot2::facet_wrap(~source, ncol=1,scales="free_y")
          }
          
          output$PLSDAPlots <- renderPlotly((
            ggplotly(p)
          ))
          
        })
      }
    })
    
    # *********************************************************************
    
    ## Reacts to ShowPLSDAPredTable button ----
    observeEvent(input$ShowPLSDAPredTable,{
      
      pr<-Predict_plsda(input$AggregOpForPLSDA, plsdaFit,plsda_set,probs=TRUE)
      pr<-round(pr,2)
      lesdiffs<-t(apply(pr,1,sort,decreasing=TRUE))
      lesdiffs<-lesdiffs[,1]-lesdiffs[,2]
      lescl<-Predict_plsda(input$AggregOpForPLSDA,plsdaFit,plsda_set,probs=FALSE)
      letest <- lescl==plsda_set[[1]][,1]
      pr<-data.frame(NoSeq=Ys_df$NoSeq,
                     EchID=Ys_df[,1],
                     True_Cl=plsda_set[[1]][,1],
                     Pred_Cl=lescl,
                     Test=letest,
                     minDiff=lesdiffs,pr)
      output$PlsDAPredTable = renderDataTable(pr,
                                              filter='top')
    })
    
    # *********************************************************************
    
    ## Reacts to Save button on modal PlsDAPredTable ----
    observeEvent(input$savePLSDAPreds, {
      
      leFichier <- utils::choose.files(paste0(projectDir,"\\*.*"),
                                       multi = F, filters = Filters[c("txt"),])
      if (length(leFichier) > 0) {
        isolate({
          pr<-Predict_plsda(input$AggregOpForPLSDA, plsdaFit,plsda_set,probs=TRUE)
          pr<-round(pr,2)
          lesdiffs<-t(apply(pr,1,sort,decreasing=TRUE))
          lesdiffs<-lesdiffs[,1]-lesdiffs[,2]
          lescl<-Predict_plsda(input$AggregOpForPLSDA,plsdaFit,plsda_set,probs=FALSE)
          letest <- lescl==plsda_set[[1]][,1]
          pr<-data.frame(NoSeq=Ys_df$NoSeq,
                         EchID=Ys_df[,1],
                         True_Cl=plsda_set[[1]][,1],
                         Pred_Cl=lescl,
                         Test=letest,
                         minDiff=lesdiffs,pr)
          utils::write.table(pr,
                             file=leFichier,
                             sep="\t",
                             dec=".",
                             row.names = FALSE,
                             quote=FALSE)
        })
      }
    })
    
    # *********************************************************************
    
    ## Reacts to save PLSDA model button ----
    observe({
      volumes <- c("UserFolder"=projectDir)
      shinyFileSave(input, "FSavePLSDA", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$FSavePLSDA)
      if (nrow(fileinfo) > 0) {
        isolate({
          leFichier <- fileinfo$datapath
          PP_params <- collectPreProParams(PPvaluesTrunc,input)
          #Need to remove some as not all XDataList members are in XsForPLS
          toRemove <- setdiff(PP_params$lesNoms,sort(input$XsForPLSDA))
          if (length(toRemove)>0){
            removeInd <- unlist(lapply(toRemove, function(x) which(x==PP_params$lesNoms)))
            PP_params$lesNoms <- as.list(sort(input$XsForPLSDA))
            PP_params$trunc_limits <- PP_params$trunc_limits[-removeInd,]
            lapply(toRemove, function(id){
              PP_params$perSpecParams[[id]] <<- NULL
              PP_params$savgolParams$doSavGol[[id]] <<- NULL
              PP_params$savgolParams$w[[id]] <<- NULL
              PP_params$savgolParams$p[[id]] <<- NULL
              PP_params$savgolParams$m[[id]] <<- NULL
            })
          }
          nom_lesX <- sort(input$XsForPLSDA)
          shortNames <- getShortType(nom_lesX)
          model_descript <- list(
            type = "PLSDA",
            description = input$PLSDADescript,
            datatype = shortNames,
            aggregation = input$AggregOpForPLSDA
          )
          pls_ncomp <- lapply(plsdaFit,function(x) x$bestTune$ncomp)
          PP_params <- stripPreProNames(PP_params)
          
          #COnvert to InSpectoR format
          dum <- prepro_Shin_2_InSp(PP_params)
          prepro_params <- dum$prepro_params
          # Remove everything in plsdaFits except finalModel and trainingData
          plsdaFit <- lapply(plsdaFit, function(f) 
            f[names(f) %in% c("finalModel","trainingData")])
          save(model_descript,prepro_params,plsdaFit,pls_ncomp,file=leFichier)
        })
      }
    })
    
    
    
    # *********************************************************************
    
    ## Plot Prob biplots in a modal window ----
    observeEvent(input$PLSDAProbBiPlot,{
      
      isolate({
        lesSizes <- c(10,10,10,8,8,7,7,7,7,7,5,5,5,5,rep(5,10))
       
        if (input$PLSDATrainTestBut == "Validation"){
          pred_prob <- Predict_plsda(input$AggregOpForPLSDA, plsdaFit,probs=TRUE)
          dum1<-data.frame(pred_prob,Class=plsdaFit[[1]]$trainingData[,1])
          
        }else
        {
          testing <- lapply (plsda_set, function(x) x[-plsda_inTrain,])
          pred_prob <- Predict_plsda(input$AggregOpForPLSDA, plsdaFit,testing,probs=TRUE)
          dum1<-data.frame(pred_prob,Class=testing[[1]][,1])
          
        }
        splitDot<-strsplit(colnames(dum1), ".", fixed = TRUE)
        colnames(dum1) <- lapply(splitDot, function(x) x[1])
        
        nCl <- nlevels(dum1$Class)
        textSize <- lesSizes[nCl-1]
        lesPlots <- vector(mode = "list", length = (nCl-1)^2)
        
        
        
        for (i in 1:(nCl-1)){
          for (j in (2:nCl)){
            k=(j-2)*(nCl-1)+i
            if (j>i){
              
              xCol <- colnames(dum1)[i]
              yCol <- colnames(dum1)[j]
              lesPlots[[k]] <- ggplot(dum1, aes(.data[[xCol]], .data[[yCol]], shape=Class)) +
                geom_point(size=1.5, show.legend = FALSE) +
                scale_shape_manual(values=c(0:3,15:19)) +
                stat_ellipse(level=0.95) + 
                theme(text=element_text(size=textSize))
              if (k>1) lesPlots[[k]] <- lesPlots[[k]] +
                theme(legend.position="none")
            }else
            {
              if (k==(nCl-1)){
                xCol <- colnames(dum1)[1]
                yCol <- colnames(dum1)[2]
                unPlot <- ggplot(dum1, aes(.data[[xCol]], .data[[yCol]], shape=Class))  +
                  geom_point(size=1.5) +
                  scale_shape_manual(values=c(0:3,15:19)) +
                  stat_ellipse(level=0.95) + 
                  theme(text=element_text(size=round(textSize*1.5)))  +
                  guides(shape=guide_legend(title=paste0("Class - ",
                                                         input$PLSDATrainTestBut,
                                                         " set")))
                
                legend <- cowplot::get_legend(unPlot)
              }
              lesPlots[[k]] <- cowplot::ggdraw()
            }
          }
        }
        
        output$plsdaProbBiPlots <- renderPlot({
          m1 <- arrangeGrob(grobs=lesPlots,nrow=nCl-1,ncol=nCl-1,top=NULL)
          if(nCl>3){
            lesWidths <- c(0.8,0.2)
          }else
          {
            lesWidths <- c(0.7,0.3)
          }
          plot_2_save <<- arrangeGrob(m1,cowplot::ggdraw(legend),ncol=2,widths=lesWidths)
          grid.arrange(plot_2_save)
        })
      })
    })
    
    # *********************************************************************
    
    ## Save Prob biplots in modal window ----
    observeEvent(input$savePLSDAProbBiplot,{
    
      #SET UP filename
      # plotFilename <- rstudioapi::selectFile(
      #   caption="Select file name to store plot", existing=FALSE)
      plotFilename <- choose.files("Select file name to store plot",
                                   multi = FALSE,
                                   filters = Filters[c("png","jpeg","pdf"),])
      laDevice <- tools::file_ext(plotFilename)
      ggsave(file=plotFilename,plot=grid.arrange(plot_2_save),
             device=laDevice)
    })
    
    # *********************************************************************
    # Apply tab ----
    ## Reacts to apply tab activation ----
    observeEvent(input$tabs,{
      if(input$tabs == "Apply models"){ #set up the pages
        
        output$modelTable <-   renderDataTable(
          datatable(data.frame(Note="TABLE OUTPUT AREA"))
        )
        output$modelPlot <- renderPlotly({
          text = "PLOT OUTPUT AREA"
          ggplotly(
            ggplot() + 
              annotate("text", x = 4, y = 25, size=8, label = text) + 
              theme_void()
          )
          
        })
        output$modelType <- renderText("No model selected")
        output$modelDescOnApply <- renderText("No model selected")
        output$dataTypeOnApply <- renderText("No model selected")
      }
      
      shinyjs::hide("applyModel")
      shinyjs::hide("saveModelResults")
      shinyjs::hide("FirstPCApplyPCA")
      shinyjs::hide("LastPCApplyPCA")
      shinyjs::hide("modelPlot")
      shinyjs::hide('modelTable')
      shinyjs::hide('factorsToShow')
      
    })
    
    # *********************************************************************
    
    ## Reacts to FLoadModel button ----
    observe({
      volumes <- c("UserFolder"=projectDir)
      shinyFileChoose(input, "FLoadModel", roots=volumes, session=session)
      fileinfo <- parseFilePaths(volumes, input$FLoadModel)
      if (nrow(fileinfo) > 0){
        modelEnv <<- new.env()
        leFichier <- fileinfo$datapath
        load(file=leFichier, envir = modelEnv) 
        #Convert Prepro params
        modelEnv$PP_params <- prepro_InSp_2_ShiInSp(modelEnv$model_descript,modelEnv$prepro_params)
        output$modelType <- renderText(modelEnv$model_descript$type)
        output$modelDescOnApply <- renderText(modelEnv$model_descript$description)
        shortTypeMod <- getShortType(modelEnv$model_descript$datatype)
        shortTypeLoaded <- getShortType(ALLXDataList)
        if (all(shortTypeMod %in% shortTypeLoaded)){  #Required data available
          output$dataTypeOnApply <- renderText(paste0(shortTypeMod,collapse="\n"))
          shinyjs::show("applyModel")
          shinyjs::hide("saveModelResults")
          if (modelEnv$model_descript$type == "PCA"){
            shinyjs::show("FirstPCApplyPCA")
            shinyjs::show("LastPCApplyPCA")
            shinyjs::hide("factorsToShow")
            updateSelectInput(session,"FirstPCApplyPCA",
                            choices=1:(as.numeric(modelEnv$NCPs)-1), selected=1)
            updateSelectInput(session,"LastPCApplyPCA",
                            choices=2:modelEnv$NCPs, selected=2)
          }else
          {
            shinyjs::hide("FirstPCApplyPCA")
            shinyjs::hide("LastPCApplyPCA")
            shinyjs::show("factorsToShow")
            updateSelectInput(session,"factorsToShow",
                              choice = names(Filter(is.factor,Ys_df)),
                              selected=names(Filter(is.factor,Ys_df))[1])
          }
        }else         #required data not available
          output$dataTypeOnApply <- renderText(paste0("Required data types\n",
                                                      "not available.\n",
                                                      "Cannot apply this\n",
                                                      "model!"))
        
      }
      shinyjs::hide("modelTable")
      shinyjs::hide("modelPlot")
    })
    
    # *********************************************************************
    
    ## Reacts to applyModel button ----
    observeEvent(input$applyModel,{
     
      switch(modelEnv$model_descript$type,
             ### PCA ----
             PCA = {
                      #Modify names
                      locPP_params <- buildPreProNames(modelEnv$PP_params)
                      
                      #Do preprocessing
                      Apply_PrePro(locPP_params,input)
                      lesNoms <- names(XData_p)
                      
                      #Apply model
                      y <- NULL
                      for (k in lesNoms){
                        spdf<-as.data.frame(XData_p[[k]][,-1])
                        pre <- strsplit(k,"_")[[1]][1]
                        pre <- strsplit(pre,"w")[[1]][1]
                        colnames(spdf)<-paste(pre,as.character(XData_p[[k]][1,-1]),sep="_")
                        if (is.null(y)){
                          y <- spdf
                        }else
                        {
                          y <- cbind(y,spdf)
                        }
                      }
                      y <- cbind(data.frame(ID=c("ID",Ys_df[[1]])),y)
                      dat_4_PCA <- y[-1,-1]
                      lesPreds <<- predict(modelEnv$lePCA,
                                      newdata=dat_4_PCA)[,1:modelEnv$NCPs]
                      i1 <- as.integer(input$FirstPCApplyPCA)
                      i2 <- as.integer(input$LastPCApplyPCA)
                      N <- i2-i1+1
                      
                      #Plot scores
                      shinyjs::show("modelPlot")
                      data <- modelEnv$lePCA$x[,1:modelEnv$NCPs]
                      dats <- rbind(data,lesPreds)[,i1:i2]
                      colorCodes <-factor(c(as.character(modelEnv$colorby),rep("Pred.",nrow(lesPreds))),
                                             levels <- c(as.character(unique(modelEnv$colorby)),
                                                         "Pred."))
                      #dats <- cbind(dats,as.data.frame(colorCodes))
                      nCl <- length(unique(modelEnv$colorby))
                      
                      couleurs <- c(mesCouleurs[1:nCl],"#000000FF")
                      
                      # #My own plot
                      # #First create plots and then arrange
                      # #PC scatter plots
                      
                      sp <- list() #upper row - nothing yet
                      for (i in 1:N^2){
                        sp <- c(sp, list(
                          ggplot2::ggplot() +
                            ggplot2::theme_void() +
                            ggplot2::geom_text(ggplot2::aes(0,0,label=' ')) +
                            ggplot2::xlab(NULL)
                          )
                        )
                      }
                      #Set up legend
                      if (N>2){
                        kk=N^2-1
                        df <- cbind(dats[,c(1,2)],as.data.frame(colorCodes),
                                    row.names=NULL)
                        nom1 <- names(df)[1]
                        nom2 <- names(df)[2]
                        sp[[kk]] <- ggplot2::ggplot(df, 
                                                    ggplot2::aes(x = .data[[nom1]],
                                                                 y = .data[[nom2]], 
                                                                 color = colorCodes)) +
                          ggplot2::geom_point() + 
                          ggplot2::scale_color_manual(values=couleurs) +
                          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 6))) +
                          theme(legend.title=element_text(size=20),
                                legend.text=element_text(size=14))
                        leg <- ggpubr::get_legend(sp[[kk]])
                        sp[[kk]] <- ggpubr::as_ggplot(leg)
                      }
                      
                    
                      # #Lower as PCs scatter
                      for (i in (i1+1):i2){
                        for (j in i1:(i-1)){
                          kk <- (j-i1)*N+(i-i1+1)
                          df <- cbind(dats[,c(j-i1+1,i-i1+1)],as.data.frame(colorCodes),
                                     row.names=NULL)
                           nom1 <- names(df)[1]
                           nom2 <- names(df)[2]
                           sp[[kk]] <- ggplot2::ggplot(df, 
                                                       ggplot2::aes(x = .data[[nom1]],
                                                            y = .data[[nom2]], 
                                                            color = colorCodes)) +
                             ggplot2::scale_color_manual(values=couleurs) +
                                              ggplot2::geom_point(show.legend = FALSE) + 
                             theme(axis.text = element_text(size = 14)) + 
                             theme(axis.title = element_text(size = 18))
                        }
                      }
                      # #Diag as densities
                      pos <- c(1,5,9)
                      for (i in i1:i2){
                        df <- cbind(dats[,(i-i1+1)],as.data.frame(colorCodes),
                                    row.names=NULL)
                        nom1 <- names(df)[1]
                        sp[[(i-i1)*N+(i-i1+1)]] <-
                          ggplot2::ggplot(df, ggplot2::aes(x=.data[[nom1]],
                                                           fill=colorCodes)) +
                          ggplot2::geom_density(adjust=1.5, alpha=.6, show.legend=FALSE) +
                          ggplot2::scale_fill_manual(values=couleurs) +
                          ggplot2::labs(x=paste0("PC",i))+ 
                          theme(axis.text = element_text(size = 14)) + 
                          theme(axis.title = element_text(size = 18))
                      }
                      
                      #OD-SD plot on top right
                      kk=N*(N-1)+1
                      #ODist vs SDist 
                      #Calcul OD et SD pour l'échantillon
                      #Voir manuel de PLS Toolbox de EigenVector dans Doc du projet
                      scs <- lesPreds
                      eigVals <- modelEnv$lePCA$sdev[1:modelEnv$NCPs] 
                      midMat <- diag(1/eigVals^2)
                      mySD <- sqrt(diag(scs %*% midMat %*% t(scs)))
                      
                      x <- as.matrix(dat_4_PCA-matrix(modelEnv$lePCA$center,
                                                       nrow=nrow(dat_4_PCA),
                                                       ncol=ncol(dat_4_PCA),
                                                       byrow = T))
                      lds <- modelEnv$lePCA$rotation[,1:modelEnv$NCPs]
                      midMat <- diag(nrow=dim(x)[2]) - lds %*% t(lds)
                      myOD <- sqrt(diag(x %*% midMat %*% t(x)))
                      
                      
                      df <- data.frame(SD=c(modelEnv$dds$SDist, mySD),
                                       OD=c(modelEnv$dds$ODist, myOD),
                                       colorCodes = colorCodes)
                      nom1 <- names(df)[1]
                      nom2 <- names(df)[2]
                      sp[[kk]] <- ggplot2::ggplot(df, 
                                                  ggplot2::aes(x = .data[[nom1]],
                                                               y = .data[[nom2]], 
                                                               color = colorCodes)) 
                      if (N==2){
                        sp[[kk]] <- sp[[kk]] +  ggplot2::geom_point(show.legend = TRUE) +
                          theme(legend.title=element_text(size=20),
                                legend.text=element_text(size=14))
                      }else
                      {
                        sp[[kk]] <- sp[[kk]] +  ggplot2::geom_point(show.legend = FALSE)
                      }
                      sp[[kk]] <- sp[[kk]] +
                        ggplot2::scale_color_manual(values=couleurs) +
                        ggplot2::labs(x="Score distance",
                                      y="Outside distance")+ 
                        theme(axis.text = element_text(size = 14)) + 
                        theme(axis.title = element_text(size = 18))
                      
                      
                      output$modelPlot <- renderPlot({
                        gridExtra::marrangeGrob(sp,ncol=N,nrow=N,top=NULL)
                      })
                      lesPreds <<- cbind(lesPreds,data.frame(OD=myOD,SD=mySD))
                   },
             ## PLS -----
             PLS = {
                   #Modify names
                   locPP_params <- buildPreProNames(modelEnv$PP_params)
                   
                   #Do preprocessing
                   Apply_PrePro(locPP_params,input)
                   
                   lesNoms <- names(XData_p)
                   lesFacs <- input$factorsToShow
                   y <- NULL
                   
                   #Apply model
                   for (k in lesNoms){
                       spdf<-as.data.frame(XData_p[[k]][,-1])
                       pre <- strsplit(k,"_")[[1]][1]
                       pre <- strsplit(pre,"w")[[1]][1]
                       colnames(spdf)<-paste(pre,as.character(XData_p[[k]][1,-1]),sep="_")
                       if (is.null(y)){
                         y <- spdf
                       }else
                       {
                         y <- cbind(y,spdf)
                       }
                     }
                     data_4_PLSDA <- list(y[-1,])
                     y <- cbind(Ys_df[lesFacs],y[-1,])
                    
                     
                     
                     shinyjs::hide("modelPlot")
                     
                     shinyjs::show("modelTable")
                     
                     pls_set <<- y
                     
                     
                     plspreds<-predict(modelEnv$plsFit[[1]],
                                       newdata = pls_set,
                                       ncomp=modelEnv$pls_ncomp)
                     
                     lesPreds <<-data.frame(Prediction=plspreds)
                     output$modelTable = renderDataTable(cbind(Ys_df[lesFacs],
                                                               Prediction=lesPreds),
                                                         options=list(
                                                           autoWidth=FALSE,
                                                           dom = "<lf<\"datatables-scroll\"t>ipr>",
                                                           rownames = FALSE,
                                                           class="compact",
                                                           lengthMenu = list(c(10, 15, 20, -1), c('10', '15', '20','All')),
                                                           pageLength = 10,
                                                           # scrollX = TRUE,
                                                           style = "bootstrap",
                                                           columnDefs = list(
                                                             list(orderable = TRUE, targets = 0),
                                                             list(width = '20px', targets = 0),
                                                             list(className = "dt-center", targets = "_all")
                                                             #columnDefs = list(list(orderable = TRUE, targets = 0)
                                                           )
                                                         ),filter='top')
                     
                     
                   
                     },
             ## PLSDA ----
             PLSDA = {
                       #Modify names
                       locPP_params <- buildPreProNames(modelEnv$PP_params)
                       
                       #Do preprocessing
                       Apply_PrePro(locPP_params,input)
                       lesNoms <- names(XData_p)
                       lesFacs <- input$factorsToShow
                       y <- NULL
                       
                       #Apply model
                       if (modelEnv$model_descript$aggregation=="concatenate"){
                         for (k in lesNoms){
                           spdf<-as.data.frame(XData_p[[k]][,-1])
                           pre <- strsplit(k,"_")[[1]][1]
                           pre <- strsplit(pre,"w")[[1]][1]
                           colnames(spdf)<-paste(pre,as.character(XData_p[[k]][1,-1]),sep="_")
                           if (is.null(y)){
                             y <- spdf
                           }else
                           {
                             y <- cbind(y,spdf)
                           }
                         }
                         data_4_PLSDA <- list(cbind(Ys_df[,1],y[-1,]))
                         y <- cbind(Ys_df[lesFacs],y[-1,])
                       }else
                       {
                         
                         nameList <- as.list(lesNoms)
                         data_4_PLSDA <- lapply(nameList, function(ii){
                           y<-data.frame(Ys_df[,1],XData_p[[ii]][-1,-1])
                            colnames(y)<-as.character(XData_p[[ii]][1,])
                           return(y)
                         })
                       }
                       
                       
                       shinyjs::hide("modelPlot")
                       
                       shinyjs::show("modelTable")
                       
                       plsda_probs <- Predict_plsda(modelEnv$model_descript$aggregation,
                                                    modelEnv$plsdaFit,
                                                    mydata=data_4_PLSDA,probs=TRUE)
                       plsda_cl <- Predict_plsda(modelEnv$model_descript$aggregation,
                                                 modelEnv$plsdaFit,
                                                 mydata=data_4_PLSDA,probs=FALSE)
                       
                       plsda_probs<-round(plsda_probs,2)
                       lesdiffs<-t(apply(plsda_probs,1,sort,decreasing=TRUE))
                       lesdiffs<-lesdiffs[,1]-lesdiffs[,2]
                       lesPreds <<-data.frame(
                                      Pred_Cl=plsda_cl,
                                      Pred_prob = plsda_probs,
                                      minDiff=lesdiffs)
                       output$modelTable = renderDataTable(cbind(Ys_df[lesFacs],lesPreds),
                                                           options=list(
                                                             autoWidth=FALSE,
                                                             dom = "<lf<\"datatables-scroll\"t>ipr>",
                                                             rownames = FALSE,
                                                             class="compact",
                                                             lengthMenu = list(c(10, 15, 20, -1), c('10', '15', '20','All')),
                                                             pageLength = 10,
                                                             # scrollX = TRUE,
                                                             style = "bootstrap",
                                                             columnDefs = list(
                                                               list(orderable = TRUE, targets = 0),
                                                               list(width = '20px', targets = 0),
                                                               list(className = "dt-center", targets = "_all")
                                                               #columnDefs = list(list(orderable = TRUE, targets = 0)
                                                             )
                                                           ),filter='top')
               
                    }
             )
      shinyjs::show("saveModelResults")
    })
    
    # *********************************************************************
    
    ## Reacts to saveModelResults button ----
    observe({
      volumes <- c("UserFolder"=projectDir)
      shinyFileSave(input, "saveModelResults", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$saveModelResults)
      if (nrow(fileinfo) > 0) {
        leFichier <- fileinfo$datapath
        outDF <- cbind(Ys_df,data.frame(Prediction=lesPreds))
        utils::write.table(outDF,file=leFichier,
                           row.names=FALSE, sep="\t")
        
      }
    })
})
    

      
    
    
