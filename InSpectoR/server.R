#
#Server for ShInSpectoR# Load libraries ----



shinyServer(function(input, output, session) {
 

    # To make modal window for loading plots draggable ----
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
        Apply_PrePro(preproParams)
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
              return("No valide Y file selected")
            }
            indi2 <- which(!stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
            if (length(indi2)==0) 
              return('No spectra file selected')
            inFile$name[indi]
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
            p <- plotly::plot_ly(dfs_plotly,x=~X,y=~Spectra,
                                 color = ~ID, colors = mycolors,
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
                            colors = mycolors,
                            size=8,
                            text=as.character(Ys_df[[1]]),
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
                        colors = mycolors,
                        size=8,
                        text=as.character(Ys_df[[1]]),
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
    
    
    # ********************************************************************* 
   
    ## Toggles Plot Loadings action button ---- 
    # Both pc1 and pc2 inputs must be a principal components for button to show
    observe({ toggle(id="plotloadings", 
                     condition=all(stringr::str_detect(c(input$pc1,input$pc2),"PC")))})
    
    
    # *********************************************************************
    
    ## Loads data ----
    observeEvent(input$files, {
        k=1
        inFile <- input$files
        if (is.null(inFile))
            return(NULL)
        indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
        if (length(indi)==0) 
          return('NULL')
        indi <- which(!stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
        if (length(indi)==0) 
          return('NULL')
        #Load Y data
        indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
        Ys_df <<- read.table(file=inFile$datapath[indi],header=TRUE,sep="\t",dec=".",
                             na.strings = "", stringsAsFactors = T)
        #make.unique to deal with repeated sample ID.
        Ys_df[,1] <<- as.factor(make.unique(as.character(Ys_df[,1])))
        Ys_df <<- cbind(Ys_df,data.frame(NoSeq=seq(1:nrow(Ys_df))))
        ORI_Ys_df <<- Ys_df
        #load all XData in All_XData
        indi <- which(!stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
        # indiDebug <<- indi
        for (ii in 1:length(indi)){
            if (indi[ii]){
                dum1<-read.table(file=inFile$datapath[ii],sep="\t", dec=".",header=FALSE)
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
                  leNom <- tools::file_path_sans_ext(inFile$name[[ii]])  
                  All_XData[[leNom]] <<- dum1
                }
            }
        }
        ORI_XData <<- All_XData
        #files are loaded - remove extension from filenames
        inFile$name<- tools::file_path_sans_ext((inFile$name))
        
        #Sets max nb of pcs to 15 or the number of samples-1 if smaller
        nSamples <- nrow(All_XData[[1]])-1
        maxnCP <- ifelse (nSamples < 21, nSamples-1, 20 )
        updateSelectInput(session,"npcs",choices=1:maxnCP,
                          selected=2)
        
        lesChoix <- computePCAonRaw(as.numeric(input$npcs),
                                    doRayleigh = TRUE)
        
        #Populate Xs file selection and select first by default
        updateSelectInput(session, "Xs",               
                          choices=inFile$name[indi],
                          selected=inFile$name[indi][1])
        ALLXDataList <<- inFile$name[indi]
        
        #Populate PCA data selection
        updateSelectInput(session,"PCA_data",
                          choices=inFile$name[indi],
                          selected=inFile$name[indi][1])
        
        
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
                                      list(width = '15px', targets = 0),
                                      list(width = '75px', targets = 1:(ncol(Ys_df)-1)),
                                      list(className = "dt-center", targets = "_all")
                                      #columnDefs = list(list(orderable = TRUE, targets = 0)
                                    )
                                ),
                                filter='top')
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
                              numericInput(paste0(id,'_E'), 'Bandwidth',5,5,25,2),
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
          Apply_PrePro(preproParams)
          lesChoix <- computePCAonRaw(as.numeric(input$npcs),doRayleigh = FALSE)

          
          #Force redraw
          isolate(proxy_Ys %>% selectRows(NULL))
        })
    })
    
    
    # *********************************************************************
    
    ## Reacts to clearRows button ----
    observeEvent(input$clearRows, {
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
      rangees <- input$Ys_rows_all
      proxy_Ys %>% clearSearch()
    })
    
    
    # *********************************************************************
    
    
    ## Reacts to deleteRows button ----
    observeEvent(input$deleteRows, {
        if (!is.null(input$Ys_rows_selected)) {
            lesRows <- unique(as.numeric(input$Ys_rows_selected))
            #Remove rows in All_XData, .
            #Recompute PCAs and update PCAs, PCAsdt_dds and PCAsdt_dds_crit
            Yvalues$dfWorking$x$data <- Yvalues$dfWorking$x$data[-lesRows,]
            Ys_df <<- Yvalues$dfWorking$x$data
            for (k in 1:length(All_XData)){
                All_XData[[k]] <<- All_XData[[k]][-(1+lesRows),]
            }
            
            for (k in 1:length(XData_p)){
              XData_p[[k]] <<- XData_p[[k]][-(1+lesRows),]
            }
            lesChoix <- computePCAonRaw(as.numeric(input$npcs),doRayleigh=FALSE)
        }
        proxy_Ys %>% selectRows(NULL)
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
                     sels <<- as.data.frame(cbind(d$x,d$y))
                     refs <<- as.data.frame(cbind(xall,yall))
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
      if (clmn<3) {
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
      Apply_PrePro(preproParams)
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
      volumes <- c("UserFolder"=fs::path_home())
      shinyFileSave(input, "FSavePrePro", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$FSavePrePro)
      if (nrow(fileinfo) > 0) {
        leFichier <- fileinfo$datapath
        PP_params <- collectPreProParams(PPvaluesTrunc,input)
        save(PP_params,file=leFichier)
      }
    })
    
    # *********************************************************************
    
    ## Reacts to load prepro button ----
    observe({
      volumes <- c("UserFolder"=fs::path_home())
      shinyFileChoose(input, "FLoadPrePro", roots=volumes, session=session)
      fileinfo <<- parseFilePaths(volumes, input$FLoadPrePro)
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
        }
        
        #Check spectrum types
        if (!all(XDataList == PP_params$lesNoms)){   #spectrum types do not match
          showModal(modalDialog(
            title = "WARNING",
            "Selected spectrum types do not match!"
          ))
        }else
        {
          #Load truncation limits
          isolate(PPvaluesTrunc$dfWorking$x$data[,2:3] <- PP_params$trunc_limits[,1:2])
          
          #update per spectrum and SavGol options
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
          Apply_PrePro(preproParams)
          
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
      XNames <- as.list(input$XsForPLS)
      valid <- input$ResamplingForPLS
      
      if (aggr=="Concatenate spectra"){
        y <- data.frame(V1=Ys_df[[YName]])
        for (k in XNames){
          spdf<-as.data.frame(XData_p[[k]][-1,-1])
          pre <- strsplit(k,"_")[[1]][1]
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
      volumes <- c("UserFolder"=fs::path_home())
      shinyFileSave(input, "FSavePLS", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$FSavePLS)
      if (nrow(fileinfo) > 0) {
        leFichier <- fileinfo$datapath
        PP_params <- collectPreProParams(PPvaluesTrunc,input)
        
        #Need to remove some as not all XDataList members are in XsForPLS
        toRemove <- setdiff(PP_params$lesNoms,input$XsForPLS)
        removeInd <- unlist(lapply(toRemove, function(x) which(x==PP_params$lesNoms)))
        PP_params$lesNoms <- as.list(input$XsForPLS)
        i <- 0
        lapply(toRemove, function(id){
          i <<- i+1
          PP_params$trunc_limits <<- PP_params$trunc_limits[-removeInd[i],]
          PP_params$perSpecParams[[id]] <<- NULL
          PP_params$savgolParams$doSavGol[[id]] <<- NULL
          PP_params$savgolParams$w[[id]] <<- NULL
          PP_params$savgolParams$p[[id]] <<- NULL
          PP_params$savgolParams$m[[id]] <<- NULL
        })
        model_descript <- list(
          type = "PLS",
          description = input$PLSDescript,
          datatype = input$XsForPLS,
          aggregation = input$AggregateForPLS
        )
        pls_ncomp <- as.numeric(input$NbLVPLS_Sel)
        colorby <- input$PLSPredPlotColorBy
        save(model_descript,PP_params,plsFit,pls_ncomp,colorby,file=leFichier) 
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
    observeEvent(input$savePLSPreds ,{
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
      
      outfile=choose.files(caption="Define file name",
                           multi=FALSE, 
                           filters=Filters[c("txt"),])
      utils::write.table(dat_tbl,
                         file=outfile,
                         sep="\t",
                         dec=".",
                         row.names = FALSE,
                         quote=FALSE)
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
          Apply_PrePro(preproParams)
          
          #Computes PCA on first spectrum in input$X to start with
          doPCA(XData_p[[input$Xs[1]]])
          
          updateSelectInput(session, 'PCATopPlotType',
                              selected = 'Screeplot')
          updateSelectInput(session, "NPCsforPCA", choices=1:lePCA_NCPs)
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
     
      
      switch(input$PCATopPlotType,
             Scores = {
                   plotly::plot_ly() %>%
                   add_markers(data=dfsPCA,          #Plot all points
                               x=lepc1, y=lepc2,
                               type = "scatter", mode = "markers",
                               color=ptColor,
                               colors = mycolors,
                               size=8,
                               text=as.character(Ys_df[[1]]),
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
                 ggplot2::ggplot(data=df, aes(x=PC, y=VarExp)) +
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
                             colors = mycolors,
                             size=8,
                             text=as.character(Ys_df[[1]]),
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
        pre <- strsplit(k,"_")[[1]][1]
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
      volumes <- c("UserFolder"=fs::path_home())
      shinyFileSave(input, "PCAScoresSave", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$PCAScoresSave)
      if (nrow(fileinfo) > 0) {
        scores<-lePCA$x[,1:input$NPCsforPCA]
        row.names(scores)=Ys_df[,1]  #Put sample IDs as row names.
        #Define column names
        shortNames <- lapply(strsplit(input$XsforPCA,"_"), function(x) x[1])
        pre <- paste(unlist(shortNames),collapse="_")
        colonnes <- paste(pre,paste0("PC",1:input$NPCsforPCA),sep="_")
        utils::write.table(scores,file=fileinfo$datapath,sep="\t")
      }
    })
    
    
    
    # *********************************************************************
    ## Reacts to Save model ----
    observe({
      volumes <- c("UserFolder"=fs::path_home())
      shinyFileSave(input, "PCAModelSave", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$PCAModelSave)
      if (nrow(fileinfo) > 0){
        nom_lesX <- input$XsforPCA
        shortNames <- sapply(strsplit(nom_lesX,'_'),"[[",1)
        model_descript=list(type="PCA",
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
        
        toRemove <- setdiff(PP_params$lesNoms,input$XsforPCA)
        removeInd <- unlist(lapply(toRemove, function(x) which(x==PP_params$lesNoms)))
        PP_params$lesNoms <- as.list(input$XsforPCA)
        i <- 0
        lapply(toRemove, function(id){
          i <<- i+1
          PP_params$trunc_limits <<- PP_params$trunc_limits[-removeInd[i],]
          PP_params$perSpecParams[[id]] <<- NULL
          PP_params$savgolParams$doSavGol[[id]] <<- NULL
          PP_params$savgolParams$w[[id]] <<- NULL
          PP_params$savgolParams$p[[id]] <<- NULL
          PP_params$savgolParams$m[[id]] <<- NULL
        })
        NCPs <- input$NPCsforPCA
        save(model_descript,PP_params,lePCA,NCPs,dds,colorby,
             file=fileinfo$datapath)
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
      Xs <- input$XsForPLSDA
      
      if (input$AggregOpForPLSDA=="concatenate"){
        #ATTN : do not work with XData_p but with the selected items.
        for (k in Xs){
          spdf<-as.data.frame(XData_p[[k]][-1,-1])
          pre <- strsplit(k,"_")[[1]][1]
          colnames(spdf)<-paste(pre,as.character(XData_p[[k]][1,-1]),sep="_")
          Ys <- cbind(Ys,spdf)
        }
        plsda_set <<- list(Ys)
      }else
      {
        plsda_set <<- lapply(as.list(Xs), function(ii){
          y<-data.frame(Cl1=Ys ,XData_p[[ii]][-1,-1])
          colnames(y)[-1]<-as.character(XData_p[[ii]][1,-1])
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
      output$PLSDAConsole <- renderPrint({
        dum <- "COMPUTING MODEL - WAIT..."
        write(dum, file="")
      })
      
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
      plsda_txt_output<<-lapply(dumind,function(x) utils::capture.output(print(plsdaFit[[x]])))
      if (input$AggregOpForPLSDA=="concatenate")
        lesnoms=as.list(paste(lesnoms,collapse=" + "))
      for (k in 1:length(plsdaFit)) plsda_txt_output[[k]][1]<<-paste("\n******************\nPLSDA on ",
                                                                     lesnoms[[k]], sep = "")
      if (N_modeles>1){
        FUNC<-input$AggregOpForPLSDA 
        plsda_txt_output <<- c(plsda_txt_output,
                               paste("*******\nConfusion matrix on training data for models aggregated with ",FUNC,
                                     ".\n",sep=""),
                               utils::capture.output(print(val_confusionmat)))
        plsda_txt_output <<- c(plsda_txt_output,
                               paste("*******\nConfusion matrix on test data for models aggregated with ",FUNC,
                                     ".\n",sep=""),
                               utils::capture.output(print(test_confusionmat)))
      }else
      {  plsda_txt_output <<- c(plsda_txt_output,
                                paste("*******\nConfusion matrix on training data for model on ",lesnoms[[1]],
                                      ".\n",sep=""),
                                utils::capture.output(print(val_confusionmat)))
      plsda_txt_output <<- c(plsda_txt_output,
                             paste("*******\nConfusion matrix on test data for model on ",lesnoms[[1]],
                                   ".\n",sep=""),
                             utils::capture.output(print(test_confusionmat)))
      }
      
      output$PLSDAConsole <- renderPrint({
        dum <- unlist(plsda_txt_output)
        write(dum, file="")
      })
      
      #Plot
      nom_lesX <- input$XsForPLSDA
      accs <- lapply(plsdaFit,function(p) p$results$Accuracy)
      accsSD <- lapply(plsdaFit,function(p) p$results$AccuracySD)
      N <- length(accs[[1]])
      grp <- rep(unlist(nom_lesX),each=N)
      if (input$AggregOpForPLSDA == "concatenate")
        grp <- rep(paste(nom_lesX,collapse=" + "),N*length(accs))
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
          nom_lesX <- input$XsForPLSDA
          accs <- lapply(plsdaFit,function(p) p$results$Accuracy)
          accsSD <- lapply(plsdaFit,function(p) p$results$AccuracySD)
          N <- length(accs[[1]])
          grp <- rep(unlist(nom_lesX),each=N)
          if (input$AggregOpForPLSDA == "concatenate")
            grp <- rep(paste(nom_lesX,collapse=" + "),N*length(accs))
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
            nom_lesX <- as.list(input$XsForPLSDA)
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
    observeEvent(input$savePLSDAPreds ,{
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
      outfile=choose.files(caption="Define file name",
                           multi=FALSE, 
                           filters=Filters[c("txt"),])
      utils::write.table(pr,
                         file=outfile,
                         sep="\t",
                         dec=".",
                         row.names = FALSE,
                         quote=FALSE)
      
    })
    
    # *********************************************************************
    
    ## Reacts to save PLSDA model button ----
    observe({
      volumes <- c("UserFolder"=fs::path_home())
      shinyFileSave(input, "FSavePLSDA", roots=volumes, session=session)
      fileinfo <- parseSavePath(volumes, input$FSavePLSDA)
      if (nrow(fileinfo) > 0) {
        leFichier <- fileinfo$datapath
        PP_params <- collectPreProParams(PPvaluesTrunc,input)
        
        #Need to remove some as not all XDataList members are in XsForPLS
        toRemove <- setdiff(PP_params$lesNoms,input$XsForPLSDA)
        removeInd <- unlist(lapply(toRemove, function(x) which(x==PP_params$lesNoms)))
        PP_params$lesNoms <- as.list(input$XsForPLSDA)
        i <- 0
        lapply(toRemove, function(id){
          i <<- i+1
          PP_params$trunc_limits <<- PP_params$trunc_limits[-removeInd[i],]
          PP_params$perSpecParams[[id]] <<- NULL
          PP_params$savgolParams$doSavGol[[id]] <<- NULL
          PP_params$savgolParams$w[[id]] <<- NULL
          PP_params$savgolParams$p[[id]] <<- NULL
          PP_params$savgolParams$m[[id]] <<- NULL
        })
        model_descript <- list(
          type = "PLSDA",
          description = input$PLSDADescript,
          datatype = input$XsForPLSDA,
          aggregation = input$AggregOpForPLSDA
        )
        pls_ncomp <- lapply(plsdaFit,function(x) x$bestTune$ncomp)
        
        save(model_descript,PP_params,plsFit,pls_ncomp,file=leFichier) 
      }
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
      }
      output$modelDescOnApply <- renderText("No model selected")
      
      shinyjs::hide("applyModel")
      shinyjs::hide("saveModelResults")
      
    })
    
})
      
    
    
