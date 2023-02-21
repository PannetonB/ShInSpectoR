#
#Server for ShInSpectoR

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
    
    # *********************************************************************
    
    # Defines table contents as reactive and create proxys ----
    ## On Data tab ----
    Yvalues <- reactiveValues(dfWorking = Ys_df)
    output$Ys <- renderDataTable({
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
            dfs_plotly <- dfForPlotly(XDataList,All_XData_p,s)
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
        dum <- input$npcs
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
        #Load Y data
        indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
        Ys_df <<- read.table(file=inFile$datapath[indi],header=TRUE,sep="\t",dec=".",
                             na.strings = "", stringsAsFactors = T)
        #make.unique to deal with repeated sample ID.
        Ys_df[,1] <- as.factor(make.unique(as.character(Ys_df[,1])))
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
        updateSliderInput(session,"npcs",max=maxnCP,value=5)
        
        #By default, remove Rayleigh and do norm by closure for fluorescence
        #for data visualisation
        lesChoix <- computePCAonRaw(input$npcs,doRayleigh=TRUE)
        
        #Populate Xs file selection and select first by default
        updateSelectInput(session, "Xs",               
                          choices=inFile$name[indi],
                          selected=inFile$name[indi][1])
        ALLXDataList <<- inFile$name[indi]
        
        #Populate PCA data selection
        updateSelectInput(session,"PCA_data",
                          choices=inFile$name[indi],
                          selected=inFile$name[indi][1])
        
        #Populate select CPs to plot
        updateSelectInput(session,"pc1",
                          choices = lesChoix,
                          selected="SDist")
        updateSelectInput(session,"pc2",
                          choices = lesChoix,
                          selected="ODist")
        updateSelectInput(session,"pcaPtColorBy",
                          choices=names(Filter(is.factor,Ys_df))[-1])
        
        
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
       
       lesChoix <- computePCAonRaw(input$npcs,doRayleigh=FALSE)
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
            #                                      min(All_XData_p[[iii]][1,-1]))),
            LowerLimit = unlist(RayleighCutoffs[XDataList]),
            HigherLimit = unlist(lapply(as.list(XDataList), function(iii)
              max(All_XData[[iii]][1,-1])))
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
                              h3('      '),
                              checkboxInput(paste0(id,"_D"), strong('SAVITSKY-GOLAY'), FALSE, width='20px'),
                              numericInput(paste0(id,'_E'), 'Bandwidth',5,5,25,2),
                              numericInput(paste0(id,'_F'), 'Polynomial order',3,2,10,1),
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
            
            for (k in 1:length(All_XData_p)){
              All_XData_p[[k]] <<- All_XData_p[[k]][-(1+lesRows),]
            }
            lesChoix <- computePCAonRaw(input$npcs,doRayleigh=FALSE)
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
            if (!is.null(All_XData_p[[leNom]])) All_XData_p[[leNom]] <<- All_XData[[leNom]][,1+c(0,i1:i2)]
        }
        
        
        lesChoix <- computePCAonRaw(input$npcs,doRayleigh=FALSE)
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
    observeEvent(input$PreProsTrunc_cell_edit, isolate({
      row  <- input$PreProsTrunc_cell_edit$row
      clmn <- input$PreProsTrunc_cell_edit$col+1
      laVal <- as.numeric(input$PreProsTrunc_cell_edit$value)
      PPvaluesTrunc$dfWorking$x$data[row, clmn] <- laVal
      lowWL <- PPvaluesTrunc$dfWorking$x$data[row, 2]
      hiWL <- PPvaluesTrunc$dfWorking$x$data[row, 3]
      id <- paste0(inserted_perSpectrumOptions()[row],"_C")
      WBand <- input[[id]]
      ctr <- round((hiWL+lowWL)/2)
      #Update values for ctr and waveband
      id <- paste0(inserted_perSpectrumOptions()[row],"_B")
      updateNumericInput(session,id,value=ctr,min=lowWL+WBand,max=hiWL-WBand)

    }))
    
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
    
    ## Dummy observe to refresh per spectrum and SavGol inputs ----
    observe({
      outtxt <- character()
      for (k in inserted_perSpectrumOptions()){
        outtxt <- paste0(outtxt,k,": ")
        for (j in LETTERS[1:7])
          outtxt <- paste(outtxt,
                          input[[paste(k,j,sep="_")]])
        outtxt <- paste0(outtxt,"\n")
      }
      output$feedback<-renderText(outtxt)
    })
    
    # *********************************************************************
    
    # On PLS tab----
    # ## Init plot and console areas ----
    # output$PLSPlotID <-   renderText("PLOT TYPE ID")
    # output$PLSPlots <- renderPlotly({
    #   text = "PLOT OUTPUT AREA"
    #   ggplotly(
    #     ggplot() + 
    #       annotate("text", x = 4, y = 25, size=8, label = text) + 
    #       theme_void()
    #   )
    # })
    # 
    # 
    # output$PLSConsole <- renderPrint({
    #   dum <- "Console output area"
    #   write(dum, file="")
    # })
    
    # *********************************************************************
    
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
          updateNumericInput(session,"NbLVForPLS",max=min(c((nSamples-1),20)))
          lesChoix <- names(Filter(function(x) is.factor(x),Ys_df))
          updateSelectInput(session,"PLSPredPlotColorBy",
                            choices = lesChoix ,
                            selected = lesChoix[1])
          
          updateSelectInput(session,"PLSPredPlotLabel",
                            choices = lesChoix ,
                            selected = lesChoix[1])
          updateSelectInput(session,"PLSScorePlotColorBy",
                            choices = lesChoix ,
                            selected = lesChoix[1])
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
    })
    
    # *********************************************************************
    
    ## Verify in nb LV is within limits ----
    observeEvent(input$NbLVForPLS,{
      nbLVs <- input$NbLVForPLS
      if (!is.null(nbLVs) & !is.na(nbLVs)){
        nbSamples <- nrow(Ys_df)
        maxi <- min(c(20,(nbSamples-1)))
        if (nbLVs>maxi & maxi>0) {
         isolate({
           updateNumericInput(session,"NbLVForPLS",value=maxi)
           updateNumericInput(session,"NbLVPLS_Sel", max = maxi)
         })
         
        }
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
      nbLV <- input$NbLVForPLS
      aggr <- input$AggregateForPLS
      Y <- Ys_df[[YName]]
      XNames <- as.list(input$XsForPLS)
      valid <- input$ResamplingForPLS
      
      if (aggr=="Concatenate spectra"){
        y <- data.frame(V1=Ys_df[[YName]])
        for (k in XNames){
          spdf<-as.data.frame(All_XData_p[[k]][-1,-1])
          pre <- strsplit(k,"_")[[1]][1]
          colnames(spdf)<-paste(pre,as.character(All_XData_p[[k]][1,-1]),sep="_")
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
      
    }, ignoreInit = T)
    
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
    observeEvent(input$PlotPLSPred,
                 whichPLSPlot <<- "Pred"
    )
    ## Reacts to plot predictions ----
    #Retrieve options (factor to apply color or to label)
    observe({
      input$PlotPLSPred
      input$NbLVPLS_Sel
      input$PredPlotTypePLS
      colBy <- input$PLSPredPlotColorBy
      labelWith <- input$PLSPredPlotLabel
      if (whichPLSPlot=="Pred")
        isolate({
          nc=input$NbLVPLS_Sel
          leType <- input$PredPlotTypePLS
          output$PLSPlotID <-   renderText(paste0("PREDICTION - ",leType))
          pl<-plot(plsFit[[1]],plottype = "prediction",
                             ncomp=nc,
                             which=leType, type="n")
          dev.off(dev.list()["RStudioGD"])
          
          pl <- as.data.frame(pl)
          
          mycolors <- colorRampPalette(mesCouleurs)(length(mesCouleurs))
          
          output$PLSPlots <-   renderPlotly({
            nc=input$NbLVPLS_Sel
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
            nc=input$NbLVPLS_Sel
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
    observeEvent(input$PLSScorePlot ,{
      whichPLSPlot <<- "Score"
      output$PLSPlotID <-   renderText("Score plot")
      dats <- scores(plsFit[[1]])
      pl <- data.frame(x=dats[,input$PLSScorePlotFirstLV], y=dats[,input$PLSScorePlotSecondLV])
      mycolors <- colorRampPalette(mesCouleurs)(length(mesCouleurs))

      output$PLSPlots <-   renderPlotly({
        colBy <- input$PLSScorePlotColorBy
        plotly::plot_ly() %>%
          add_markers(data=pl,          #Plot all points
                      x=as.formula("~x"),
                      y=as.formula("~y"),
                      type = "scatter", mode = "markers",
                      color=as.character(Ys_df[[colBy]]),
                      colors = mycolors,
                      size=8,
                      text=as.character(Ys_df[,1]),
                      hovertext=as.character(Ys_df[,1]),
                      hovertemplate = paste('EchID: %{text}'))
      })
    })
    
    # *********************************************************************
    
    ## Reacts to ShowPLSPredTable button
    observeEvent(input$ShowPLSPredTable,{
    
      pl<-plot(plsFit[[1]],plottype = "prediction",
               ncomp=input$NbLVPLS_Sel,
               which="train")
      dev.off(dev.list()["RStudioGD"])
      dat_tbl=as.data.frame(pl)
      pl<-plot(plsFit[[1]],plottype = "prediction",
               ncomp=input$NbLVPLS_Sel,
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
               ncomp=input$NbLVPLS_Sel,
               which="train")
      dev.off(dev.list()["RStudioGD"])
      dat_tbl=as.data.frame(pl)
      pl<-plot(plsFit[[1]],plottype = "prediction",
               ncomp=input$NbLVPLS_Sel,
               which="validation")
      dat_tbl=cbind(dat_tbl,as.data.frame(pl)[,2])
      dat_tbl=cbind(Ys_df[,1],dat_tbl,NoSeq=seq_len(nrow(dat_tbl)))
      colnames(dat_tbl)[c(1,3,4)]=c(colnames(Ys_df)[1],
                                    "Training","Validation")
      
      outfile=choose.files(caption="Define file name",
                           multi=FALSE, 
                           filters=Filters[c("txt")])
      utils::write.table(dat_tbl,
                         file=outfile,
                         sep="\t",
                         dec=".",
                         row.names = FALSE,
                         quote=FALSE)
    })

})
