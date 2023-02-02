#
#Server for ShInSpectoR



shinyServer(function(input, output, session) {

    # To make modal window for loading plots draggable ----
    jqui_draggable('#modalExample')
    
    
    # Defines table contents as reactive and create proxys ----
    ## On Data tab ----
    Yvalues <- reactiveValues(dfWorking = Ys_df)
    output$Ys <- renderDataTable({
      Yvalues$dfWorking
    })
    proxy_Ys = dataTableProxy('Ys')
    
    ## On Prepro tab ----
    PPvaluesTrunc <- reactiveValues(dfWorking = data.frame())
    PPvaluesPerSpectra <- reactiveValues(dfWorking = data.frame())
    PPvaluesSavgol <- reactiveValues(dfWorking = data.frame())
    
    output$PreProsTrunc <- renderDataTable({
        PPvaluesTrunc$dfWorking
    })

    output$PreProsPerSpectra <- renderDataTable({
        PPvaluesPerSpectra$dfWorking
    })
    
    output$PreProsSavgol <- renderDataTable({
        PPvaluesSavgol$dfWorking
    })
    
   
    proxy_PreProTrunc = dataTableProxy('PreProsTrunc')
    proxy_PreProsPerSpectra = dataTableProxy('PreProsPerSpectra')
    proxy_PreProsSavgol = dataTableProxy('PreProsSavgol')
    
    # On Data tab ----
    ## Shows Y filename. Triggered by input$files ----
    output$yFileName <- renderText({
            inFile <- input$files
            if (is.null(inFile)){
                return("None selected")
            }
            indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
            inFile$name[indi]
        })
    
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
    
    ## Plot PCA ----
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
        dfsPCA <- as.data.frame(PCAs[[lePCA]]$x)
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
                   SDist = p %>% layout(shapes = list(vline(PCAs_dds_crit[[lePCA]][1]))),
                   ODist = p %>% layout(shapes = list(vline(PCAs_dds_crit[[lePCA]][2]))),
                   p
            )
            p <- switch(input$pc2,
                    SDist = p %>% layout(shapes = list(hline(PCAs_dds_crit[[lePCA]][1]))),
                    ODist = p %>% layout(shapes = list(hline(PCAs_dds_crit[[lePCA]][2]))),
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
                      SDist = p %>% layout(shapes = list(vline(PCAs_dds_crit[[lePCA]][1]))),
                      ODist = p %>% layout(shapes = list(vline(PCAs_dds_crit[[lePCA]][2]))),
                      p
          )
          p <- switch(input$pc2,
                      SDist = p %>% layout(shapes = list(hline(PCAs_dds_crit[[lePCA]][1]))),
                      ODist = p %>% layout(shapes = list(hline(PCAs_dds_crit[[lePCA]][2]))),
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
    
    ## Plot loadings in a modal window ----
    output$loadingPlots <- renderPlotly({
        if (all(stringr::str_detect(c(input$pc1,input$pc2),"PC"))){
            lePCA <- PCAs[[input$PCA_data]]
            whichData <- which(input$PCA_data==names(PCAs))
            colPC1 <- which(colnames(lePCA$x)==input$pc1)
            colPC2 <- which(colnames(lePCA$x)==input$pc2)
            ys1 <- lePCA$rotation[,colPC1]  #First PC loadings
            ys2 <-lePCA$rotation[,colPC2]  #Second PC loadings
            xs <- as.numeric(All_XData_p[[whichData]][1,-1])
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
    
   
   
    ## Toggles Plot Loadings action button ---- 
    # Both pc1 and pc2 inputs must be a principal components for button to show
    observe({ toggle(id="plotloadings", 
                     condition=all(stringr::str_detect(c(input$pc1,input$pc2),"PC")))})
    
    
    ## Loads data ----
    observeEvent(input$files, {
        k=1
        inFile <- input$files
        if (is.null(inFile))
            return(NULL)
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
                    All_XData[[inFile$name[ii]]] <<- dum1
                }
            }
        }
        ORI_XData <<- All_XData
        
        #Sets max nb of pcs to 15 or the number of samples-1 if smaller
        nSamples <- nrow(All_XData[[1]])-1
        maxnCP <- ifelse (nSamples < 21, nSamples-1, 20 )
        updateSliderInput(session,"npcs",max=maxnCP,value=5)
        
        #By default, remove Rayleigh and do norm by closure for fluorescence
        #for data visualisation
        for (jj in 1:length(All_XData)){
          dats <- All_XData[[jj]]
          
          #First find cutoff for Rayleigh
          #Find excitation wavelength
          leNom <- inFile$name[indi[[jj]]]
          # Check if this is fluorescence
          isFluo <- substr(leNom,1,2)=="EX"
          if (isFluo){
            #Find excitation wavelength
            EXwv <- strsplit(leNom,"_")[[1]][1]
            #ATTN - marche si longueur d'onde d'excitation a 3 chiffres.
            EXwv <- as.numeric(substr(EXwv,start=3,stop=5))
            
            #Find local min between EXwv and EXwv+50
            #First do some smoothing
            #Isolate data
            ind1 <- dats[1,-1] >=EXwv
            ind2 <- dats[1,-1] <= (EXwv+50)
            myarray <- dats[-1,c(FALSE,(ind1 & ind2))]
            #Smooth per line
            myarray <- apply(myarray,2,function(x) smooth.spline(x)$y)
            infl <- apply(myarray,2,function(x) c(FALSE,diff(diff(x)>0)!=0))
            #Find inflection points (1rst derivative changes sign)
            wvDips <- apply(infl,1, function(x) (EXwv:(EXwv+50))[which(x)[1]])
            #Find average location of dip for all samples.
            wvDip <- round(quantile(wvDips,probs=0.9,na.rm=T))
            #If wvDip too close to EXwv, default do EXwv + 25
            if (wvDip<(EXwv+10)) wvDip <- wvDip+25
            
            #Store to RayleighCutoffs
            RayleighCutoffs[[leNom]] <<- wvDip
            
            #Truncation
            inTrunc <- dats[1,-1]>wvDip
            dum <- All_XData[[jj]][,c(TRUE,inTrunc)]
            dum <- normLigne(dum)
            All_XData_p[[leNom]] <<-dum  
            
            # #Compute PCA on normalized spectra
            lesChoix <- computePCA(input$npcs, dum, leNom)
          }
          else{
            #normalize matrices by closure by default
            dum <- normLigne(All_XData[[leNom]])
            All_XData_p[[leNom]] <<-dum 
            # #Compute PCA on normalized spectra
            lesChoix <- computePCA(input$npcs, dum, leNom)
            #Store dummy to RayleighCutoffs
            RayleighCutoffs[[leNom]] <<- All_XData[[leNom]][1,2]
          }
        }
        
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
        
        },
        ignoreInit = T
    )
    
    
    ## Reacts to number of PCs ----
    observeEvent(input$npcs,{
        for (k in 1:length(All_XData_p)){
          leNom <- names(All_XData_p)[k]
          dum <- All_XData_p[[leNom]]
          dats <- dum[-1,-1]
          # #Compute PCA on normalized spectra
          lesChoix <- computePCA(input$npcs, dum, leNom)
        }
        #Populate select CPs to plot
        
        updateSelectInput(session,"pc1",
                          choices = lesChoix,
                          selected="SDist")
        updateSelectInput(session,"pc2",
                          choices = lesChoix,
                          selected="ODist")
        proxy_Ys %>% selectRows(NULL)
    }, ignoreInit = T)
    
    
    ## Reacts to X selection ----
    observe({
        XDataList <<- input$Xs
        
        
        #Update prepro tables 
        #Truncation
        truncDF <- data.frame(
          Spectra = XDataList,
          # LowerLimit = unlist(lapply(as.list(XDataList), function(iii)
          #                                      min(All_XData_p[[iii]][1,-1]))),
          LowerLimit = unlist(RayleighCutoffs[XDataList]),
          HigherLimit = unlist(lapply(as.list(XDataList), function(iii)
            max(All_XData_p[[iii]][1,-1])))
        )
        
        dtable <- datatable(truncDF,rownames = F, width='800px',
                            options = list(dom = 't',
                                           scrollX=TRUE),
                            editable=TRUE
                            
        )
        
        
        PPvaluesTrunc$dfWorking <- dtable
        #Cancel selections in Ys table
        proxy_Ys %>% selectRows(NULL)
    })
    
    
    ## Reacts to clearRows button ----
    observeEvent(input$clearRows, {
        proxy_Ys %>% selectRows(NULL)
    })
    
    ## Reacts to deleteRows button ----
    observeEvent(input$deleteRows, {
        if (!is.null(input$Ys_rows_selected)) {
            lesRows <- unique(as.numeric(input$Ys_rows_selected))
            #Remove rows in All_XData, .
            #Recompute PCAs and update PCAs, PCAs_dds and PCAs_dds_crit - see lines 180-202 above.
            Yvalues$dfWorking$x$data <- Yvalues$dfWorking$x$data[-lesRows,]
            Ys_df <<- Yvalues$dfWorking$x$data
            for (k in 1:length(All_XData)){
                All_XData[[k]] <<- All_XData[[k]][-(1+lesRows),]
                All_XData_p[[k]] <<- All_XData_p[[k]][-(1+lesRows),]
                
                #COmpute PCA on preprocessed data
                leNom <- names(All_XData_p)[k]
                dum <- All_XData_p[[leNom]]
                dats <- dum[-1,-1]
                # #Compute PCA on normalized spectra
                lesChoix <- computePCA(input$npcs, dum, leNom)
            }
        }
        proxy_Ys %>% selectRows(NULL)
    })
    
    
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
            wv <- All_XData_p[[leNom]][1,-1]
            i1 <- which(All_XData[[leNom]][1,-1]==min(wv))
            i2 <- which(All_XData[[leNom]][1,-1]==max(wv))
            All_XData_p[[leNom]] <<- All_XData[[leNom]][,1+c(0,i1:i2)]
            #COmpute PCA on preprocessed data
            dum <- All_XData_p[[leNom]]
            dum <- normLigne(All_XData_p[[leNom]])
            All_XData_p[[leNom]] <<-dum 
            
            #Compute PCA on normalized spectra
            lesChoix <- computePCA(input$npcs, dum, leNom)
        }
        proxy_Ys %>% selectRows(NULL)
        
        #Populate select CPs to plot
        updateSelectInput(session,"pc1",
                          choices = lesChoix,
                          selected="SDist")
        updateSelectInput(session,"pc2",
                          choices = lesChoix,
                          selected="ODist")
        
    })
    
    ## Reacts to click on pcaPlot ----
    observeEvent(event_data("plotly_click",source ="pcaPlot"),
                 {
                       d <- event_data("plotly_click",source ="pcaPlot")
                       lePCA <- isolate(input$PCA_data)
                       dfsPCA <- as.data.frame(PCAs[[lePCA]]$x)
                       xall <- isolate(as.numeric(dfsPCA[input$pc1][,1]))
                       yall <-isolate( as.numeric(dfsPCA[input$pc2][,1]))
                       laRow <- (myNearPoint(d$x,d$y,xall,yall))
                       choisie <- input$Ys_rows_selected
                       proxy_Ys %>% selectRows(c(choisie,laRow))
                 },
                 ignoreInit = T
                 )
    
    ## Reacts to selection on pcaPlot ----
    observeEvent(event_data("plotly_selected",source ="pcaPlot"),
                 {
                     d <- event_data("plotly_selected",source ="pcaPlot")
                     # d$x and d$y : corners of selected box
                     # so find all points in a box
                     lePCA <- isolate(input$PCA_data)
                     dfsPCA <- as.data.frame(PCAs[[lePCA]]$x)
                     xall <- isolate(as.numeric(dfsPCA[input$pc1][,1]))
                     yall <-isolate( as.numeric(dfsPCA[input$pc2][,1]))
                     sels <<- as.data.frame(cbind(d$x,d$y))
                     refs <<- as.data.frame(cbind(xall,yall))
                     lesRows <- prodlim::row.match(sels,refs)
                     proxy_Ys %>% selectRows(lesRows)
                 },
                 ignoreInit = T
                )
    
    # On PrePro tab ----
    
    ## Reacts to PrePro editing ----
    observeEvent(input$PreProsTrunc_cell_edit, {
      row  <- input$PreProsTrunc_cell_edit$row
      clmn <- input$PreProsTrunc_cell_edit$col+1
      PPvaluesTrunc$dfWorking$x$data[row, clmn] <- input$PreProsTrunc_cell_edit$value
      Apply_PrePro(PPvaluesTrunc)
      #Force redraw
      s <- unique(input$Ys_rows_selected)
      proxy_Ys %>% selectRows(NULL)
      proxy_Ys %>% selectRows(s)
      
    })

})
