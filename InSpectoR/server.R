#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    
    jqui_draggable('#modalExample')
    
    Yvalues <- reactiveValues(dfWorking = Ys_df)
    PPvaluesTrunc <- reactiveValues(dfWorking = data.frame())
    PPvaluesPerSpectra <- reactiveValues(dfWorking = data.frame())
    PPvaluesSavgol <- reactiveValues(dfWorking = data.frame())
    
    
    output$Ys <- renderDataTable({
        Yvalues$dfWorking
    })
    
    
    
    output$PreProsTrunc <- renderDataTable({
        PPvaluesTrunc$dfWorking
    })

    
    
    output$PreProsPerSpectra <- renderDataTable({
        PPvaluesPerSpectra$dfWorking
    })
    
    output$PreProsSavgol <- renderDataTable({
        PPvaluesSavgol$dfWorking
    })
    
    proxy_Ys = dataTableProxy('Ys')
    proxy_PreProTrunc = dataTableProxy('PreProsTrunc')
    proxy_PreProsPerSpectra = dataTableProxy('PreProsPerSpectra')
    proxy_PreProsSavgol = dataTableProxy('PreProsSavgol')
    
    output$yFileName <- renderText({
            inFile <- input$files
            if (is.null(inFile)){
                return("None selected")
            }
            indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
            inFile$name[indi]
        })
    
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
                       legend=list(title=list(text='<b>Type de donnéesÉchantillon - Échantillon</b>'),
                    #  x=0.75, y=0.98),
                       xaxis = list(title = 'Wavelength [nm] or Wavenumber[cm<sup>-1</sup>]'), 
                       yaxis = list(title = 'Spectra [A.U.]')))
            
        }
    })
    
    output$acpPlots <- renderPlotly({
        s <- unique(input$Ys_rows_selected)
        if (length(s)) {
            s <- sort(s)
            lePCA <- input$PCA_data
            lepc1 <- as.formula(paste0("~",input$pc1))
            lepc2 <- as.formula(paste0("~",input$pc2))
            ptColor <- as.formula(paste0("~",input$pcaPtColorBy))
            dfsPCA <- as.data.frame(PCAs[[lePCA]]$x)
            dfsPCA <- cbind(dfsPCA,Ys_df[input$pcaPtColorBy])
            dfsPCA_unsel <- dfsPCA[-s,]
            dfsPCA_sel <- dfsPCA[s,]
            
            # nb.cols <- length(levels(Ys_df[input$pcaPtColorBy][,1]))
            # mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(nb.cols)
            mycolors <- colorRampPalette(mesCouleurs)(length(mesCouleurs))
            
            t <- list(
                family = "sans serif",
                size = 14,
                color = toRGB("red"))
            
            
            p <- plotly::plot_ly(source="pcaPlot"
                                 ) %>%
                add_markers(data=dfsPCA,
                            x=lepc1, y=lepc2,
                            type = "scatter", mode = "markers",
                            color=ptColor,
                            colors = mycolors,
                            size=8,
                            text=as.character(Ys_df[[1]]),
                            hovertext=as.character(Ys_df[,input$pcaPtColorBy]),
                            hovertemplate = paste('EchID: %{text}')) %>%
                add_markers(data=dfsPCA_sel,
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
                add_text(data=dfsPCA_sel,
                         x=lepc1,y=lepc2,
                         text = paste0("  ",rownames(dfsPCA_sel)),
                         textfont = t, textposition = "top right",
                         showlegend=FALSE) %>% 
                layout(dragmode = "select") %>%
                event_register("plotly_click") %>%
                event_register("plotly_brushing") %>%
                event_register("plotly_doubleclick")
            
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
            plotly_empty(type='scatter',mode="markers",source="pcaPlot") %>% 
            event_register("plotly_click") %>%
            event_register("plotly_brushing") %>%
            event_register("plotly_selected")
    })
    
    output$loadingPlots <- renderPlotly({
        if (all(stringr::str_detect(c(input$pc1,input$pc2),"PC"))){
            lePCA <- PCAs[[input$PCA_data]]
            whichData <- which(input$PCA_data==names(PCAs))
            unPCA <<- lePCA
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
    
   
   
     
    observe({ toggle(id="plotloadings", 
                     condition=all(stringr::str_detect(c(input$pc1,input$pc2),"PC")))})
    
    
    observeEvent(input$files, {
        k=1
        inFile <- input$files
        if (is.null(inFile))
            return(NULL)
        #Load Y data
        indi <- which(stringr::str_detect(inFile$name,glob2rx("Y_*.txt")))
        Ys_df <<- read.table(file=inFile$datapath[indi],header=TRUE,sep="\t",dec=".",
                             na.strings = "", stringsAsFactors = T)
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
                    # #normalize matrices by closure by default for PCA
                    # dats <- dum1[-1,-1]
                    # L <- ncol(dats)
                    # dats <- t(apply(dats,1,function(z) z*L/sum(z))) 
                    # #Compute PCA on normalized spectra
                    # nCP <- input$npcs #hard limit to 15 CPs
                    # pcdum <- prcomp(dats, rank=nCP)
                    # #Compute in-model and perpendicular distances
                    # pca2<-pr_2_prin(pcdum)
                    # dd <- chemometrics::pcaDiagplot(dats,pca2,a=nCP,
                    #                                 plot=FALSE,scale=FALSE)
                    # PCAs_dds[[inFile$name[ii]]] <<- cbind(dd$SDist,dd$ODist)
                    # PCAs_dds_crit[[inFile$name[ii]]] <<- c(dd$critSD,dd$critOD)
                    # 
                    # #Finalise PCA scores matrix
                    # rownames(pcdum$x) <- dum1[-1,1]
                    # #Add SDist and ODist columns
                    # pcdum$x <- cbind(pcdum$x,dd$SDist,dd$ODist)
                    # colnames(pcdum$x) <- c(paste0("PC",1:nCP),
                    #                        "SDist","ODist")
                    # rownames(pcdum$rotation) <- dum1[1,-1]
                    # colnames(pcdum$rotation) <- dum1[-1,1][1:nCP]
                    # PCAs[[inFile$name[ii]]] <<- pcdum
                }
            }
        }
        ORI_XData <<- All_XData
        
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
            
            #Truncation
            inTrunc <- dats[1,-1]>wvDip
            dum <- All_XData[[jj]][,c(TRUE,inTrunc)]
            
            #normalize matrices by closure by default for PCA
            dats <- dum[-1,-1]
            L <- ncol(dats)
            dats <- t(apply(dats,1,function(z) z*L/sum(z)))
            dum[-1,-1] <- dats
            All_XData_p[[leNom]] <<-dum  
            
            #dats <- dum[-1,-1]
            L <- ncol(dats)
            dats <- t(apply(dats,1,function(z) z*L/sum(z))) 
            #Compute PCA on normalized spectra
            nCP <- input$npcs #hard limit to 15 CPs
            pcdum <- prcomp(dats, rank=nCP)
            #Compute in-model and perpendicular distances
            pca2<-pr_2_prin(pcdum)
            dd <- chemometrics::pcaDiagplot(dats,pca2,a=nCP,
                                            plot=FALSE,scale=FALSE)
            PCAs_dds[[leNom]] <<- cbind(dd$SDist,dd$ODist)
            PCAs_dds_crit[[leNom]] <<- c(dd$critSD,dd$critOD)
            
            #Finalise PCA scores matrix
            rownames(pcdum$x) <- dum1[-1,1]
            #Add SDist and ODist columns
            pcdum$x <- cbind(pcdum$x,dd$SDist,dd$ODist)
            colnames(pcdum$x) <- c(paste0("PC",1:nCP),
                                   "SDist","ODist")
            rownames(pcdum$rotation) <- dum[1,-1]
            colnames(pcdum$rotation) <- dum[-1,1][1:nCP]
            PCAs[[leNom]] <<- pcdum
          }
          else{
            All_XData_p[[leNom]] <<- All_XData[[leNom]]
          }
        }
        #All_XData_p <<- All_XData
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
        leschoix <- colnames(pcdum$x)
        updateSelectInput(session,"pc1",
                          choices = leschoix,
                          selected="SDist")
        updateSelectInput(session,"pc2",
                          choices = leschoix,
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
        
        #Tables of PrePros
        XDataList <<- inFile$name[indi][1]
        nX <- length(XDataList)
        #Truncation
        truncDF <- data.frame(
            Spectra = XDataList,
            LowerLimit = min(All_XData[[1]][1,-1]),
            HigherLimit = max(All_XData[[1]][1,-1])
        )
        
        dtable <- datatable(truncDF,rownames = F, width='800px',
                            options = list(dom = 't',
                                           scrollX=TRUE),
                            editable=TRUE
                            
        )
        
        
        PPvaluesTrunc$dfWorking <- dtable
        
        },
        ignoreInit = T
    )
    
    observeEvent(input$PreProsTrunc_cell_edit, {
      print("Bingo")
      row  <- input$PreProsTrunc_cell_edit$row
      clmn <- input$PreProsTrunc_cell_edit$col
      PPvaluesTrunc$dfWorking$x$data[row, clmn] <- input$PreProsTrunc_cell_edit$value
    })
    
    observeEvent(input$npcs,{
        for (k in 1:length(All_XData_p)){
            dats <- All_XData_p[[k]][-1,-1]
            #normalize matrices by closure by default
            L <- ncol(dats)
            dats <- t(apply(dats,1,function(z) z*L/sum(z))) 
            #Compute PCA on normalized spectra
            nCP <- input$npcs #hard limit to 15 CPs
            pcdum <- prcomp(dats, rank=nCP)
            #Compute in-model and perpendicular distances
            pca2<-pr_2_prin(pcdum)
            dd <- chemometrics::pcaDiagplot(dats,pca2,a=nCP,
                                            plot=FALSE,scale=FALSE)
            PCAs_dds[[names(All_XData_p)[k]]] <<- cbind(dd$SDist,dd$ODist)
            PCAs_dds_crit[[names(All_XData_p)[k]]] <<- c(dd$critSD,dd$critOD)
            
            #Finalise PCA scores matrix
            rownames(pcdum$x) <- All_XData_p[[k]][-1,1]
            #Add SDist and ODist columns
            pcdum$x <- cbind(pcdum$x,dd$SDist,dd$ODist)
            colnames(pcdum$x) <- c(paste0("PC",1:nCP),
                                   "SDist","ODist")
            rownames(pcdum$rotation) <- All_XData_p[[k]][1,-1]
            colnames(pcdum$rotation) <- All_XData_p[[k]][-1,1][1:nCP]
            PCAs[[names(All_XData_p)[k]]] <<- pcdum
        }
        #Populate select CPs to plot
        leschoix <- colnames(pcdum$x)
        updateSelectInput(session,"pc1",
                          choices = leschoix,
                          selected="SDist")
        updateSelectInput(session,"pc2",
                          choices = leschoix,
                          selected="ODist")
        proxy_Ys %>% selectRows(NULL)
    }, ignoreInit = T)
    
    observe({
        XDataList <<- input$Xs
        
        #Update prepro tables 
        #Truncation
        truncDF <- data.frame(
          Spectra = XDataList,
          LowerLimit = unlist(lapply(as.list(XDataList), function(iii)
                                               min(All_XData[[iii]][1,-1]))),
          HigherLimit = unlist(lapply(as.list(XDataList), function(iii)
            max(All_XData[[iii]][1,-1])))
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
    
    observeEvent(input$clearRows, {
        proxy_Ys %>% selectRows(NULL)
    })
    
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
                dats <- All_XData_p[[k]][-1,-1]
                #normalize matrices by closure by default
                L <- ncol(dats)
                dats <- t(apply(dats,1,function(z) z*L/sum(z))) 
                #Compute PCA on normalized spectra
                nCP <- input$npcs #hard limit to 15 CPs
                pcdum <- prcomp(dats, rank=nCP)
                #Compute in-model and perpendicular distances
                pca2<-pr_2_prin(pcdum)
                dd <- chemometrics::pcaDiagplot(dats,pca2,a=nCP,
                                                plot=FALSE,scale=FALSE)
                PCAs_dds[[names(All_XData)[k]]] <<- cbind(dd$SDist,dd$ODist)
                PCAs_dds_crit[[names(All_XData)[k]]] <<- c(dd$critSD,dd$critOD)
                
                #Finalise PCA scores matrix
                rownames(pcdum$x) <- All_XData[[k]][-1,1]
                #Add SDist and ODist columns
                pcdum$x <- cbind(pcdum$x,dd$SDist,dd$ODist)
                colnames(pcdum$x) <- c(paste0("PC",1:nCP),
                                       "SDist","ODist")
                rownames(pcdum$rotation) <- All_XData[[k]][1,-1]
                colnames(pcdum$rotation) <- All_XData[[k]][-1,1][1:nCP]
                PCAs[[names(All_XData)[k]]] <<- pcdum
            }
        }
        proxy_Ys %>% selectRows(NULL)
    })
    
    
    observeEvent(input$restoreOriData, {
        Ys_df <<- ORI_Ys_df
        dtable <- DT::datatable(Ys_df, width = '900px',
                                options=list(
                                    lengthMenu = list(c(10, 15, 20, -1), c('10', '15', '20','All')),
                                    pageLength = 20,
                                    scrollX = TRUE,
                                    columnDefs = list(list(orderable = TRUE, targets = 0))
                                ))
        dtable$x$data[[1]] <- as.numeric(dtable$x$data[[1]])-1
        Yvalues$dfWorking <- dtable
        
        All_XData <<- ORI_XData
        for (k in 1:length(All_XData)){
            dats <- All_XData[[k]][-1,-1]
            #normalize matrices by closure by default
            L <- ncol(dats)
            dats <- t(apply(dats,1,function(z) z*L/sum(z)))
            #Compute PCA on normalized spectra
            nCP <- input$npcs 
            pcdum <- prcomp(dats, rank=nCP)
            #Compute in-model and perpendicular distances
            pca2<-pr_2_prin(pcdum)
            dd <- chemometrics::pcaDiagplot(dats,pca2,a=nCP,
                                            plot=FALSE,scale=FALSE)
            PCAs_dds[[names(All_XData)[k]]] <<- cbind(dd$SDist,dd$ODist)
            PCAs_dds_crit[[names(All_XData)[k]]] <<- c(dd$critSD,dd$critOD)

            #Finalise PCA scores matrix
            rownames(pcdum$x) <- All_XData[[k]][-1,1]
            #Add SDist and ODist columns
            pcdum$x <- cbind(pcdum$x,dd$SDist,dd$ODist)
            colnames(pcdum$x) <- c(paste0("PC",1:nCP),
                                   "SDist","ODist")
            rownames(pcdum$rotation) <- All_XData[[k]][1,-1]
            colnames(pcdum$rotation) <- All_XData[[k]][-1,1][1:nCP]
            PCAs[[names(All_XData)[k]]] <<- pcdum
        }
        proxy_Ys %>% selectRows(NULL)
        
    })
    
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
    
    

})
