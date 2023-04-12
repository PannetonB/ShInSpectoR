
# Global variables ----
## In global environment ----
PCAsDT <<- list()  #PCAs of all spectrum types in XDataList
PCAsdt_dds <<- list()   #Score and Outside distances of PCAs 
PCAsdt_dds_crit <<- list()  #Limits for PCAs_dds
All_XData <<- list()  #Current set of spectra, one type per element of list
XData_p <<- list()  #Current set of preprocessed spectra, one type per element of list
ORI_XData <<- list()  #As in file set of spectra
Ys_df <<- data.frame(ID=character(0))  #Current Ys 
ORI_Ys_df <<- data.frame()   #As in file Ys
XDataList <<- character()   #List of current spectra types
ALLXDataList <<- character() #List of loaded spectra types
RayleighCutoffs <<- list() #where to cut to eliminate Rayleigh in fluorescence
                           #one element per element in All_XData with same names.
whichPLSPlot <<- "none"
whichPLSDAPlot <<- "none"

## In Shiny app ----

mesCouleurs <- paletteer::paletteer_d("Polychrome::palette36")[c(3,5,6,7,10,13,16,30,36)]
mesCouleurs <- paletteer::paletteer_d("ggthemes::Classic_10")

# Functions for the app ----


#***********************************************************************
chargeLibs <- function(lesLibrairies)
{
  i1 <- !(lesLibrairies %in% row.names(installed.packages()))
  if(any(i1)) {
    install.packages(lesLibrairies[i1], dependencies = TRUE) 
  }
  lapply(lesLibrairies, require, character.only = TRUE)
}

#***********************************************************************
getShortType <- function(lestypes)
{
  shortType <- strsplit(lestypes,"_")
  shortType <- sapply(shortType,"[[",1)
  return(shortType)
}



#***********************************************************************

stripPreProNames <- function(PP_params){
  #Strip list item names so that only spectrum type ID is retained.
  #e.g. EX320_bbla_I.txt becomes EX320.
  shortNames <- sapply(strsplit(unlist(PP_params$lesNoms),"_"),"[[",1)
  PP_out <- PP_params
  PP_out$lesNoms <- shortNames
  names(PP_out$perSpecParams) <- shortNames
  names(PP_out$savgolParams$doSavGol) <- shortNames
  names(PP_out$savgolParams$w) <- shortNames
  names(PP_out$savgolParams$m) <- shortNames
  names(PP_out$savgolParams$p) <- shortNames
  rownames(PP_out$trunc_limits) <- shortNames
  return(PP_out)
} 
#***********************************************************************

buildPreProNames <- function(PP_params){
  #Apply spectrum data file names to rename list items
  #for PrePros when loading from file.
  sortedALLXDataList <- sort(ALLXDataList)
  longNames <- sortedALLXDataList
  shortNames <- sapply(strsplit(sortedALLXDataList,"_"),"[[",1)
  longNames <- sortedALLXDataList[shortNames %in% PP_params$lesNoms]
  PP_out <- PP_params
  PP_out$lesNoms <- longNames
  names(PP_out$perSpecParams) <- longNames
  names(PP_out$savgolParams$doSavGol) <- longNames
  names(PP_out$savgolParams$w) <- longNames
  names(PP_out$savgolParams$m) <- longNames
  names(PP_out$savgolParams$p) <- longNames
  rownames(PP_out$trunc_limits) <- longNames
  return(PP_out)
}

#***********************************************************************

collectPreProParams <- function(PPValuesTrunc,input){
  # cat("In collectPreProParams\n")
  #retrieve content of dfWorking data frame in PPValuesTrunc
  #truncParams is a data frame with spectrum type, lower limit and higher limit
  #in column 1 to 3 respectively
  TruncParams <- PPValuesTrunc$dfWorking$x$data
  
  #Truncation limits
  trunc_limits <- TruncParams[,-1]
  lesNoms <- as.list(TruncParams[,1])
  names(trunc_limits) <- c("LowerLimit","HigherLimit","RayleighCutoff")
  
  #Retrieve per spectrum norm parameters 
  perSpecParams <- lapply(as.list(inserted_perSpectrumOptions()), function(opt){
    para <- c(unlist(isolate(input[[paste0(opt,"_A")]])),
              unlist(isolate(input[[paste0(opt,"_B")]])),
              unlist(isolate(input[[paste0(opt,"_C")]]))
    )
  })
  names(perSpecParams) <- lesNoms
  
  #Retrieve per SavGol parameters
  letest <- lapply(as.list(inserted_perSpectrumOptions()), function(opt){
    isolate(input[[paste0(opt,"_D")]])
  })
  names(letest) <- lesNoms
  
  lesW <- lapply(as.list(inserted_perSpectrumOptions()), function(opt){
    isolate(input[[paste0(opt,"_E")]])
  })
  names(lesW) <- lesNoms
  
  lesPoly <- lapply(as.list(inserted_perSpectrumOptions()), function(opt){
    isolate(input[[paste0(opt,"_F")]])
  })
  names(lesPoly) <- lesNoms
  
  lesDeriv <- lapply(as.list(inserted_perSpectrumOptions()), function(opt){
    isolate(input[[paste0(opt,"_G")]])
  })
  names(lesDeriv) <- lesNoms
  savgolParams <- list(doSavGol=letest,w=lesW,p=lesPoly,m=lesDeriv)
  
  
  preproParams <- list(lesNoms=lesNoms,trunc_limits=trunc_limits,perSpecParams=perSpecParams,savgolParams=savgolParams)
  
  return(preproParams)
  
}

#***********************************************************************

prepro_InSp_2_ShiInSp <- function(model_descript,prepro_params)  
{
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
  return(PP_params)
}

#***********************************************************************

prepro_Shin_2_InSp <- function(PP_params)
{
  
  
  #Faire la liste model_descript
  model_descript <- list()
  model_descript$type <- "prepro"
  model_descript$description <- "Créé par ShInSpectoR"
  model_descript$datatype <- PP_params$lesNoms
  NSpect <- length(PP_params$lesNoms)
  
  #Faire la liste prepro_params
  prepro_params <- list()
  trLims <- matrix(as.numeric(unlist(PP_params$trunc_limits[,1:2])),nrow=NSpect,ncol=2)
  prepro_params$trunc_limits <- trLims
  
  scalIndx <- unlist(lapply(PP_params$perSpecParams, function(x) which(c("none", "waveband", "closure")==x[1])))
  names(scalIndx) <- NULL
  prepro_params$byspectra_scaling_index <- scalIndx
  
  c_n_w <- unlist(lapply(PP_params$perSpecParams, function(x) x[2]))
  c_n_w <- c(c_n_w,unlist(lapply(PP_params$perSpecParams, function(x) x[3])))
  c_n_w <- matrix(as.numeric(c_n_w),nrow=NSpect,ncol=2)
  prepro_params$cntr_n_w <- c_n_w
  
  doSGol <- unlist(PP_params$savgolParams$doSavGol)
  names(doSGol)<- NULL
  prepro_params$do_savgol <- doSGol
  
  mSGol <- unlist(PP_params$savgolParams$m)
  names(mSGol)<- NULL
  prepro_params$m <- mSGol
  
  pSGol <- unlist(PP_params$savgolParams$p)
  names(pSGol)<- NULL
  prepro_params$p <- pSGol
  
  wSGol <- unlist(PP_params$savgolParams$w)
  names(wSGol)<- NULL
  prepro_params$w <- wSGol
  
  return(list(model_descript=model_descript,prepro_params=prepro_params))
}

#***********************************************************************

Apply_PrePro <- function(preproParams)
  #Apply all preprocessing as defined in the prepro tab.
  #In preproParams, lesNoms give active spectrum types
{
  #Truncation-----
  # cat("In Apply_Prepro\n")
  
  trunc_limits <- preproParams$trunc_limits
  lesNoms <- preproParams$lesNoms
  XData_p <<- list()
  
  lapply(lesNoms,function(leNom){
    wl<-All_XData[[leNom]][1,-1]
    XData_p[[leNom]] <<- All_XData[[leNom]][,c(TRUE,
                                                   ((wl>=trunc_limits[leNom,1]) & (wl<=trunc_limits[leNom,2])))]
  }) 
  
  #Per spectrum norm ----
  ##Retrieve per spectrum norm parameters ----
  perSpecParams <- preproParams$perSpecParams
  
  lapply(lesNoms, function(leNom){
    typePerSpec <- perSpecParams[[leNom]][1]
    if (length(typePerSpec==1)){
      switch(typePerSpec,
             closure = {
               dum <- normLigne(XData_p[[leNom]])
               XData_p[[leNom]] <<-dum  
             },
             waveband ={
               wl <- XData_p[[leNom]][1,-1]
               ctr <- as.numeric(perSpecParams[[leNom]][2])
               band <- as.numeric(perSpecParams[[leNom]][3])
               halfband <- floor(band/2)
               i1 <- ctr-halfband
               i2 <- ctr+halfband
               i1 <- which.min(abs(i1-wl))
               i2 <- which.min(abs(i2-wl))
               dats <- XData_p[[leNom]][-1,-1]
               L <- ncol(dats)
               dats <- t(apply(dats,1,function(z) z/mean(z[i1:i2])))
               XData_p[[leNom]][-1,-1] <<- dats
             })
    }
  })
  
 
  
  
  if (!any(unlist(lapply(preproParams$savgolParams$w,is.null)))){   #to take of init phase
    lapply(lesNoms, function(leNom){
      if (preproParams$savgolParams$doSavGol[[leNom]]){
        m <- preproParams$savgolParams$m[[leNom]]
        p <- preproParams$savgolParams$p[[leNom]]
        w <- preproParams$savgolParams$w[[leNom]]
        
        #Adjust length because savGolay truncates
        w_2=floor(w/2)
        wl<- XData_p[[leNom]][1,-1] # a list of wl vector, one per spectrum type
        wl <- wl[(w_2+1):(length(wl)-w_2)]
        
        X <- XData_p[[leNom]][-1,-1]
        X <- prospectr::savitzkyGolay(X,m,p,w)
        X <- rbind(wl,X) #rebuild matrices with wl
        XData_p[[leNom]] <<- cbind(XData_p[[leNom]][,1],X)
      }
    })
  }
}

#***********************************************************************

computePCAsDT <- function(nCP,dum, leNom)
  # nCP: number of PC desired
  # dum: an element of XData_p
  # leNom : name of dum
{
  dats <- dum[-1,-1]
  #Compute PCA on normalized spectra
  pcdum <- prcomp(dats, rank=nCP)
  #Compute in-model and perpendicular distances
  pca2<-pr_2_prin(pcdum)
  dd <- chemometrics::pcaDiagplot(dats,pca2,a=nCP,
                                  plot=FALSE,scale=FALSE)
  PCAsdt_dds[[leNom]] <<- cbind(dd$SDist,dd$ODist)
  PCAsdt_dds_crit[[leNom]] <<- c(dd$critSD,dd$critOD)
  
  #Finalise PCA scores matrix
  rownames(pcdum$x) <- dum[-1,1]
  #Add SDist and ODist columns
  pcdum$x <- cbind(pcdum$x,dd$SDist,dd$ODist)
  colnames(pcdum$x) <- c(paste0("PC",1:nCP),
                         "SDist","ODist")
  
  dum[-1,-1] <- dats
  rownames(pcdum$rotation) <- dum[1,-1]
  colnames(pcdum$rotation) <- paste0("PC",(1:nCP))
  PCAsDT[[leNom]] <<- pcdum
  lesChoix <- colnames(pcdum$x)
  return(lesChoix)
}

#***********************************************************************

computePCAonRaw <- function(nCP, doRayleigh=FALSE)
#Compute PCA on all elements of All_XData performing
# truncation of Rayleigh for fluo and normalisation by
# closure for all. This generates PCAsDT (PCA for data tab)  
  # nCP: number of PC desired
  # doRayleigh : when TRUE, calculates Rayleigh cutoffs.
{
  # cat("In computePCAonRaw\n")
  for (jj in 1:length(All_XData)){
    dats <- All_XData[[jj]]
    
    #First find cutoff for Rayleigh
    #Find excitation wavelength
    leNom <- names(All_XData)[jj]
    # Check if this is fluorescence
    isFluo <- substr(leNom,1,2)=="EX"
    if (isFluo){
      if (doRayleigh){
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
        if (nrow(myarray)>3)
          myarray <- apply(myarray,2,function(x) smooth.spline(x)$y)
        infl <- apply(myarray,2,function(x) c(FALSE,diff(diff(x)>0)!=0))
        #Find inflection points (1rst derivative changes sign)
        wvDips <- apply(infl,1, function(x) (EXwv:(EXwv+50))[which(x)[1]])
        #Find average location of dip for all samples.
        wvDip <- round(quantile(wvDips,probs=0.9,na.rm=T))
        #If wvDip too close to EXwv, default do EXwv + 25
        if (wvDip<(EXwv+10)) wvDip <- EXwv+25
        if (wvDip>(EXwv+25)) wvDip <- EXwv+25
        
        #Store to RayleighCutoffs
        RayleighCutoffs[[leNom]] <<- wvDip
      }else
      {
        wvDip <- RayleighCutoffs[[leNom]]
      }
      
      #Truncation
      inTrunc <- dats[1,-1]>=wvDip
      dum <- All_XData[[jj]][,c(TRUE,inTrunc)]
      dum <- normLigne(dum) 
      
      # #Compute PCA on normalized spectra
      lesChoix <- computePCAsDT(nCP, dum, leNom)
    }
    else{  #not fluo!
      #normalize matrices by closure by default
      dum <- normLigne(All_XData[[leNom]])
      #Compute PCA on normalized spectra
      lesChoix <- computePCAsDT(nCP, dum, leNom)
      #Store dummy to RayleighCutoffs
      RayleighCutoffs[[leNom]] <<- All_XData[[leNom]][1,2]
    }
  }
  return(lesChoix)
}

#***********************************************************************

doPCA <- function(dats){
  thr=0.005
  dat_4_PCA <<- dats[-1,-1]
  lePCA <<- prcomp(dat_4_PCA,tol=thr)
  lePCA_NCPs <<-ncol(lePCA$rotation)
  PCA_var_explained <<- summary(lePCA)$importance[2,]
}


normLigne <- function(dum)
    # dum: an element of XData_p
{
  dats <- dum[-1,-1]
  L <- ncol(dats)
  dats <- t(apply(dats,1,function(z) z*L/sum(z)))
  dum[-1,-1] <- dats
  return(dum)
}

#******************************************************************************
Predict_plsda <- function(aggregOp,plsdaFit,mydata=NULL,probs=TRUE)
  #Predicts classes or probabilities on mydata. If mydata is NULL, predicts on training
  #If probs is TRUE, will return a table of probabilities.
{
  
  N_modeles<-length(plsdaFit)
  if (N_modeles>1){  #Need to aggregate results
    ind_apply<-as.list(1:N_modeles)
    Ps <- lapply(ind_apply, function(ii){
      if (is.null(mydata))
        predict(plsdaFit[[ii]]$finalModel, newdata=plsdaFit[[ii]]$trainingData[,-1], type="prob")
      else
        predict(plsdaFit[[ii]]$finalModel, newdata=mydata[[ii]][,-1], type="prob")
    })
    Ps<-lapply(Ps,drop)  #remove useless third dimension.
    pooled <- Ps[[1]] * NA 
    n <- nrow(pooled) 
    classes <- colnames(pooled) 
    
    FUNC<-aggregOp
    for(i in 1:ncol(pooled)) 
    { 
      tmp <- lapply(Ps, function(y, col) y[,col], col = i) 
      tmp <- do.call("rbind", tmp) 
      pooled[,i] <- apply(tmp, 2, function(x) do.call(FUNC,as.list(x))) 
    } 
    pooled <- t(apply(pooled, 1, function(x) x/sum(x))) #the probs combined should be normalized
    classes <- colnames(pooled) 
    val_pred_cl <- pooled
    if (!probs)
      val_pred_cl <- factor(classes[apply(pooled, 1, which.max)], levels = classes) 
  }else  #only one model - no aggregation.
  {
    if (is.null(mydata)){
      val_pred_cl <- predict(plsdaFit[[1]],newdata=plsdaFit[[1]]$trainingData[,-1]
                             , type="prob")
    }else
    {
      val_pred_cl <- predict(plsdaFit[[1]],newdata=mydata[[1]], type="prob")
    }
    classes<-colnames(val_pred_cl)
    if (!probs)
      val_pred_cl <- factor(classes[apply(val_pred_cl, 1, which.max)], levels = classes) 
  }
  return(val_pred_cl)
}


#***********************************************************************
Plot_Confusion_Matrix <- function(conf){
  melted<-reshape2::melt(conf)
  ggplotly(
    p1<-ggplot2::ggplot(data=melted,ggplot2::aes(x=Prediction,y=Reference,fill=value)) + 
      ggplot2::geom_tile(colour="grey50") +
      ggplot2::scale_fill_gradient2(low="blue",high="red",mid="white") +
      ggplot2::theme(axis.text.x = element_text(angle=60,hjust=1)) +
      ggplot2::theme(text = element_text(size=12)) +
      ggplot2::geom_text(ggplot2::aes(label=value), size=4)
  )
}

#***********************************************************************

plotPCAPairs <- function(data,preds,pc1,pc2)
{
  # Function to add histograms
  panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr=usr))
    par(usr = c(usr[1:2], 0, 1.5))
    his <- hist(x, plot = FALSE)
    breaks <- his$breaks
    nB <- length(breaks)
    y <- his$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
    #lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
  }
  
  dats <- rbind(data,preds)[,pc1:pc2]
  colorCodes <-as.factor(c(modelEnv$colorby,rep("Pred.",nrow(preds))))
  nCl <- length(unique(modelEnv$colorby))
  couleurs <- c(terrain.colors(nCl),"red")
  
  # Creating the scatter plot matrix
  pairs(dats,
        upper.panel = NULL,         # Disabling the upper panel
        diag.panel = panel.hist,
        pch = 21,                 # Pch symbol
        bg = couleurs[colorCodes],  # Background color of the symbol (pch 21 to 25)
        main = modelEnv$model_descript$type,
        oma=c(3,3,3,15)# Title of the plot)   
  ) 
  
  par(xpd = TRUE)
  legend('topright',pch=21,pt.bg = couleurs,legend=unique(colorCodes))
}


#***********************************************************************

dfForPlotly <- function(dataList,dats,selection)
# Convert data in dats based on selection of type (datalist) and selection
# of samples (selection).  Output a data frame ready for plotly
{
  library(tidyr)
  dfs_selection <- lapply(dataList,function(t){
    dum <- dats[[t]][1+c(0,selection),]
    dum[[1]] <- make.unique(as.character(dum[[1]]),".")
    dum[1,1] <- "ID"
    colnames(dum) <- dum[1,]
    dum <- dum[-1,]
    dum <- gather(dum,"X","Spectra",-ID)
    dum["DType"] <- strsplit(t,"_")[[1]][1]
    dum
  })
  
  dfs_plotly <- dfs_selection[[1]]
  Ndfs <- length(dfs_selection)
  if (Ndfs > 1 )
    for (i in 2:length(dfs_selection))
      dfs_plotly <- rbind(dfs_plotly,dfs_selection[[i]])
  dfs_plotly$X <- as.numeric(dfs_plotly$X)
  dfs_plotly$names <- paste(dfs_plotly$DType,dfs_plotly$ID,sep=" - ")
  dfs_plotly
}

#***********************************************************************

myNearPoint <- function(xsel,ysel,xall,yall){
  #Among (xall,yall), finds the one closer to (xsel,ysel)
  lesDist <- as.matrix(dist(matrix(c(xsel,xall,ysel,yall),ncol=2,byrow=F)))[1,]
  laRow <- which.min(lesDist[-1])
  return(laRow)
}

#***********************************************************************

myPointsInBox <-function(xbox,ybox,xall,yall){
  #Find all (xall,yall) in box defined by (xbox,ybox)
  lesRows <- which(between(xall,xbox[1],xbox[2]) & between(yall,ybox[1],ybox[2]))
}

#***********************************************************************

pr_2_prin <- function(x)
  # Converts objects of class prcomp (Q-mode PCA) to class princomp (R-mode PCA)
  # Bryan Hanson, DePauw Univ, Sept 2009
  # original is modified by adding list elements (these could be removed to save space)
{
  
  if (!"prcomp" %in% class(x)) stop("The PCA object was not of class prcomp")
  
  # sdev, center and scale for both classes are the same; no change necessary
  
  x$loadings <- x$rotation
  x$scores <- x$x
  x$call <- "No call available (data converted from class prcomp)"
  x$n.obs <- dim(x$x)[1]	
  class(x) <- c("conPCA", class(x))
  x
}

#***********************************************************************

vline <- function(x = 0, color = "red") {
  #Defines a red vertical dashed line for ggplot2
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, dash = "dash")
  )
}

#***********************************************************************

hline <- function(y = 0, color = "red") {
  #Defines a red horizontal dashed line for ggplot2
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, dash = "dash")
  )
}