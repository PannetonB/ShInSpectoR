
# Global variables ----
## In global environment ----
PCAsDT <<- list()  #PCAs of all spectrum types in XDataList
PCAsdt_dds <<- list()   #Score and Outside distances of PCAs 
PCAsdt_dds_crit <<- list()  #Limits for PCAs_dds
All_XData <<- list()  #Current set of spectra, one type per element of list
All_XData_p <<- list()  #Current set of preprocessed spectra, one type per element of list
ORI_XData <<- list()  #As in file set of spectra
Ys_df <<- data.frame(ID=character(0))  #Current Ys 
ORI_Ys_df <<- data.frame()   #As in file Ys
XDataList <<- character()   #List of current spectra types
ALLXDataList <<- character() #List of loaded spectra types
RayleighCutoffs <<- list() #where to cut to eliminate Rayleigh in fluorescence
                           #one element per element in All_XData with same names.
whichPLSPlot <<- "Error"

## In Shiny app ----

mesCouleurs <- paletteer::paletteer_d("Polychrome::palette36")

# Functions for the app ----


#***********************************************************************
#*

collectPreProParams <- function(PPValuesTrunc,input){
  
  #retrieve content of dfWorking data frame in PPValuesTrunc
  #truncParams is a data frame with spectrum type, lower limit and higher limit
  #in column 1 to 3 respectively
  TruncParams <- PPValuesTrunc$dfWorking$x$data
  
  #Truncation limits
  trunc_limits <- TruncParams[,-1]
  lesNoms <- as.list(TruncParams[,1])
  names(trunc_limits) <- c("lo","hi")
  
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

Apply_PrePro <- function(preproParams)
  #Apply all preprocessing as defined in the prepro tab.
  #In preproParams, lesNoms give active spectrum types
{
  #Truncation-----
 
  
  trunc_limits <- preproParams$trunc_limits
  lesNoms <- preproParams$lesNoms
  
  lapply(lesNoms,function(leNom){
    wl<-All_XData[[leNom]][1,-1]
    All_XData_p[[leNom]] <<- All_XData[[leNom]][,c(TRUE,
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
               dum <- normLigne(All_XData_p[[leNom]])
               All_XData_p[[leNom]] <<-dum  
             },
             waveband ={
               wl <- All_XData_p[[leNom]][1,-1]
               ctr <- as.numeric(perSpecParams[[leNom]][2])
               band <- as.numeric(perSpecParams[[leNom]][3])
               halfband <- floor(band/2)
               i1 <- ctr-halfband
               i2 <- ctr+halfband
               i1 <- which.min(abs(i1-wl))
               i2 <- which.min(abs(i2-wl))
               dats <- All_XData_p[[leNom]][-1,-1]
               L <- ncol(dats)
               dats <- t(apply(dats,1,function(z) z/mean(z[i1:i2])))
               All_XData_p[[leNom]][-1,-1] <<- dats
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
        wl<- All_XData_p[[leNom]][1,-1] # a list of wl vector, one per spectrum type
        wl <- wl[(w_2+1):(length(wl)-w_2)]
        
        X <- All_XData_p[[leNom]][-1,-1]
        X <- prospectr::savitzkyGolay(X,m,p,w)
        X <- rbind(wl,X) #rebuild matrices with wl
        All_XData_p[[leNom]] <<- cbind(All_XData_p[[leNom]][,1],X)
      }
    })
  }
}

#***********************************************************************

computePCAsDT <- function(nCP,dum, leNom)
  # nCP: number of PC desired
  # dum: an element of All_XData_p
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
  colnames(pcdum$rotation) <- dum[-1,1][1:nCP]
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
      lesChoix <- computePCAsDT(input$npcs, dum, leNom)
      #Store dummy to RayleighCutoffs
      RayleighCutoffs[[leNom]] <<- All_XData[[leNom]][1,2]
    }
  }
  return(lesChoix)
}

#***********************************************************************

normLigne <- function(dum)
    # dum: an element of All_XData_p
{
  dats <- dum[-1,-1]
  L <- ncol(dats)
  dats <- t(apply(dats,1,function(z) z*L/sum(z)))
  dum[-1,-1] <- dats
  return(dum)
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