
# Global variables ----
## In global environment ----
PCAs <<- list()  #PCAs of all spectrum types in XDataList
PCAs_dds <<- list()   #Score and Outside distances of PCAs 
PCAs_dds_crit <<- list()  #Limits for PCAs_dds
All_XData <<- list()  #Current set of spectra, one type per element of list
All_XData_p <<- list()  #Current set of preprocessed spectra, one type per element of list
ORI_XData <<- list()  #As in file set of spectra
Ys_df <<- data.frame(ID=character(0))  #Current Ys 
ORI_Ys_df <<- data.frame()   #As in file Ys
XDataList <<- character()   #List of current spectra types
ALLXDataList <<- character() #List of loaded spectra types
RayleighCutoffs <<- list() #where to cut to eliminate Rayleigh in fluorescence
                           #one element per element in All_XData with same names.

## In Shiny app ----

mesCouleurs <- paletteer::paletteer_d("Polychrome::palette36")

# Functions for the app ----


#***********************************************************************
Apply_PrePro <- function(PPvaluesTrunc)
  #Apply all preprocessing as defined in the prepro tab.
  # Retrieve parameters from GUI. When prepro parameters are
  # retrieved from a file, they are stored in the GUI (to be
  # implemented).
{
  #-----Truncation-----
  #retrieve from GUI
  #params is a data frame with spectrum type, lower limit and higher limit
  #in column 1 to 3 respectively
  params <- isolate(PPvaluesTrunc$dfWorking)$x$data
  
  trunc_limits <- as.matrix(params[,-1])
  lesNoms <- as.list(params[,1])
  
  lapply(lesNoms,function(leNom){
    wl<-All_XData[[leNom]][1,-1]
    All_XData_p[[leNom]] <<- All_XData[[leNom]][,((wl>=trunc_limits[x,1]) & (wl<=trunc_limits[x,2]))]
    NULL
  }) 
  
  # #-----Then per_spectra normalization - value-----
  # if (is.null(prepro_par)){   #retrieve from GUI
  #   N=length(XData_p)
  #   if (N>1){
  #     type <- sapply(gf1$children[[1]][-1,2],gWidgets2::svalue,index=TRUE)
  #   }else
  #   {
  #     type <- gWidgets2::svalue(gf1$children[[1]][-1,2],index=TRUE)
  #   }
  #   letest=any(type==2)
  #   prepro_params$byspectra_scaling_index <<- type
  #   cntr_n_w_table <- sapply(gf1$children[[1]][-1,-1],gWidgets2::svalue)
  #   cntr_n_w <- matrix(cntr_n_w_table,ncol=3,byrow=FALSE)
  #   cntr_n_w <- matrix(as.numeric(cntr_n_w[,-1]),ncol=2,byrow=FALSE)
  #   prepro_params$cntr_n_w <<- cntr_n_w
  # }else   #extract from prepro_params
  # {
  #   type <- prepro_params$byspectra_scaling_index
  #   letest=any(type==2)
  #   cntr_n_w <- prepro_params$cntr_n_w
  #   #load into GUI
  #   le_r <- lapply(seq_len(nrow(cntr_n_w)), function(i) cntr_n_w[i,])
  #   gWidgets2::delete(gf1,gf1$children[[1]])
  #   gf1<-build_byvalue_scaling_widget(XDatalist,gf1,type,le_r)
  #   
  # }
  # if (letest){
  #   wl<-lapply(XData_p, function(y) y[1,]) # a list of wl vector, one per spectrum type
  #   X<-lapply(XData_p, function(y) y[-1,]) #a list of spectral data matrix, no wl.
  #   ii<-as.list(1:nrow(cntr_n_w))
  #   XData_p <<- lapply(ii,function(x){
  #     if (type[x]==2){
  #       i1<-which(wl[[x]]>=(cntr_n_w[x,1]-cntr_n_w[x,2]))[1]
  #       i2<-which(wl[[x]]>=(cntr_n_w[x,1]+cntr_n_w[x,2]))[1]
  #       X[[x]]<-t(apply(X[[x]],1,function(z) z/mean(z[i1:i2])))
  #     }
  #     rbind(wl[[x]],X[[x]])  #rebuild matrices with wl
  #   })
  # }
  # 
  # #-----Then per_spectra normalization - closure-----
  # if (is.null(prepro_par)){   #retrieve from GUI
  #   N=length(XData)
  #   if (N>1){
  #     type <- sapply(gf1$children[[1]][-1,2],gWidgets2::svalue,index=TRUE)
  #   }else
  #   {
  #     type <- gWidgets2::svalue(gf1$children[[1]][-1,2],index=TRUE)
  #   }
  #   letest=any(type==3)
  # }else  #extract from prepro_params
  # {
  #   type <- prepro_params$byspectra_scaling_index
  #   letest<-any(type==3)
  # }
  # if (letest){
  #   wl<-lapply(XData_p, function(y) y[1,]) # a list of wl vector, one per spectrum type
  #   X<-lapply(XData_p, function(y) y[-1,]) #a list of spectral data matrix, no wl.
  #   iis<-as.list(1:length(wl))
  #   XData_p <<- lapply(iis, function(ii){
  #     if (type[ii]==3){
  #       L <- length(wl[[ii]])
  #       X[[ii]]<-t(apply(X[[ii]],1,function(z) z*L/sum(z))) #normalize matrices
  #     }
  #     rbind(wl[[ii]],X[[ii]]) #rebuild matrices with wl
  #   })
  # } 
  # 
  # #-----Then Savitzky-Golay-----
  # if (is.null(prepro_par)){    #retrieve from GUI
  #   N=length(XData)
  #   if (N>1){
  #     dosavgol <- sapply(gf_savgol$children[[1]][-1,2],gWidgets2::svalue)
  #     m<-sapply(gf_savgol$children[[1]][-1,4],gWidgets2::svalue)
  #     p<-sapply(gf_savgol$children[[1]][-1,5],gWidgets2::svalue)
  #     w<-sapply(gf_savgol$children[[1]][-1,3],gWidgets2::svalue)
  #   }else
  #   {
  #     dosavgol <- gWidgets2::svalue(gf_savgol$children[[1]][-1,2])
  #     m<-gWidgets2::svalue(gf_savgol$children[[1]][-1,4])
  #     p<-gWidgets2::svalue(gf_savgol$children[[1]][-1,5])
  #     w<-gWidgets2::svalue(gf_savgol$children[[1]][-1,3])
  #   }
  #   
  #   letest=any(dosavgol)
  #   
  #   prepro_params$do_savgol <<- dosavgol
  #   prepro_params$m <<- m
  #   prepro_params$p <<- p
  #   prepro_params$w <<- w
  # }else   #extract from prepro_params
  # {
  #   N=length(XData)
  #   dum <- as.list(seq_len(N))
  #   dosavgol<-prepro_params$do_savgol
  #   letest=any(dosavgol==TRUE)
  #   m <- prepro_par$m 
  #   p <- prepro_params$p
  #   w <- prepro_par$w
  #   gWidgets2::delete(gf_savgol,gf_savgol$children[[1]])
  #   gf_savgol <- build_savgol_widget(XDatalist,gf_savgol)
  #   sapply(dum, function(ii){
  #     lecheck <- gf_savgol$children[[1]][(ii+1),2]
  #     gWidgets2::svalue(lecheck) <- dosavgol[ii]
  #     lem <- gf_savgol$children[[1]][ii+1,4]
  #     gWidgets2::svalue(lem) <- m[ii]
  #     lep <- gf_savgol$children[[1]][ii+1,5]
  #     gWidgets2::svalue(lep) <- p[ii]
  #     lew <- gf_savgol$children[[1]][ii+1,3]
  #     gWidgets2::svalue(lew) <- w[ii]
  #   })
  # }
  # if (letest){
  #   #retrieve paramaters
  #   wl<-lapply(XData_p, function(y) y[1,]) # a list of wl vector, one per spectrum type
  #   #Truncate wl to suit window size (w)
  #   #Need to adapt length of wl to result of Savitzky-Golay
  #   iis<-as.list(1:length(wl))
  #   wl <- lapply(iis,function(k){
  #     w_2=floor(w[[k]]/2)
  #     if (dosavgol[k]){
  #       wl[[k]][(w_2+1):(length(wl[[k]])-w_2)]
  #     }else
  #     {
  #       wl[[k]]
  #     }
  #   }
  #   )
  #   X<-lapply(XData_p, function(y) y[-1,]) #a list of spectral data matrix, no wl.
  #   
  #   
  #   XData_p <<- lapply(iis, function(k) {
  #     if (dosavgol[k]){
  #       X[[k]]<-prospectr::savitzkyGolay(X[[k]],m[k],p[k],w[k])
  #     }
  #     rbind(wl[[k]],X[[k]]) #rebuild matrices with wl
  #   })
  #   
  # }
  # #end
  # return()
}

computePCA <- function(nCP,dum, leNom)
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
  PCAs_dds[[leNom]] <<- cbind(dd$SDist,dd$ODist)
  PCAs_dds_crit[[leNom]] <<- c(dd$critSD,dd$critOD)
  
  #Finalise PCA scores matrix
  rownames(pcdum$x) <- dum[-1,1]
  #Add SDist and ODist columns
  pcdum$x <- cbind(pcdum$x,dd$SDist,dd$ODist)
  colnames(pcdum$x) <- c(paste0("PC",1:nCP),
                         "SDist","ODist")
  
  dum[-1,-1] <- dats
  rownames(pcdum$rotation) <- dum[1,-1]
  colnames(pcdum$rotation) <- dum[-1,1][1:nCP]
  PCAs[[leNom]] <<- pcdum
  lesChoix <- colnames(pcdum$x)
  return(lesChoix)
}

normLigne <- function(dum)
    # dum: an element of All_XData_p
{
  dats <- dum[-1,-1]
  L <- ncol(dats)
  dats <- t(apply(dats,1,function(z) z*L/sum(z)))
  dum[-1,-1] <- dats
  return(dum)
}

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

myNearPoint <- function(xsel,ysel,xall,yall){
  #Among (xall,yall), finds the one closer to (xsel,ysel)
  lesDist <- as.matrix(dist(matrix(c(xsel,xall,ysel,yall),ncol=2,byrow=F)))[1,]
  laRow <- which.min(lesDist[-1])
  return(laRow)
}

myPointsInBox <-function(xbox,ybox,xall,yall){
  #Find all (xall,yall) in box defined by (xbox,ybox)
  lesRows <- which(between(xall,xbox[1],xbox[2]) & between(yall,ybox[1],ybox[2]))
}

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