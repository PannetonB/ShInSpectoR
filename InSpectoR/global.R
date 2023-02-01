
# Global variables ----
## In global environment ----
PCAs <<- list()  #PCAs of all spectrum types in XDataList
PCAs_dds <<- list()   #Score and Outside distances of PCAs 
PCAs_dds_crit <<- list()  #Limits for PCAs_dds
All_XData <<- list()  #Current set of spectra, one type per element of list
All_XData_p <<- list()  #Current set of preprocessed spectra, one type per element of list
ORI_XData <<- list()  #As in file set of spectra
Ys_df <<- data.frame()  #Current Ys 
ORI_Ys_df <<- data.frame()   #As in file Ys
XDataList <<- character()   #List of current spectra types
ALLXDataList <<- character() #List of loaded spectra types

## In Shiny app ----

mesCouleurs <- paletteer::paletteer_d("Polychrome::palette36")

# Functions for the app ----

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
  #Defines a vertical line for ggplot2
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