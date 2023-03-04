#data <- modelEnv$lePCA$x
#PlotPCAPairs(data,lesPreds,1,5)


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

#PlotPCAPairs(data,lesPreds,1,5)
  