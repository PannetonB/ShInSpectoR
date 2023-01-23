dfForPlotly <- function(dataList,dats,selection)
{
  library(tidyr)
  dfs_selection <- lapply(dataList,function(t){
    dum <- dats[[t]][1+c(0,selection),]
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
  dfs_plotly$names <- paste(dfs_plotly$ID,dfs_plotly$DType,sep=" - ")
  dfs_plotly
 
}


plotly::plot_ly(dfs_plotly,x=~X,y=~Spectra, color = ~DType, type = 'scatter', mode = 'lines',
                 linetype = ~ID, 
                 name=~names) %>%
  layout(legend=list(title=list(text='<b>Échantillon - Type de données</b>'),
                     x=0.75, y=0.98),
         xaxis = list(title = 'Wavelength [nm] or Wavenumber[cm<sup>-1</sup>]'), 
         yaxis = list(title = 'Spectra [A.U.]'))
