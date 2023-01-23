library(DT)
datatable(iris,
          rownames = FALSE, 
          extensions = 'RowGroup', 
          options = list(rowGroup = list(dataSrc=c(4)),
                         columnDefs = list(list(visible=FALSE, targets=c(4)))
          )
)


datatable(mtcars[order(mtcars[,2]),],
          extensions = "RowGroup", 
          options = list(rowGroup = list(dataSrc=c(2)),
                         columnDefs = list(list(visible=FALSE, targets=c(2)))
          )
)

nX <- length(ALLXDataList)
truncDF <- data.frame(
  Spectra = ALLXDataList,
  LowerLimit = rep(0,nX),
  HigherLimit = rep(100,nX)
)

datatable(truncDF,rownames = F, 
          caption = "TRUNCATION", width=600,
          options = list(dom = 't',
                         scrollX = TRUE,
                         autoWidth = TRUE,
                         columnDefs = list(list(width = '100px', targets = c(1,2)))
          )
)

library(rhandsontable)
dum <- rhandsontable(truncDF)
