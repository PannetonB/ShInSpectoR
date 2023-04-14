FTIR_Assembler <- function()
{
  lePath <- choose.dir()
  fichierOut <- file.path(lePath,"FTIR_forShInSpectoR.txt")
  lesFichiers <- list.files(lePath)
  #Ne conserver que les fichiers *.dpt
  indi <- stringr::str_detect(lesFichiers,"dpt")
  lesFichiers <- lesFichiers[indi]
  newOutFile <- TRUE
  ID <- character()
  for (unFichier in lesFichiers){
    echID <-  sub("_.*","",unFichier)
    ID <- c(ID,echID)
    dats <- read.delim(file.path(lePath,unFichier), header=FALSE)
    #Reverse elements as wn are in decreasing order
    wn <- rev(as.numeric(dats[,1]))
    wn1 <- ceiling(min(wn))
    wn2 <- floor(max(wn))
    wnInt <- wn1:wn2
    sp <- rev(as.numeric(dats[,2]))
    spInt = spline(wn, sp, xout = wnInt)
    if (newOutFile){
      outData <- data.frame()
      outData[1,1] <- ""
      outData[1,(2:(length(wnInt)+1))] <- wnInt
      newOutFile <- FALSE
    }
    leRow <- nrow(outData)+1 
    outData[leRow,] <- 0
    outData[leRow,1] <- echID
    outData[leRow,-1] <- spInt$y
  
  }
  write.table(outData,file=fichierOut,row.names = F,col.names = F,sep="\t")
  Yfile <- file.path(lePath,"Y_forShInSpectoR.txt")
  outData <- data.frame(ID=ID,Seq=1:length(lesFichiers))
  write.table(outData,file=Yfile,row.names = F,col.names = T,sep="\t")
}
