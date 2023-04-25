FTIR_Assembler <- function()
#Reads all dpt files in a directory and assembles data in a single
# X data files for ShInSpectoR. 
# Also creates a Y data file with ID in first column and a dummy
# factor in the second column. This dummy factor is simply FTIR.
#
# The user is prompted to select a directory and the resulting
# files are stored in this directory. Two files are created:
#   - FTIR_forShInSpectoR.txt which contains the spectra
#   _ Y_forShInSpectoR.txt which is the required Y file.
#
#  
  
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
    echID <-  sub("[.].*","",unFichier) #Keep all left to first dot
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
  outData <- data.frame(ID=ID,SpectrumType="FTIR",Seq=1:length(lesFichiers))
  write.table(outData,file=Yfile,row.names = F,col.names = T,sep="\t")
}
