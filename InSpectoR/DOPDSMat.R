DOPDSMat <- function( MWsize=3, Ncomp=2)
{
  
  PDS<-function(masterSpectra, slaveSpectra, MWsize=3, Ncomp=2, wavelength){
    #Piecewise Direct Standardization (PDS) algorithm:
    
    #INPUT:   masterSpectra = Spectra acquired with the master instrument (matrix).
    #         slaveSpectra = Spectra acquired with the slave instrument (matrix).
    #         MWsize = Half size of the moving window (integer).
    #         Ncomp = Number of latent variables used in the PLS model (integer).
    #         wavelength = wavelength (numeric vector).
    
    #OUTPUT:  P = the PDS transfer matrix.
    #         Intercept = constant to add
    
    # Ncomp<-2
    # MWsize<-2 
    # wavelength<-wavelengthsMaster
    
    #APPLYING EXAMPLE
    # # Compute the transfer matrix P:
    # Pmat<-PDS(Master$NIR, Slave$NIR, MWsize, Ncomp, wavelength)
    # # Standardization of the slave data (using P and the intercept):
    # SlaveCor<-as.matrix(Slave$NIR)%*%as.matrix(Pmat$P)
    # SlaveCor<-sweep(SlaveCor, 2, as.numeric(t(Pmat$Intercept)), "+")
    # SlaveCor<-data.frame(Slave[,1:6], NIR = I(SlaveCor))
    
    
    require(pls)
    
    #Loop Initialization:
    i<-MWsize
    k<-i-1
    #Creation of an empty P matrix:
    P<-matrix(0,nrow=ncol(masterSpectra),ncol=ncol(masterSpectra)-(2*i)+2)
    InterceptReg<-c()
    
    while(i<=(ncol(masterSpectra)-k)){
      
      #PLS regression:
      fit<- plsr(masterSpectra[,i] ~ as.matrix(slaveSpectra[,(i-k):(i+k)]),
                 ncomp=Ncomp, scale=F, method="oscorespls")
      
      #Extraction of the regression coefficients:
      coefReg<-as.numeric(coef(fit, ncomp=Ncomp, intercept = TRUE))
      InterceptReg<-c(InterceptReg,coefReg[1])
      coefReg<-coefReg[2:length(coefReg)]
      
      #Add coefficients to the transfer matrix:
      P[(i-k):(i+k),i-k]<-t(coefReg)
      
      rm(coefReg,fit)
      i<-i+1
      
      #Diplay progression:
      cat("\r",paste(round(i/ncol(masterSpectra)*100)," %",sep=""))}
    
    P<-data.frame(matrix(0,nrow=ncol(masterSpectra),ncol=k), P,
                  matrix(0,nrow=ncol(masterSpectra),ncol=k))
    InterceptReg<-c(rep(0,k),InterceptReg,rep(0,k)) 
    
    Output<-list(P = P , Intercept = InterceptReg)
    
    return(Output)
  }
  
  
  refMat <- choose.files(
    caption = "Choisir la matrice des spectres de référence",
    multi = FALSE,
    filters = Filters[c("txt"),]
  )
  slaveMat <- choose.files(
    caption = "Choisir la matrice des spectres de l'instrument secondaire",
    multi = FALSE,
    filters = Filters[c("txt"),]
  )
  EXwvRef <- sub("w.*","",basename(refMat))
  EXwvSlave <- sub("w.*","",basename(slaveMat))
  
  if (EXwvRef != EXwvSlave){
    outtext <- paste("\n****************************\n",
                     "Fichiers non compatibles\n",
                     refMat,"\n",
                     slaveMat,"\n",
                     "****************************\n")
    cat(outtext)
    return("")
  } 
  
  ref <- read.delim(refMat, header=FALSE)
  slave <- read.delim(slaveMat, header=FALSE)
  
  if (any(dim(ref) != dim(slave))){
    cat("\n**************************\n",
        "Dimensions des matrices incompatibles\n",
        "**************************\n")
    return("")
  }
  
  cat("\n**********************\nCalcul\n******************\n")
  P <- PDS(ref[-1,-1], slave[-1,-1], MWsize=3, Ncomp=2,
           wavelength= as.numeric(ref[1,-1]))
  
  
  outFile <- choose.files(
    caption = "Définir le nom du fichier de transfer",
    default = EXwvRef,
    multi = FALSE,
    filters = Filters[c("RData"),]
  )
  save(P,file=outFile)
  
}
