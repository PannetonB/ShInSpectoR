doFTIRShift <- function(top=15, tol=5, plotme=T)
{  
  source(here::here("InSpectoR/trouvePics.R"), encoding = 'UTF-8', echo=TRUE)
  source(here::here("InSpectoR/matchPics.R"), encoding = 'UTF-8', echo=TRUE)
  source(here::here("InSpectoR/shiftFTIR.R"), encoding = 'UTF-8', echo=TRUE)
  
  #Charge les données de référence
  fichierRef <- utils::choose.files(caption="Fichier de référence",
                                    multi=FALSE)
  unPath <- dirname(fichierRef)
  
  fichierToCorrect <- utils::choose.files(
    default = file.path(unPath,".txt"),
    caption="Fichier à corriger",
    multi=FALSE)
  
  ftirRef <- read.table(file=fichierRef,header=FALSE,sep="\t",dec=".",
             na.strings = "", stringsAsFactors = T)
  ftirToCorrect <- read.table(file=fichierToCorrect,header=FALSE,sep="\t",dec=".",
             na.strings = "", stringsAsFactors = T)
  
  wn <- as.numeric(ftirRef[1,-1])
  spGood <- ftirRef[-1,-1]
  spToCorrect <- ftirToCorrect[-1,-1]
  
  #Calcul de la correction.
  #On trouve les pics dans les spectres à corriger et ceux de référence
  fGood <- colMeans(spGood)
  fToCorrect <- colMeans(spToCorrect)
  picGood <- trouvePics(wn,fGood,top)
  picToCorrect <- trouvePics(wn,fToCorrect,top)
  
  lesPics <- matchPics(picGood,picToCorrect,top=top,tol=tol)
  #Les pics dont la différence de position est supérieure ou égale à 5 sont rejetés
  picsGood <- lesPics$pics1
  picsToCorrect <- lesPics$pics2
  
  newSp <- shiftFTIR(spToCorrect,picsGood,picsToCorrect,wn,plotme)
  
  if (plotme){
    
  lesRanges <- list(c(400,1100),c(1100,1800),c(2500,3200))
  #On illustre le résultat
    windows(width = 8, height=8)
    par(mfrow=c(3,1))
    ymax <- max(c(fGood,fToCorrect))
    ymin <- min(c(fGood,fToCorrect))
    for (k in 1:length(lesRanges))
    {
      plot(wn,fGood, type="l", col="blue", ylim=c(ymin,ymax),
           xlim=lesRanges[[k]],
           xlab= "Nombre d'onde",
           ylab= "FTIR [A.U.]")
      abline(v=picsGood,col="blue")
      lines(wn,fToCorrect, type="l", col="red")
      abline(v=picsToCorrect,col="red")
    }
    
    fCorrected <- colMeans(newSp)
    
    windows(width = 8, height=8)
    par(mfrow=c(3,1))
    for (k in 1:length(lesRanges)){
      plot(wn,fCorrected,type='l',pch=20, cex=0.6, col="blue", xlim=lesRanges[[k]],
           xlab = expression("Nombre d'onde [cm"^"-1"*"]"),
           ylab = "Spectres FTIR [U.A.]")
      lines(wn,fGood,col="red",type='l')
      legend("topleft",legend=c("Corrigés","Référence"), lty=c(1,1),
             pch=c(NA,NA), inset=c(0.02,0.02), col=c("blue","red"))
    }
    par(mfrow=c(1,1))
  }
  ftirToCorrect[-1,-1] <- newSp
  unPath <- dirname(fichierToCorrect)
  unNom <- basename(fichierToCorrect)
  
  outFile <- paste0(sub("[_].*","c",unNom),"_",sub(".*[_]","",unNom))
  write.table(ftirToCorrect,file=file.path(unPath,outFile),sep="\t",dec=".",row.names = F, col.names = F)
  
}