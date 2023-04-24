#On a fait une ACP sur les données FTIR.
#Sur PC1, les données plus récentes sont aux valeurs élevées (>10).
#On va utiliser ce critère pour séparer les 2 jeux de données.
ACPRes <- read.delim("~/Consultant/AAC_ACIA_2022_23/Data/FTIR_x_Shifts/ACPRes.txt")
hist(ACPRes$FTIR_PC1,20)
#PC1>10 : bonnes données
#PC1 <=10 : vieilles données

#Charge les données
#path <- utils::choose.dir()
path <- "D:\\Bernard\\Documents\\Consultant\\AAC_ACIA_2022_23\\Data\\FTIR_x_Shifts\\data"
ftir <- read.table(file=file.path(path,'FTIR_forShInSpectoR.txt'),header=FALSE,sep="\t",dec=".",
           na.strings = "", stringsAsFactors = T)
wn <- as.numeric(ftir[1,-1])
sp <- ftir[-1,-1]
indi <- which(ACPRes$FTIR_PC1>10)
fGood <- sp[indi,]
fToCorrect <- sp[-indi,]


lesRanges <- list(c(400,1100),c(1100,1800),c(1800,2500),c(2500,3200))
par(mfrow=c(2,2))
for (k in 1:4){
  plot(wn,colMeans(fGood), type="l", col="blue",xlim=lesRanges[[k]],
       xlab = expression("Nombre d'onde [cm"^"-1"*"]"),
       ylab = "Spectres FTIR [U.A.]")
  legend("topleft",legend=c("Référence","À corriger"),
         col=c("blue","red"),pch=NA,lty=1, inset=c(0.02,0.02))
  lines(wn, colMeans(fToCorrect), type="l", col="red")
  
}

newSp <- doFTIRShift(fGood,fToCorrect,wn, plotme=T)
