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
Ys_df <- read.table(file=file.path(path,'Y_forShInSpectoR.txt'),header=TRUE,sep="\t",dec=".",
                   na.strings = "", stringsAsFactors = T)
echID <- as.character(ftir[,1])
wn <- as.numeric(ftir[1,-1])
sp <- ftir[-1,-1]
indi <- which(ACPRes$FTIR_PC1>10)
fGood <- sp[indi,]
fToCorrect <- sp[-indi,]

xRef <- rbind(wn,fGood)
xRef <- cbind(ID=c("",as.character(Ys_df[indi,1])),xRef)

write.table(xRef,file=file.path(path,"FTIR_RefforSInSpectoR.txt"),
                       col.names = F, row.names = F, dec=".", sep="\t")
write.table(Ys_df[indi,],file=file.path(path,"Y_RefforSInSpectoR.txt"),
                       col.names = T, row.names = F,  sep="\t")

xToCorrect <- rbind(wn,fToCorrect)
xToCorrect <- cbind(ID=c("",as.character(Ys_df[-indi,1])),xToCorrect)

write.table(xToCorrect,file=file.path(path,"FTIR_toCorrectforSInSpectoR.txt"),
            col.names = F, row.names = F, dec=".", sep="\t")
write.table(Ys_df[-indi,],file=file.path(path,"Y_toCorrectforSInSpectoR.txt"),
            col.names = T, row.names = F,  sep="\t")




lesRanges <- list(c(400,1100),c(1100,1800),c(2500,3200))
par(mfrow=c(3,1))
for (k in 1:length(lesRanges)){
  plot(wn,colMeans(fGood), type="l", col="blue",xlim=lesRanges[[k]],
       xlab = expression("Nombre d'onde [cm"^"-1"*"]"),
       ylab = "Spectres FTIR [U.A.]")
  legend("topleft",legend=c("Référence","À corriger"),
         col=c("blue","red"),pch=NA,lty=1, inset=c(0.02,0.02))
  lines(wn, colMeans(fToCorrect), type="l", col="red")
  
}

newSp <- doFTIRShift(top=15,tol=6,plotme=T)
