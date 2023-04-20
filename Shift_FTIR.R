#Sur PC1, les données plus récentes sont aux valeurs élevées
ACPRes <- read.delim("~/Consultant/AAC_ACIA_2022_23/Data/FTIR_x_Shifts/ACPRes.txt")
hist(ACPRes$FTIR_PC1,20)
#PC1>10 : bonnes données
#PC1 <=10 : vieilles données
path <- utils::choose.dir()
ftir <- read.table(file=file.path(path,'FTIR_forShInSpectoR.txt'),header=FALSE,sep="\t",dec=".",
           na.strings = "", stringsAsFactors = T)
wn <- as.numeric(ftir[1,-1])
sp <- ftir[-1,-1]
indi <- which(ACPRes$FTIR_PC1>10)
fGood <- sp[indi,]
fToCorrect <- sp[-indi,]

fGood <- colMeans(fGood)
fToCorrect <- colMeans(fToCorrect)

plot(wn,fGood, type="l", col="blue")
lines(wn, fToCorrect, type="l", col="red")



#Trouve pics
trouvePics <- function(wn,Sp, top=6)
{
  w=7
  delta <- w %/% 2
  N <- length(wn)
  deriv1 <- prospectr::savitzkyGolay(Sp,1,5,w,1)
  deriv2 <- prospectr::savitzkyGolay(Sp,2,5,w,1)
  wnSG <- wn[-c(1:delta,(N-delta+1):N)]
  laderiv1 <- approxfun(wnSG, deriv1)
  laderiv2 <- approxfun(wnSG, deriv2)
  curve(laderiv1(x), min(wnSG), max(wnSG))
  curve(laderiv2(x), min(wnSG), max(wnSG))
  zeroDeriv1 <- rootSolve::uniroot.all(laderiv1, interval=range(wnSG))
  Deriv2atZeros <- laderiv2(zeroDeriv1)
  wlMax <- zeroDeriv1[Deriv2atZeros<0]
  Deriv2atZeros <- Deriv2atZeros[Deriv2atZeros<0]
  sortedd2OnMax <- sort(Deriv2atZeros,decreasing=F,index.return=T)
  return(wlMax[sortedd2OnMax$ix][1:top])
}
top=10
picGood <- trouvePics(wn,fGood,top)
plot(wn,fGood, type="l", col="blue")
abline(v=picGood,col="blue")
picToCorrect <- trouvePics(wn,fToCorrect,top)
lines(wn,fToCorrect, type="l", col="red")
abline(v=picToCorrect,col="red")

#Appariement des pics
diffs <- numeric()
lesMatch <- integer()
for (k in 1:top)
{
  lesdiffs <- abs(picToCorrect[k]-picGood)
  unMatch <- which.min(lesdiffs)
  diffs <- c(diffs,lesdiffs[unMatch])
  lesMatch <- c(lesMatch,unMatch)
}
picsGood <- picGood[lesMatch[diffs<5]]
picsToCorrect <- picToCorrect[diffs<5]
plot(picsToCorrect,picsGood)
lm.Correction <- lm(picsGood~poly(picsToCorrect,2))
