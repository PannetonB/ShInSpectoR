#On a fait une ACP sur les données FTIR.
#Sur PC1, les données plus récentes sont aux valeurs élevées (>10).
#On va utiliser ce critère pour séparer les 2 jeux de données.
ACPRes <- read.delim("~/Consultant/AAC_ACIA_2022_23/Data/FTIR_x_Shifts/ACPRes.txt")
hist(ACPRes$FTIR_PC1,20)
#PC1>10 : bonnes données
#PC1 <=10 : vieilles données

#Charge les données
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

plot(wn,fGood, type="l", col="blue",xlim=c(2700,3200),
     xlab = expression("Nombre d'onde [cm"^"-1"*"]"),
     ylab = "Spectres FTIR [U.A.]")
legend("topright",legend=c("Référence","À corriger"),
       col=c("blue","red"),pch=NA,lty=1, inset=c(0.02,0.02))
lines(wn, fToCorrect, type="l", col="red")




trouvePics <- function(wn,Sp, top=6)
  #Fonction pour trouver des pics. 
  #Paramètres
  # ENTRÉES:
  #   wn : vecteur des nombres d'onde
  #   Sp : spectre aux valeurs de wn
  #   top: nombre de spectres à retourner. Les pics sont classés par 
  #        valeur de courbure. Plus la courbure est forte, plus le pic est
  #        bien défini (pointu). On ne garde que les "top" pics dont la courbure
  #        est la plus grande.
  # SORTIE:
  #   La valeur sur l'axe wn des pics retenus.  
{
  
  cat("\n TROUVE PICS\n")
  w=7
  delta <- w %/% 2
  N <- length(wn)
  deriv1 <- prospectr::savitzkyGolay(Sp,1,5,w,1)
  deriv2 <- prospectr::savitzkyGolay(Sp,2,5,w,1)
  wnSG <- wn[-c(1:delta,(N-delta+1):N)]
  laderiv1 <- approxfun(wnSG, deriv1)
  laderiv2 <- approxfun(wnSG, deriv2)
  zeroDeriv1 <- rootSolve::uniroot.all(laderiv1, 
                                       interval=range(wnSG),
                                       n=1000)
  # cat(paste0(zeroDeriv1," - ",laderiv2(zeroDeriv1),"\n"))
  Deriv2atZeros <- laderiv2(zeroDeriv1)
  wlMax <- zeroDeriv1[Deriv2atZeros<0]
  Deriv2atZeros <- Deriv2atZeros[Deriv2atZeros<0]
  sortedd2OnMax <- sort(Deriv2atZeros,decreasing=F,index.return=T)
  return(wlMax[sortedd2OnMax$ix][1:top])
}

#Calcul de la correction.
#On trouve les pics dans les spectres à corriger et ceux de référence
top=15
picGood <- trouvePics(wn,fGood,top)
picToCorrect <- trouvePics(wn,fToCorrect,top)

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
#Les pics dont la différence de position est supérieure ou égale à 5 sont rejetés
picsGood <- picGood[lesMatch[diffs<5]]
picsToCorrect <- picToCorrect[diffs<5]

#On illustre le résultat
par(mfrow=c(2,1))
ymax <- max(c(fGood,fToCorrect))
ymin <- min(c(fGood,fToCorrect))
plot(wn,fGood, type="l", col="blue", ylim=c(ymin,ymax),
     xlim=c(400,1500),
     xlab= "Nombre d'onde",
     ylab= "FTIR [A.U.]")
abline(v=picsGood,col="blue")
lines(wn,fToCorrect, type="l", col="red")
abline(v=picsToCorrect,col="red")

plot(wn,fGood, type="l", col="blue", ylim=c(ymin,ymax),
     xlim=c(1500,3200),
     xlab= "Nombre d'onde",
     ylab= "FTIR [A.U.]")
abline(v=picsGood,col="blue")
picToCorrect <- trouvePics(wn,fToCorrect,top)
lines(wn,fToCorrect, type="l", col="red")
abline(v=picsToCorrect,col="red")
par(mfrow=c(1,1))

#Calcul de la régression pour faire la correction
lm.Correction <- lm(picsGood~picsToCorrect + I(picsToCorrect^2))
print(lm.Correction)
anova(lm.Correction)
mais <- par("mai")
par(mai=mais+c(0,0.4,0,0))
plot(picsToCorrect,picsGood, pch=21, col="blue",bg="cyan", cex=1.2,
     xlab=expression("Nombre d'onde - Spectres à corriger [cm"^"-1"*"]"),
     ylab=expression("Nombre d'onde - Spectres de référence [cm"^"-1"*"]"))
lines(400:3200,predict(lm.Correction,
                        newdata = data.frame(picsToCorrect=400:3200)),
      col="blue", lwd=2
)
coe <- format(coef(lm.Correction),digits=4)
text(1000,2700,paste0("Y = ", coe[1], "+", coe[2],"xX + ",coe[3]," X²"),pos=4)
par(mai=mais)

#On fait la correction pour générer de nouveaux spectres.
#Nombre d'ondes corrigé
wnCorr <- predict(lm.Correction,
                  newdata = data.frame(picsToCorrect=wn))
#On refait l'interpolation - on illustre sur les spectres moyens seulement
newSp <- apply(sp[-indi,],1,FUN=function(unSp){spline(wnCorr, unSp,xout=wn)$y})
fCorrected <- rowMeans(newSp)
plot(wn,fCorrected,type='p',pch=20, cex=0.8, col="blue", xlim=c(1000,1600),
     xlab = expression("Nombre d'onde [cm"^"-1"*"]"),
     ylab = "Spectres FTIR [U.A.]")
lines(wn,fGood,col="red",type='l')
legend("topleft",legend=c("Corrigés","Référence"), lty=c(NA,1),
       pch=c(20,NA), inset=c(0.02,0.02), col=c("blue","red"))

plot(wn,fCorrected,type='p',pch=20, cex=0.8, col="blue", xlim=c(2800,3100),
     xlab = expression("Nombre d'onde [cm"^"-1"*"]"),
     ylab = "Spectres FTIR [U.A.]")
lines(wn,fGood,col="red",type='l')
legend("topleft",legend=c("Corrigés","Référence"), lty=c(NA,1),
       pch=c(20,NA), inset=c(0.02,0.02), col=c("blue","red"))
