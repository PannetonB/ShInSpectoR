shiftFTIR <- function(spToCorrect,picsGood,picsToCorrect,wn, plotme=TRUE)
  # Fait la correction de l'axe des x.
  # picsGood et picsToCorrect sont respectivement pics1 et pics2 de la liste
  # retournée par la fonction matchPics
  # ENTRÉES :
  #   spToCorrect   : les spectres auxquelles on doit appliquer la correction
  #   picsGood      : le vecteur des x pour les pics du jeu de référence
  #   picsToCorrect : le vecteur des x pour les pics du jeu à corriger.
  #   wn            : le vecteur des x pour les spectres de référence.
  #   plotme        : TRUE pour montrer la régression.
  # SORTIE :
  #   La matrice des spectres réinterpolés aux valeurs de wn
#   après correction de l'axe des x.
{
  lm.Correction <- lm(picsGood~picsToCorrect + I(picsToCorrect^2))
  print(lm.Correction)
  anova(lm.Correction)
  
  if (plotme){
    windows()
    mais <- par("mai")
    par(mai=mais+c(0,0.4,0,0))
    par(mfrow=c(1,1))
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
  }
  
  #On fait la correction pour générer de nouveaux spectres.
  #Nombre d'ondes corrigé
  wnCorr <- predict(lm.Correction,
                    newdata = data.frame(picsToCorrect=wn))
  #On refait l'interpolation - on illustre sur les spectres moyens seulement
  newSp <- t(apply(spToCorrect,1,FUN=function(unSp){spline(wnCorr, unSp,xout=wn)$y}))
  return(newSp)
}