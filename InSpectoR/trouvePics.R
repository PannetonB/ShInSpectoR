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