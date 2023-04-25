matchPics <- function(pics1,pics2,top=6,tol=5)
  #Appariement des pics entre les 2 listes.
  # ENTRÉES :
  #     pics1 : vecteur de positions de pic
  #     pics2 : vecteur de positions de pic
  #     tol   : écart maximal entre 2 pics appariés
  # SORTIE :
  #     vecteur des positions des pics communs
{
  diffs <- numeric()
  lesMatch <- integer()
  for (k in 1:top)
  {
    lesdiffs <- abs(pics2[k]-pics1)
    unMatch <- which.min(lesdiffs)
    diffs <- c(diffs,lesdiffs[unMatch])
    lesMatch <- c(lesMatch,unMatch)
  }
  lesMatch <- lesMatch[diffs<tol]
  picsGood <- pics1[lesMatch]
  picsToCorrect <- pics2[diffs<tol]
  return(list(pics1=picsGood,pics2=picsToCorrect))
}