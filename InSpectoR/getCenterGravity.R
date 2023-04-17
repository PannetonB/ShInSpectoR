getCenterGravity <- function(plage=c(610:710),seuil=0.1)
  #*****************************************************************************
  # Calcul du centre de gravité sur un portion d'un spectre.
  # La portion du spectre est définie par le paramètre place. La valeur par
  # défaut est de 610 à 710 en unités de l'axe des x (longueur d'onde ou nombre
  # d'onde selon le cas).
  #
  # On applique un seuil d'amplitude minimale de la moyenne prise sur la plage pour
  # éviter de faire le calcul pour des spectres où l'amplitude est très faible dans
  # la plage. Le seuil s'applique sur des spectres normalisés en ligne. Par défaut,
  # le seuil est fixé à 0.1. Si le seuil n'est pas atteint, on retourne un NaN.
  #*****************************************************************************
  # L'opérateur est invité à choisir un fichier de spectre et un fichier des Y.
  # Si aucun fichier des Y n'est choisi, un fichier sera créé automatiquement.
  # Le résultat du calcul apparaît dans le fichier des Y dans une colonne
  # nommée Ctr_Grav.
  #*****************************************************************************
  #*B. Panneton - Avril 2023
  #*****************************************************************************
  
  
{
  normLigne <- function(dum)
    # dum: an element of XData_p
  {
    dats <- dum[-1,-1]
    L <- ncol(dats)
    dats <- t(apply(dats,1,function(z) z*L/sum(z)))
    dum[-1,-1] <- dats
    return(dum)
  }
  
  leFichier <- choose.files(caption="Choose a spectral data file",multi=F)
  
  dats <- read.delim(leFichier, header=FALSE)
  wl <- as.numeric(dats[1,-1])
  ID <- dats[-1,1]
  sp <-dats[-1,-1]
  
  yFichier <- choose.files(caption="Choose Y files or cancel for automatic creation",
                           multi = FALSE,
                           default = dirname(leFichier))
  if (length(yFichier)==0){
    newY <- TRUE
    datasetID <- sapply(strsplit(leFichier,"_"),"[[",2)
    yFichier <- file.path(dirname(leFichier),paste0("Y_",datasetID,".txt"))
    dum <- "YES"
    if (file.exists(yFichier)) dum <- utils::winDialog(type="yesno",
                                                message=paste0("FILE EXISTS!\n",
                                                               "DO YOU WANT TO REPLACE CURRENT FILE?"))
    if (dum=="NO"){
      Ys_df <- read.delim(yFichier)
    }else
    {
      Ys_df <- data.frame(ECHID = ID,dumFac="CG")
    }
  }else
  {
    Ys_df <- read.delim(yFichier)
  }
  
  
  #Remove rows with 0's or NA'susing drop_na()
  lesRows <- which(rowSums(sp)==0 | is.na(rowSums(sp)))
  if (length(lesRows)>0){
    sp <- as.matrix(sp[-lesRows,])
    ID <- ID[-lesRows]
    Ys_df <- Ys_df[-lesRows,]
  }
  
  #Calcul du centre de gravité
  indi <-  which(wl %in% plage)
  indSeuil <- which(wl==wvSeuil)
  cgs <- apply(sp,1,FUN=function(s){
    cg <- sum(s[indi]*wl[indi])/sum(s[indi])
    #Test peak amplitude in range
    if (mean(s[indi]<seuil)) cg <- NA
    round(cg,2)
  })
  
  Ys_df <- cbind(Ys_df,data.frame(Ctr_Grav=cgs))
  write.table(Ys_df,file=yFichier,sep="\t",row.names = FALSE)

  
}
