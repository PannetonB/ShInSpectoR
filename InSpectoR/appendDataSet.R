appendDataSet <- function()
# Append Ys or Xs files for one set to another set.
# Must have compatible columns. 
# Assumed that all files have sample IDs in first column.
# It is up to the user to append Ys and all required Xs files 
# by repeatedly calling this function.
#  
# To be useful both Ys files and at least one set of X files need to be merged.
# When doing so, it is up to the user to give a consistent data set ID to both
# resulting files. For example Y_appended.txt and EX320wINV_appended.txt where
# "appended" is the data set ID.
#  
# *****************************************************************
# B. Panneton
# April 2023
# *****************************************************************

{
  #Charge les données de référence
  fichierBase <- utils::choose.files(caption="Fichier auquel on ajoute",
                                    multi=FALSE)
  unPath <- dirname(fichierBase)
  
  fichierToAppend <- utils::choose.files(
    default = file.path(unPath,".txt"),
    caption="Fichier à annexer",
    multi=FALSE)
  
  fichierToSave <- utils::choose.files(
    default = file.path(unPath,".txt"),
    caption="Fichier de destination",
    multi=FALSE)
  
 laBase <- read.table(file=fichierBase,header=FALSE,sep="\t",dec=".",
                        na.strings = "", stringsAsFactors = T)
 lAnnexe <- read.table(file=fichierToAppend,header=FALSE,sep="\t",dec=".",
                      na.strings = "", stringsAsFactors = T)
 
 #Check if columns have the same names
 unTest <- all(laBase[1,-1] == lAnnexe[1,-1])
 if (!unTest){
   winDialog(type="ok",message="FICHIERS NON COMPATIBLES")
 }else
 {
   laBase <- rbind(laBase,lAnnexe[-1,])
   write.table(laBase,file=fichierToSave,
               row.names = F, col.names = F, 
               sep="\t", dec=".")
 }
  
}