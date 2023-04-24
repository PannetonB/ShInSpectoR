setProjectPath <- function()
{

  leFichier <- here("InSpectoR","www","defPath.RData")
  if (file.exists(leFichier)){
    load(leFichier)
    projectDir <- utils::choose.dir(projectDir)
    save(projectDir,file=leFichier)
  }else
  {
    projectDir <- utils::choose.dir(fs::path_home_r())
    save(projectDir,file=leFichier)
  }
}