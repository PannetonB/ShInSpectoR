# Load libraries ----

lesLibrairies <-
  c(  "shiny",
      "shinyjs",
      "shinyBS",
      "shinyFiles",
      "DT",
      "plotly",
      "dplyr",
      "shinyjqui",
      "rhandsontable",
      "reactlog",
      "ggplot2",
      "ggthemes",
      "pls",
      "waiter",
      "GGally",
      "ggpubr",
      "prospectr",
      "caret",
      "paletteer"
  )

cat("Loading libraires!")
i1 <- !(lesLibrairies %in% row.names(installed.packages()))
if(any(i1)) {
  install.packages(lesLibrairies[i1], dependencies = TRUE) 
}
lapply(lesLibrairies, require, character.only = TRUE)