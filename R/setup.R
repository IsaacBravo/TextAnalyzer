load_libraries <- function(){
  
  # Load packages
  Packages <- c("tidyverse", "magrittr", "tidytext", "gt", "DT", "shiny", 
                "shinythemes", "shinyBS", "shinyWidgets", "plotly", "ggpol",
                "reshape2", "textdata", "ggnewscale", "shinydashboard",
                "shinycssloaders", "reactable", "tidytext", "tm", 
                "wordcloud2", "sentimentr", "RColorBrewer", "stringr",
                "shinythemes", "cleanNLP")
  lapply(Packages, library, character.only = TRUE)
  
  cnlp_init_udpipe()
}