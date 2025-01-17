install.packages("rtweet")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("knitr")
install.packages("wordcloud")
install.packages("igraph")
install.packages("ggraph")
install.packages("quanteda")
install.packages("e1071")
install.packages("SparseM")
install.packages("RSelenium")
install.packages("sm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("hrbrthemes")
install.packages("UpSetR")
install.packages("eechidna")
install.packages("UpSetR")
install.packages("cartogram")
install.packages("shinythemes")
install.packages("reshape2") #INSTALAR PARA FUNCIÓN "acast"
install.packages("twitteR")
install.packages("textdata")
install.packages("wordcloud2")

loadLibraries <- function(){
  library(rtweet)
  library(tidyverse)
  library(tidytext)
  library(knitr)
  library(wordcloud)
  library(wordcloud2)
  library(igraph)
  library(ggraph)
  library(quanteda)
  library(e1071)
  library(SparseM)
  library(RSelenium)
  library(sm)
  library(wordcloud)
  library(RColorBrewer)
  library(hrbrthemes)
  library(UpSetR)
  library(eechidna)
  library(cartogram)
  
  library(scales)     #LIBRERÍAS EXTRA PARA CIERTAS FUNCIONES EN LA GENERACIÓN DE PLOTS
  library(radarchart)
  
  library(gridExtra)
  library(grid)
  library(ggplot2)
  library(lattice)  #LIBRERÍAS EXTRA PARA CIERTAS FUNCIONES EN LA GENERACIÓN DE PLOTS
  
  library(lubridate) #ERROR EN PRÁCTICA 13
  library(UpSetR)
  
  
  library(reshape2)
  
  
  library(twitteR)
    
  library(textdata)
}
  