rep <- "C:/Users/gottavianoni/Desktop/GESTAE/"

library("htmlwidgets")
library("shiny")
library("shinydashboard")

#browseURL("http://localhost:6002")
runApp(paste0(rep,"App-1/"),port = 6002,launch.browser = T)