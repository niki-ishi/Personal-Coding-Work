library(bslib)
library(shiny)
library(rsconnect)
source("ui2.R")
source("server2.R")


shinyApp(ui = ui, server = server)