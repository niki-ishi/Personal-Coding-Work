library(shiny)
library(bslib)
library(shiny)
# install.packages(rsconnect)
library(rsconnect)
source("ui1.R")
source("server1.R")

# Publish Shiny App to the web
shinyApp(ui = ui, server = server)
