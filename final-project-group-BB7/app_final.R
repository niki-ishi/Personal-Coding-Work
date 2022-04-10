library(shiny)
library(bslib)
library(shiny)
# install.packages(rsconnect)
library(rsconnect)
library(knitr)
library(kableExtra)

source("ui_final.R")
source("server_final.R")

# Publish Shiny App to the web
shinyApp(ui = ui, server = server)

