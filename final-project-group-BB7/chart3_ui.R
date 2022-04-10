library(ggplot2)
library(plotly)
library(bslib)

data1 <- read.csv("Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")

plot3_sidebar <- sidebarPanel(
  radioButtons(
    inputId = "user_category",
    label = "Group",
    choices = c("By Disability", "By Sex", "By Education", "By Gender Identity", "By Sexual Orientation")
    ),
  selectInput(
    inputId = "time",
    label = "Year",
    choices = c("2020", "2021", "2022"),
    selected = "2021"
  )
  )

plot3_main <- mainPanel(
  plotlyOutput(outputId = "myplot"),
  tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  
  tags$head(tags$style(".shiny-output-error:after{content: 'Invalid year. Please try another';
visibility: visible}")),
  h2("Purpose"),
  p("The purpose of Chart 3 is to demonstrate and understand how different groups are effected by mental health symptoms throughout the Covid-19 pandemic. These different groups are known as Education, Sex, Sexual Orientation, Gender Identity, and Disability. These various groups can be further broken down into subgroups relating to each group. These can be seen by selecting the radio button correlating to one of the groups. Additionally, a drop down menu can be used to select a year ranging from 2020-2022 to showcase the data from that group. However, due to some inconsistencies in the original dataset some groups do not have visual data for some years.")
)

plot_3 <- tabPanel(
  "Social Identities Plot",
  sidebarLayout(
    plot3_sidebar,
    plot3_main
  )
)

ui <- navbarPage(
  "Covid",
  plot_3
)
