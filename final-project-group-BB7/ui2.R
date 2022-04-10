library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
#------------data manipulation-------------------------
data1 <- read.csv("Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")


data1$Time.Period.Start.Date<- as.Date(data1$Time.Period.Start.Date, format = "%m/%d/%Y")
#date format: yyyy-mm-dd 
data1$Time.Period.End.Date<- as.Date(data1$Time.Period.End.Date, format = "%m/%d/%Y")


#Data by year
data2020 <- data1 %>% 
  group_by(Time.Period.End.Date) %>% 
  filter(Time.Period.End.Date >= "2020-01-01" & Time.Period.End.Date <= "2020-12-31")

data2021 <-  data1 %>% 
  group_by(Time.Period.End.Date) %>% 
  filter(Time.Period.End.Date >= "2021-01-01" & Time.Period.End.Date <= "2021-12-31")

data2022 <- data1 %>% 
  group_by(Time.Period.End.Date) %>% 
  filter(Time.Period.End.Date >= "2022-01-01" & Time.Period.End.Date <= "2022-12-31")

# avg 2020
avg_age_2020 <- data2020 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2020") 

#avg 2021
avg_age_2021 <- data2021 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2021") 

#avg 2022
avg_age_2022 <- data2022 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2022") 

chart2_data <- rbind(avg_age_2020, avg_age_2021, avg_age_2022)
#--------------ui----------------
plot_sidebar <- sidebarPanel(
  selectInput(
    inputId = "user_category1",
    label = "Select Year",
    choices = chart2_data$Year,
    selected = c("2020"),
    multiple = TRUE)
)

plot_main <- mainPanel(
  plotlyOutput(outputId = "agePlot")
)

plot_tab <- tabPanel(
  "Age Plot",
  sidebarLayout(
    plot_sidebar,
    plot_main
  ),
  p("The purpose of Chart 2 was to show and understand how mental health symptoms vary by age throughout the pandemic. We chose a line chart to visualize this data because we wanted to see if there was a trend of mental health symptoms across different age groups over time. You may select multiple years by clicking the drop-down bar to view data per year. "),
  p("The information in the data visualization offered several insights. There was a clear decline in mental health symptoms from the 18-29 years age group to the 80 years and above age group. Younger people tend to have the highest percentage of mental health symptoms and older people tend to have the lowest percentage. This shows that pandemic-related stress and depression affect younger people more, possibly due to its large disruption of social interactions, work, and school. Moreover, the percentage of reported depressive symptoms was the highest across all age groups in 2020. The spread of COVID-19 worldwide and lockdown mandates in 2020 took a toll on everyone’s mental wellness." )
)

ui <- navbarPage(
  "Age",
  plot_tab,
)
