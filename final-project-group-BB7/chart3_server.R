library(ggplot2)
library(plotly)
library(dplyr)

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

avg_race_2020 <- data2020 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>%   group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2020") 

avg_sex_2020 <- data2020 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2020") 

avg_edu_2020 <- data2020 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2020") 

#avg 2021
avg_age_2021 <- data2021 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2021") 

avg_race_2021 <- data2021 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>%   group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2021") 

avg_sex_2021 <- data2021 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2021") 

avg_edu_2021 <- data2021 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2021")  

avg_ori_2021 <- data2021 %>%
  group_by(Group ,Subgroup) %>%
  filter(Group == 'By Sexual orientation') %>%
  summarize(avg = mean(Value, na.rm = TRUE)) %>%
  mutate(Year = "2021")

avg_gen_2021 <- data2021 %>%
  group_by(Group ,Subgroup) %>%
  filter(Group == 'By Gender identity') %>%
  summarize(avg = mean(Value, na.rm = TRUE)) %>%
  mutate(Year = "2021")

avg_dis_2021 <- data2021 %>%
  group_by(Group ,Subgroup) %>%
  filter(Group == 'By Disability status') %>%
  summarize(avg = mean(Value, na.rm = TRUE)) %>%
  mutate(Year = "2021")

#avg 2022
avg_age_2022 <- data2022 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2022") 

avg_race_2022 <- data2022 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>%   group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2022") 

avg_sex_2022 <- data2022 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2022") 

avg_edu_2022 <- data2022 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2022") 

avg_dis_2022 <- data2022 %>% 
  filter(Group == 'By Disability status') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2022") 

avg_gend_2022 <- data2022 %>% 
  filter(Group == 'By Gender identity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2022") 

avg_ori_2022 <-  data2022 %>% 
  filter(Group == 'By Sexual Orientation') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2022") 

chart3_sex <- rbind(avg_sex_2021, avg_sex_2022)

chart3_ori <- rbind(avg_ori_2021, avg_ori_2022)

chart3_edu <- rbind(avg_edu_2020, avg_edu_2021, avg_edu_2022)

chart3_gen <- rbind(avg_gen_2021, avg_gend_2022)

chart3_dis <- rbind(avg_dis_2021, avg_dis_2022)

server <- function(input, output, session) {
  output$myplot <- renderPlotly({
    if (input$user_category == "By Sex") {
      chart3_sex <- chart3_sex %>% filter(Year %in% input$time)
      ggplot(chart3_sex, aes(Subgroup, avg)) + geom_col(fill = "pink") +
        facet_wrap(~Year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % of subgroup with mental health symptoms")
    } else if (input$user_category == "By Education") {
      chart3_edu <- chart3_edu %>% filter(Year %in% input$time)
      ggplot(chart3_edu, aes(Subgroup, avg)) + geom_col(fill = "light green") +
        facet_wrap(~Year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % of subgroup with mental health symptoms")
    }
    else if (input$user_category == "By Disability") {
      chart3_dis <- chart3_dis %>% filter(Year %in% input$time)
      ggplot(chart3_dis, aes(Subgroup, avg)) + geom_col(fill = "violet") +
        facet_wrap(~Year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % of subgroup with mental health symptoms")
    }
    else if (input$user_category == "By Gender Identity") {
      chart3_gen <- chart3_gen %>% filter(Year %in% input$time)
      ggplot(chart3_gen, aes(Subgroup, avg)) + geom_col(fill = "light blue") +
        facet_wrap(~Year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % of subgroup with mental health symptoms")
    }
    else if (input$user_category == "By Sexual Orientation") {
      chart3_ori <- chart3_ori %>% filter(Year %in% input$time)
      ggplot(chart3_ori, aes(Subgroup, avg)) + geom_col(fill = "turquoise") +
        facet_wrap(~Year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % of subgroup with mental health symptoms")
    }
  })
  
}



