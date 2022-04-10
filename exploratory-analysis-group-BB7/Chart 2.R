library(readr)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)

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
  mutate(Year = "2021") 

avg_sex_2022 <- data2022 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Year = "2021") 

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

full_table <- rbind(avg_age_2020, avg_age_2021, avg_age_2022, avg_dis_2021, avg_dis_2022, avg_edu_2020, avg_edu_2021, avg_edu_2022, avg_gen_2021, avg_gend_2022, avg_ori_2021, avg_ori_2022, avg_race_2020, avg_race_2021,  avg_race_2022, avg_sex_2020, avg_sex_2021, avg_sex_2022)

full_table$avg <- (round(full_table$avg, digits = 2))

final_table <- rename(full_table,"Average Percentage of Subgroup with Mental Health Symptoms Per Year" = avg) 

kable(final_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

#Chart 2 should show the Average percent of subgroup with Mental Health symptoms in 2021 by Age.

chart2.data <- full_table %>% filter(Group == "By Age") %>% filter(Year == "2021")

library(ggplot2)

ggplot(chart2.data, mapping = aes(x = Subgroup, y = avg, group = 1)) +
  geom_point() + geom_line() +
  labs(
    title = " Mental Health by Age During COVID-19 (2021)", x = "Ages", y = "Avg % of age group with mental health symptoms")
