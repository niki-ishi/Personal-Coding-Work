data1 <- read.csv("Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")

library(dplyr)
library(stringr)


data1$Time.Period.Start.Date<- as.Date(data1$Time.Period.Start.Date, format = "%m/%d/%Y")
#date format: yyyy-mm-dd 
data1$Time.Period.End.Date<- as.Date(data1$Time.Period.End.Date, format = "%m/%d/%Y")

#separate Indicators into different dataframe
#depressive <- data1 %>% filter(Indicator =='Symptoms of Depressive Disorder')
#anxiety <- data1 %>% filter(Indicator =='Symptoms of Anxiety Disorder')
#depressive_or_anxiety <- data1 %>% filter(Indicator =='Symptoms of Anxiety Disorder or Depressive Disorder')
 
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

#--------------- 2020 total reported symptoms----------------
#total reported symptoms by age in 2020 
avg_age_2020 <- data2020 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_age_2020 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2020") 

#total reported symptoms by race in 2020 
avg_race_2020 <- data2020 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_race_2020 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2020") 

#total reported symptoms by sex in 2020 
avg_sex_2020 <- data2020 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_sex_2020 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2020") 

#total reported symptoms by education in 2020 
avg_edu_2020 <- data2020 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_edu_2020 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2020") 
# no data for avg_dis_2020 (Data available not until 2021-04-14 to present)
# no data for avg_gend_2020 (Data available not until 2021-07-21 to present)
# no data for avg_ori_2020 (Data available not until 2021-07-21 to present)

#--------------- 2021 total reported symptoms----------------

#total reported symptoms by age in 2021 
avg_age_2021 <- data2021 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_age_2021 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#total reported symptoms by race in 2021 
avg_race_2021 <- data2021 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_race_2021 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#total reported symptoms by sex in 2021 
avg_sex_2021 <- data2021 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_sex_2021 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#total reported symptoms by education in 2021 
avg_edu_2021 <- data2021 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_edu_2021 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#total reported symptoms by disability status in 2021 
avg_dis_2021 <- data2021 %>% 
  filter(Group == 'By Disability status') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_edu_2021 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#total reported symptoms by gender identity in 2021 
avg_gend_2021 <- data2021 %>% 
  filter(Group == 'By Gender identity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_edu_2021 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#total reported symptoms by sexual orientation in 2021 
avg_ori_2021 <-  data2021 %>% 
  filter(Group == 'By Sexual Orientation') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_ori_2021 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#------------2022 total reported symptoms-----------
#total reported symptoms by age in 2022 
avg_age_2022 <- data2022 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_age_2022 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

#total reported symptoms by race in 2022 
avg_race_2022 <- data2022 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_race_2022 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#total reported symptoms by sex in 2022 
avg_sex_2022 <- data2022 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_sex_2022 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

#total reported symptoms by education in 2022 
avg_edu_2022 <- data2022 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_edu_2022 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

#total reported symptoms by disability status in 2022 
avg_dis_2022 <- data2022 %>% 
  filter(Group == 'By Disability status') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_edu_2022 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

#total reported symptoms by gender identity in 2022 
avg_gend_2022 <- data2022 %>% 
  filter(Group == 'By Gender identity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_edu_2022 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

#total reported symptoms by sexual orientation in 2022 
avg_ori_2022 <-  data2022 %>% 
  filter(Group == 'By Sexual Orientation') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg_ori_2022 = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

#-----------Information for Summary----------------
#highest average value of total symptoms by age 
#final value
highest_max_avg_age_2020 <- data2020 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_age_2020 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_age_2020 == max(avg_age_2020, na.rm = TRUE)) %>% 
  pull(avg_age_2020)

highest_max_avg_age_2020 = round(highest_max_avg_age_2020, 2)

max_avg_age_2021 <- data2021 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_age_2021 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_age_2021 == max(avg_age_2021, na.rm = TRUE)) %>% 
  pull(avg_age_2021)

max_avg_age_2022 <- data2022 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_age_2022 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_age_2022 == max(avg_age_2022, na.rm = TRUE)) %>% 
  pull(avg_age_2022)

list(highest_max_avg_age_2020, max_avg_age_2021, max_avg_age_2022)

#lowest average value of total symptoms by age 
min_avg_age_2020 <- data2020 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_age_2020 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_age_2020 == min(avg_age_2020, na.rm = TRUE)) %>% 
  pull(avg_age_2020)

min_avg_age_2021 <- data2021 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_age_2021 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_age_2021 == min(avg_age_2021, na.rm = TRUE)) %>% 
  pull(avg_age_2021)
#final value
lowest_min_avg_age_2022 <- data2022 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_age_2022 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_age_2022 == min(avg_age_2022, na.rm = TRUE)) %>% 
  pull(avg_age_2022)

lowest_min_avg_age_2022 = round(lowest_min_avg_age_2022, 2)

list(min_avg_age_2020, min_avg_age_2021, lowest_min_avg_age_2022)

#Highest average value of reported total symptoms by race 
#final value
highest_max_avg_race_2020 <- data2020 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_race_2020 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_race_2020 == max(avg_race_2020, na.rm = TRUE)) %>% 
  pull(avg_race_2020)

 highest_max_avg_race_2020 = round(highest_max_avg_race_2020, 2)

max_avg_race_2021 <- data2021 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_race_2021 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_race_2021 == max(avg_race_2021, na.rm = TRUE)) %>% 
  pull(avg_race_2021)

max_avg_race_2022 <- data2022 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_race_2022 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_race_2022 == max(avg_race_2022, na.rm = TRUE)) %>% 
  pull(avg_race_2022)

list(highest_max_avg_race_2020, max_avg_race_2021, max_avg_race_2022)
#Lowest average value of reported total symptoms by race
min_avg_race_2020 <- data2020 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_race_2020 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_race_2020 == min(avg_race_2020, na.rm = TRUE)) %>% 
  pull(avg_race_2020)

min_avg_race_2021 <- data2021 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_race_2021 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_race_2021 == min(avg_race_2021, na.rm = TRUE)) %>% 
  pull(avg_race_2021)
#final value
lowest_min_avg_race_2022 <- data2022 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_race_2022 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_race_2022 == min(avg_race_2022, na.rm = TRUE)) %>% 
  pull(avg_race_2022)

lowest_min_avg_race_2022 = round(lowest_min_avg_race_2022, 2)

list(min_avg_race_2020, min_avg_race_2021, lowest_min_avg_race_2022)

#highest average value of reported total symptoms by education
#final value 
highest_max_avg_edu_2020 <- data2020 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_edu_2020 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_edu_2020 == max(avg_edu_2020, na.rm = TRUE)) %>% 
  pull(avg_edu_2020)

highest_max_avg_edu_2020 = round(highest_max_avg_edu_2020, 2)

max_avg_edu_2021 <- data2021 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_edu_2021 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_edu_2021 == max(avg_edu_2021, na.rm = TRUE)) %>% 
  pull(avg_edu_2021)

max_avg_edu_2022 <- data2022 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Subgroup) %>% 
  summarize(avg_edu_2022 = mean(Value, na.rm = TRUE))%>% 
  filter(avg_edu_2022 == max(avg_edu_2022, na.rm = TRUE)) %>% 
  pull(avg_edu_2022)

list(highest_max_avg_edu_2020, max_avg_edu_2021, max_avg_edu_2022)

summary_information <- list(highest_max_avg_age_2020 =highest_max_avg_age_2020, highest_max_avg_race_2020 = highest_max_avg_race_2020 , highest__max_avg_edu_2020  = highest_max_avg_edu_2020, lowest_min_avg_race_2022 = lowest_min_avg_race_2022, lowest_min_avg_age_2022 = lowest_min_avg_age_2022)
