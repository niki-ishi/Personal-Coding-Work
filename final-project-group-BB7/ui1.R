library(ggplot2)
library(plotly)
library(bslib)

# -------------------------- data manipulation --------------------------
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
  mutate(year = "2020") 

avg_race_2020 <- data2020 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2020") 

avg_sex_2020 <- data2020 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2020") 

avg_edu_2020 <- data2020 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2020") 

#avg 2021
avg_age_2021 <- data2021 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

avg_race_2021 <- data2021 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

avg_sex_2021 <- data2021 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

avg_edu_2021 <- data2021 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021")  

avg_ori_2021 <- data2021 %>%
  group_by(Group ,Subgroup) %>%
  filter(Group == 'By Sexual orientation') %>%
  summarize(avg = mean(Value, na.rm = TRUE)) %>%
  mutate(year = "2021")

avg_gen_2021 <- data2021 %>%
  group_by(Group ,Subgroup) %>%
  filter(Group == 'By Gender identity') %>%
  summarize(avg = mean(Value, na.rm = TRUE)) %>%
  mutate(year = "2021")

avg_dis_2021 <- data2021 %>%
  group_by(Group ,Subgroup) %>%
  filter(Group == 'By Disability status') %>%
  summarize(avg = mean(Value, na.rm = TRUE)) %>%
  mutate(year = "2021")

#avg 2022
avg_age_2022 <- data2022 %>% 
  filter(Group == 'By Age') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

avg_race_2022 <- data2022 %>% 
  filter(Group == 'By Race/Hispanic ethnicity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

avg_sex_2022 <- data2022 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2021") 

avg_edu_2022 <- data2022 %>% 
  filter(Group == 'By Education') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

avg_dis_2022 <- data2022 %>% 
  filter(Group == 'By Disability status') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

avg_gend_2022 <- data2022 %>% 
  filter(Group == 'By Gender identity') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

avg_ori_2022 <-  data2022 %>% 
  filter(Group == 'By Sexual Orientation') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

final_table <- rbind(avg_age_2020, avg_age_2021, avg_age_2022, avg_dis_2021, avg_dis_2022, avg_edu_2020, avg_edu_2021, avg_edu_2022, avg_gen_2021, avg_gend_2022, avg_ori_2021, avg_ori_2022, avg_race_2020, avg_race_2021,  avg_race_2022, avg_sex_2020, avg_sex_2021, avg_sex_2022)

final_table$avg <- (round(final_table$avg, digits = 2))


kable(final_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

chart1_data <- final_table %>% filter(Group == "By Race/Hispanic ethnicity") 

# ---------------------------------------- ui --------------------------- 
intro_tab <- tabPanel(
  "Final Project",
  fluidPage(theme = bs_theme(bootswatch = "lux"),
            h1("Final Project"), 
            p("Sabrina Lin, Lily He, Niki Ishisaki, Vy Nguyen"), 
            p("INFO 201 Final Project"),
            p("March 11th, 2022"), 
            h1("Introduction")
  )
)

plot1_sidebar <- sidebarPanel(
  selectInput(
    inputId = "user_category",
    label = "Select Race",
    # Fill in the correct code here
    choices = chart1_data$Subgroup,
    selected = list("Non-Hispanic Black, single race", "Non-Hispanic White, single race"), 
    multiple = TRUE),
  
)

plot1_main <- mainPanel(
  h1("Chart 1 Plot"),
  plotlyOutput(outputId = "chart1Plot"),
  h2("Purpose"),
  p("The purpose of Chart 1 was to show and understand how the dominant racial group's mental health symptoms compare to other racial groups' symptoms across different years of the pandemic, and if anything significantly changed over time. We chose a bar chart to show this data because we wanted to compare a continuous variable (percent of anxiety or depression symptoms or \"Value\") to a categorical variable (years of the pandemic)."), 
  h2("Analysis"), 
  p("The information in the data visualization offered several **insights**. One is that \"Non-Hispanic Asian\" and \"Non-Hispanic White racial groups\" have the lowest percent of mental health symptoms across all years of the pandemic. The data also shows that \"Non-Hispanic, other races and multiple races\" have the highest percent of mental health symptoms across all years of the pandemic. It is important to note that stigmatization of mental health is prevalent among Asian Americans, which could lead to reduced self-reporting of symptoms in that specific population. There is also ambiguity in the \"Non-Hispanic, other races and multiple races\" category, which could lead to skewed results.")
)

plot1_tab <- tabPanel(
  "Chart 1 Plot",
  sidebarLayout(
    plot1_sidebar,
    plot1_main
  )
)

plot2_tab <- tabPanel(
  "Chart 2",
  h1("insert plot 2 stuff here"),
)


ui <- navbarPage(
  "Mental Health During COVID-19",
  intro_tab,
  plot1_tab, 
  plot2_tab
)
