library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)

data1 <- read.csv("Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")

# Manipulated Table
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
  mutate(year = "2022") 

avg_sex_2022 <- data2022 %>% 
  filter(Group == 'By Sex') %>% 
  group_by(Group ,Subgroup) %>% 
  summarize(avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(year = "2022") 

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

full_table <- rbind(avg_age_2020, avg_age_2021, avg_age_2022, avg_dis_2021, avg_dis_2022, avg_edu_2020, avg_edu_2021, avg_edu_2022, avg_gen_2021, avg_gend_2022, avg_ori_2021, avg_ori_2022, avg_race_2020, avg_race_2021,  avg_race_2022, avg_sex_2020, avg_sex_2021, avg_sex_2022)

final_table$avg <- (round(full_table$avg, digits = 2))


kable(final_table) %>% kable_styling(bootstrap_options = "striped", full_width = FALSE)

# ------------------------------ Chart 1 Data ---------------------------
chart1_data <- final_table %>% filter(Group == "By Race/Hispanic ethnicity") 

# ----------------------------- Chart 2 Data ----------------------------
chart2_data <- rbind(avg_age_2020, avg_age_2021, avg_age_2022)

# ------------------------------ Chart 3 Data -----------------------------
chart3_sex <- rbind(avg_sex_2021, avg_sex_2022)

chart3_ori <- rbind(avg_ori_2021, avg_ori_2022)

chart3_edu <- rbind(avg_edu_2020, avg_edu_2021, avg_edu_2022)

chart3_gen <- rbind(avg_gen_2021, avg_gend_2022)

chart3_dis <- rbind(avg_dis_2021, avg_dis_2022)

# ------------------------------ Server ---------------------------------
server <- function(input, output, session) {
  
  output$racePlot <- renderPlotly({
    
    # ---------------------- Chart 1 Data -------------------------------
    chart1_data <- final_table %>% filter(Group == "By Race/Hispanic ethnicity") %>% filter(Subgroup %in% input$user_category)
    
    library(ggplot2)
    custom_colors <- c("#afbcde", "#b3d6e6", "#b3a8e0", "#f7d1ad", "#a7d6c8")
    
    # ---------------------- Chart 1 ------------------------------------
    chart1 <- ggplot(chart1_data) +
      geom_col(aes(x = year, y = avg, fill = Subgroup), position = "dodge") +
      labs(title = "Mental Health by Race During COVID-19 (2020-2022)", x = "Year of the Pandemic", y = "Avg % with depressive symptoms", fill = "Race") + 
      scale_fill_manual(values = custom_colors) + 
      coord_flip() 
    
    # ---------------------- Interactive plot ---------------------------
    plotly_plot1 <- ggplotly(chart1)
    
    return(plotly_plot1)
    
  })

output$agePlot <- renderPlotly({
  # --------------------------- Chart 2 Data ---------------------------
  chart2_data <- chart2_data %>%  filter(year %in% input$user_category2) 
  
  age_plot <- ggplot(data = chart2_data) +
    geom_line(mapping = aes(x = Subgroup, y = avg, group = year, color = year)) +
    labs(
      title = " Mental Health by Age During COVID-19", x = "Ages", y = "Avg % with depressive symptoms")
  
  age_plotly_plot <- ggplotly(age_plot) %>% config(displayModeBar = FALSE)
  
  return(age_plotly_plot)
  
})

output$myplot <- renderPlotly({
    if (input$user_choice == "By Sex") {
      chart3_sex <- chart3_sex %>% filter(year %in% input$time)
      ggplot(chart3_sex, aes(Subgroup, avg)) + geom_col(fill = "pink") +
        facet_wrap(~year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % with depressive symptoms")
    } else if (input$user_choice == "By Education") {
      chart3_edu <- chart3_edu %>% filter(year %in% input$time)
      ggplot(chart3_edu, aes(Subgroup, avg)) + geom_col(fill = "light green") +
        facet_wrap(~year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % with depressive symptoms") +
        scale_x_discrete( limits = c("Less than a high school diploma", "High school diploma or GED", "Some college/Associate's degree", "Bachelor's degree or higher")) 
    }
    else if (input$user_choice == "By Disability") {
      chart3_dis <- chart3_dis %>% filter(year %in% input$time)
      ggplot(chart3_dis, aes(Subgroup, avg)) + geom_col(fill = "violet") +
        facet_wrap(~year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % with depressive symptoms")
    }
    else if (input$user_choice == "By Gender Identity") {
      chart3_gen <- chart3_gen %>% filter(year %in% input$time)
      ggplot(chart3_gen, aes(Subgroup, avg)) + geom_col(fill = "light blue") +
        facet_wrap(~year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % with depressive symptoms")
    }
    else if (input$user_choice == "By Sexual Orientation") {
      chart3_ori <- chart3_ori %>% filter(year %in% input$time)
      ggplot(chart3_ori, aes(Subgroup, avg)) + geom_col(fill = "turquoise") +
        facet_wrap(~year) +
        labs( 
          title = "Mental Health Levels During COVID-19 (2020-2022)", y= "Avg % with depressive symptoms")
    }
  })
  
}


