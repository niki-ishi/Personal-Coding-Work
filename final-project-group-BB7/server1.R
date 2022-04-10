library(ggplot2)
library(plotly)
library(dplyr)

# -------------------------- Original Data ------------------------------
data1 <- read.csv("Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")

# -------------------------------- Server -----------------------------
server <- function(input, output) {
  
  output$chart1Plot <- renderPlotly({
    
    # ------------------------- Manipulated data ------------------------
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
    
    # ---------------------- Chart 1 Data -------------------------------
    chart1_data <- final_table %>% filter(Group == "By Race/Hispanic ethnicity") %>% filter(Subgroup %in% input$user_category)
    
    library(ggplot2)
    custom_colors <- c("#afbcde", "#b3d6e6", "#b3a8e0", "#f7d1ad", "#a7d6c8")
    
    # ---------------------- Chart 1 ------------------------------------
    chart1 <- ggplot(chart1_data) +
      geom_col(aes(x = year, y = avg, fill = Subgroup), position = "dodge") +
      labs(title = "Mental Health by Race During COVID-19 (2020-2021)", x = "Year of the Pandemic", y = "Avg % of race with mental health symptoms", fill = "Race") + 
      scale_fill_manual(values = custom_colors) + 
      coord_flip() 
    
    # ---------------------- Interactive plot ---------------------------
    plotly_plot1 <- ggplotly(chart1)
    
    return(plotly_plot1)
    
  })
}
