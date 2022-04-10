library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)

#------------------------- Original Dataset -----------------------------
data1 <- read.csv("Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")

# ----------------------- Manipulated Data ---------------------------------
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

final_table <- rbind(avg_age_2020, avg_age_2021, avg_age_2022, avg_dis_2021, avg_dis_2022, avg_edu_2020, avg_edu_2021, avg_edu_2022, avg_gen_2021, avg_gend_2022, avg_ori_2021, avg_ori_2022, avg_race_2020, avg_race_2021,  avg_race_2022, avg_sex_2020, avg_sex_2021, avg_sex_2022)

final_table$avg <- (round(final_table$avg, digits = 2))


kable(final_table) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# -------------------------- Chart 1 Data -------------------------------
chart1_data <- final_table %>% filter(Group == "By Race/Hispanic ethnicity") 

# -------------------------- Chart 2 Data -------------------------------
chart2_data <- rbind(avg_age_2020, avg_age_2021, avg_age_2022)

#---------------------------- ui ----------------------------------------
intro_tab <- tabPanel(
  "Introduction",
  fluidPage(theme = bs_theme(bootswatch = "lux"),
            tags$head(tags$style(HTML("a {color: blue}"))),
            h1("Mental Health During the COVID-19 Pandemic"), 
            p("Sabrina Lin, Lily He, Niki Ishisaki, Vy Nguyen"), 
            p("INFO 201 Final Project"),
            p("March 10th, 2022"), 
            tags$img(src = "https://assets.teenvogue.com/photos/5ec2ea33939ed3e75497b6dd/16:9/w_2560%2Cc_limit/GettyImages-1179507984.jpg", width = "600px", height = "300px"),
            p("Source:", tags$a(href = "https://www.teenvogue.com/story/coronavirus-pandemic-mental-health", "https://www.teenvogue.com/story/coronavirus-pandemic-mental-health")), 
            h3("Introduction"),
            p("Our main question is: \"Are certain ethnicities, gender identities, sexual orientations, educational-levels, disabilities, or ages more impacted by the effects of COVID-19 as seen in their mental health? If so, why?\" To address this concern, we plan to analyze the correlation between mental health disorders during the COVID-19 pandemic and various social identities. This is important because analyzing the data is the first step in understanding the inequities in mental health care and moving towards more access to resources for marginalized communities."), 
            p("The COVID-19 pandemic has been ongoing for the past 2 years with over a million cases worldwide. Because of this airborne disease, many governors in the U.S chose to lockdown public areas, enforce social distancing, and implement mask mandates. Those with more access to healthcare resources and health education were able to access COVID-19 testing, vaccination sites, and other resources relating to the pandemic. Marginalized communities are more likely to live in larger multigenerational households, be essential workers, and endure other barriers preventing them from following CDC guidelines. As a result of social distancing and isolation, many datasets have shown a spike in anxiety and depression symptoms among the population in the last two years. Our question lies around whether or not there is a correlation between the increase in mental health disorders and ethnicities, gender identities, sexual orientations, educational-levels, disabilities, and ages."),
            p("More specifically, our research questions include:"),
            p("1. Are certain ethnicities, gender identities, sexual orientations, educational-levels, disabilities, or ages more impacted by the effects of COVID-19 as seen in their mental health? If so, why?"),
            p("2. Is the percent of depressive or anxiety symptoms different among populations after vaccine distribution? If so, why?"),
            p("3. Which social identities have the largest gap in mental health symptoms between subgroups?"),
            p("We are interested in the research questions above because of the health inequities that have magnified from the COVID-19 pandemic, making marginalized communities more susceptible to the effects of COVID-19. It is important for the general public to know these trends in order for them to act on it to promote equitable change for vulnerable populations. More specifically, it is important for advocates, change-makers, and susceptible communities like college students and marginalized racial groups to know this information, since it provides the first steps to addressing the systemic issue seen in mental health trends during the pandemic."),
            h3("Related Work"),
            p("The spread of COVID-19 quickly turned into a worldwide pandemic and, as a result, America was required to take immediate action and instill a nationwide lockdown. The pandemic challenged many by confining them, but also led others to continue essential work during these dangerous times. With these new living conditions, we wanted to understand if there was a correlation between the rise of mental health disorders, like anxiety and depression, during the pandemic and social identities like race, sexual orientation, and educational level. In relation to our topic, an article was published by", tags$a(href = "https://www.forbes.com/sites/williamhaseltine/2021/04/29/understanding-the-neurological-and-psychological-effects-of-covid-19/?sh=3a2b13672090", "Forbes"), "in which William Haseltine emphasizes the lesser known side effects of COVID-19 itself. These include mental health struggles such as PTSD, depression, anxiety, and more rarely brain fog."),
            p("Additionally", tags$a(href ="https://www.kff.org/coronavirus-covid-19/issue-brief/the-implications-of-covid-19-for-mental-health-and-substance-use/", "KFF"), "published a research article observing the negative impacts of mental health trends in adults during the pandemic. In their article, the data compares household survey results of adults who reported symptoms of mental illnesses such as depression before and after the pandemic. In contrast, a case study observed by the", tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/34798148/", "Department of Biostatistics and Health Information"), "in the UK looked at the long term effects of COVID-19. From their observations, they were able to conclude that the disease itself did not contribute to increasing psychological symptoms. These related works portray various research and results on how COVID-19 affects individuals, as well as how the pandemic has raised psychological symptoms and concerns."), 
            h3("The Dataset"), 
            p("Our dataset:", tags$a(href ="https://catalog.data.gov/dataset/indicators-of-anxiety-or-depression-based-on-reported-frequency-of-symptoms-during-last-7-", "Indicators of Anxiety or Depression Based on Reported Frequency of Symptoms During Last 7 Days"), 
            p("The data is found in the open data source from the U.S. Government and is collected by the U.S. Census Bureau. It was conducted by an internet questionnaire called", tags$a(href = "https://www.census.gov/programs-surveys/household-pulse-survey/data.html", "Household Pulse Survey"), "."), 
            p("The questionnaire focuses on the pandemic's impact on employment status, consumer spending, food security, housing, education disruptions, and dimensions of physical and mental health. This particular dataset centers on psychological wellness, and contains 14 observations and 9135 features. It captures a subset of metadata relating to the frequency of anxiety and depression symptoms under questions on mental wellness."), 
            p("The U.S. Census Bureau sent invitations to the survey by email and text message. For households with multiple email addresses or cell phone numbers, the U.S. Census Bureau sent invitations randomly, and they gathered only one response for each household."), 
            p("The purpose of the data is to measure the impacts of COVID-19 for American households weekly and to provide timely data aiding the recovery of the pandemic."), 
            p("However, there are still some problems that this dataset might not address in the dataset. People who cannot understand English might ignore the email and text message, causing a lack of representation of certain groups of people in the data, particularly people of color. In general, stigma on mental health or cultural differences might lead to an underreporting of anxiety or depressive symptoms for certain respondents. Moreover, the random collection of responses in multi-resident households excludes incidences of domestic violence, and the dataset might not collect responses of victims of domestic violence. It would not gather data for homes with multiple residents of diverse social identities. It also neglects people who lack internet or proper electronic devices for receiving survey invitations, which would hinder people with lower socioeconomic status from filling out the survey and getting counted in the data."),
            p("The survey is designed to be short and covers a broader range of questions, thus making the dataset lack intersectionality. It was not inclusive in determining respondents' sexual orientations or race in terms of categorizing. As mentioned above, the dataset does not represent oppressed groups who do not have access to the survey invitation."),
            h3("Implications"),
            p("Assuming we answer our research questions, possible implications for technologists could be implementing apps or websites to make telehealth more accessible for marginalized communities, allowing for greater access to one-on-one interactions with a therapist without increasing COVID-19 exposure with in-person contact."), 
            p("Possible implications for designers could be designing COVID-19 health education resources (for example, infographics, posters, etc) intended to reach marginalized communities to improve their mental health knowledge and health outcomes. The designers could include some information from our data analysis to inform audiences about the associations between mental health during the pandemic and social identities, and provide resources for how to go about getting the mental health care they need. Another implication for designers could be designing more accessible mental health clinics in areas with higher populations of underserved communities in hopes of improving mental health outcomes and providing much-needed resources."), 
            p("Possible implications for policymakers could be passing policies to expand insurance coverage to make it more accessible to marginalized communities. Health insurance is one of the barriers in healthcare access, so removing this barrier could help marginalized communities improve their mental health. Promoting mental health education in communities will destigmatize help-seeking behavior in getting the professional help they need."),
            h3("Limitations and Challenges"), 
            p("Some of the limitations we might need to address with our project are the missing or generalized voices from the dataset that may not lead to inclusive results in our ultimate analysis. The main goal of this project is to analyze whether or not marginalized social identities are more likely to be affected by mental health disorders during the COVID-19 pandemic, but not all marginalized social identities are represented well in the data. For example, gay/lesbian, straight, and bisexual are the only sexual orientations considered in the dataset, so we will not be able to discuss mental health percent values of other sexual orientations like queer, pansexual, and asexual in our analysis and conclusions. Additionally, in the race category, there is a subcategory of \"Non-hispanic, other races and mixed races,\" which generalizes the complexities of race and condenses it into one category, skewing values in the dataset and thus, skewing our perceptions and analysis of the data. We can only make conclusions about what is presented in the data, and the data is flawed and biased. It is important to acknowledge these challenges and limitations to the dataset and our project in order to understand the possible biases seen in our results."), 
            tags$img(src = "https://www.muhlenberg.edu/media/contentassets/images/newscenter/magazine/MentalHealth-Illustration-4.png", width = "330px", height = "400px"),
            p("Source:", tags$a(href = "https://www.muhlenberg.edu/news/mentalhealthoncampus/", "https://www.muhlenberg.edu/news/mentalhealthoncampus/"))
            
  )
)
)
  
# ------------------------- Chart 1 ui ----------------------------------
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
  h1("Race Plot"),
  plotlyOutput(outputId = "racePlot"),
  h2("Purpose"),
  p("The purpose of the Race Plot was to show and understand how the dominant racial group's mental health symptoms compare to other racial groups' symptoms across different years of the pandemic, and if anything significantly changed over time. We chose a bar chart to show this data because we wanted to compare a continuous variable (percent of anxiety or depression symptoms or \"Value\") to a categorical variable (years of the pandemic)."), 
  h2("Analysis"), 
  p("The information in the data visualization offered several insights. One is that \"Non-Hispanic Asian\" and \"Non-Hispanic White racial groups\" have the lowest percent of mental health symptoms across all years of the pandemic. The data also shows that \"Non-Hispanic, other races and multiple races\" have the highest percent of mental health symptoms across all years of the pandemic. It is important to note that stigmatization of mental health is prevalent among Asian Americans, which could lead to reduced self-reporting of symptoms in that specific population. There is also ambiguity in the \"Non-Hispanic, other races and multiple races\" category, which could lead to skewed results.")
)

plot1_tab <- tabPanel(
  "Race Plot",
  sidebarLayout(
    plot1_sidebar,
    plot1_main
  )
)

# ------------------------- Chart 2 ui ----------------------------------
plot2_sidebar <- sidebarPanel(
  selectInput(
    inputId = "user_category2",
    label = "Select Year",
    choices = chart2_data$year,
    selected = c("2020"),
    multiple = TRUE),
  
)

plot2_main <- mainPanel(
  h1("Age Plot"),
  plotlyOutput(outputId = "agePlot"),
  h2("Purpose"),
  p("The purpose of Chart 2 was to show and understand how mental health symptoms vary by age throughout the pandemic. We chose a line chart to visualize this data because we wanted to see if there was a trend of mental health symptoms across different age groups over time. You may select multiple years by clicking the drop-down bar to view data per year. "),
  h2("Analysis"),
  p("The information in the data visualization offered several insights. There was a clear decline in mental health symptoms from the 18-29 years age group to the 80 years and above age group. Younger people tend to have the highest percentage of mental health symptoms and older people tend to have the lowest percentage. This shows that pandemic-related stress and depression affect younger people more, possibly due to its large disruption of social interactions, work, and school. Moreover, the percentage of reported depressive symptoms was the highest across all age groups in 2020. The spread of COVID-19 worldwide and lockdown mandates in 2020 took a toll on everyone's mental wellness." )
)


plot2_tab <- tabPanel(
  "Age Plot",
  sidebarLayout(
    plot2_sidebar,
    plot2_main
  )
 
)

# ------------------------- Chart 3 ui ----------------------------------
plot3_sidebar <- sidebarPanel(
  radioButtons(
    inputId = "user_choice",
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
  h1("Social Identities Plot"),
  plotlyOutput(outputId = "myplot"),
  tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  tags$head(tags$style(".shiny-output-error:after{content: 'Invalid year. Please try another';
visibility: visible}")),
  p("Note: The four main educational levels represented in the \"By Education\" graph are (1) Less than a high school diploma, (2) High school diploma or GED, (3) Some college/Associate's degree, and (4) Bachelor's degree or higher."),
  h2("Purpose"),
  p("The purpose of this social identities plot is to help us demonstrate and understand how different social groups are affected by mental health symptoms throughout the COVID-19 pandemic. These different groups are categorized by education, sex, sexual orientation, gender identity, and disability. These various groups can be further broken down into subgroups relating to each group (for example, male and female for sex category), which can be seen by selecting the radio button correlating to one of the groups. Additionally, a drop down menu can be used to select a year ranging from 2020-2022 to showcase the data across different years of the pandemic. However, due to some inconsistencies in the original dataset, some groups do not have visual data for some years, and a \"Invalid Year. Please try another.\" message will appear if you choose a year that isn't represented in that specific group."),
  h2("Analysis"),
  p("The information in the data visualization offered several interesting insights. When looking at mental health levels based on disability, we found that people with disabilities are more than two times as likely to have anxiety and depression symptoms during the pandemic in 2021 and 2022 compared to people without disabilities. This is an extremely significant difference. When looking at mental health levels based on sex, we found that females were ~ 6% more likely to have mental health symptoms compared to males in 2021 and 2022."), 
  p("Among the educational levels, we found that there was not a strong correlation between mental health symptoms and educational level. Surprisingly, when looking at educational levels, we found that people with some college or an Associate's degree had higher levels of mental health symptoms compared to people with a high school diploma or a GED in 2021 and 2022. Unsurprisingly, we found that people who earned a Bachelor's degree or higher had the lowest average percent of mental health symptoms in 2021 and 2022 compared to other educational levels."), 
  p("For gender identity, we found that transgender individuals had over twice as high average percent mental health symptoms compared to cis-gender individuals in all pandemic years represented (2021 and 2022). Lastly, for sexual orientation, we found that bisexual people had higher average percent mental health symptoms compared to gay, lesbian, and straight people in 2021. This was surprising, since we expected gay, lesbian, and bisexual mental health percentages to be similar, but there was a ~ 13% gap."),
  p("Ultimately, after analyzing the mental health trends of different social groups, we found that minority groups (people with disabilities, females, people in the lowest educational-level, transgender individuals, and bisexual people) were more likely to have mental health symptoms during the pandemic compared to dominant groups (people without disabilities, males, people in the highest educational-level, cis-gender individuals, and straight people etc). This uncovers the significant mental health inequities seen among social groups, even during the COVID-19 pandemic.")
)

plot3_tab <- tabPanel(
  "Social Identities Plot",
  sidebarLayout(
    plot3_sidebar,
    plot3_main
  )
)

# ------------------------------- Conclusion ----------------------------------
conclusion_sidebar <- sidebarPanel(
  h3("Research Questions"),
  p("We had several key takeaways that tied back to our original research questions:"),
  p("1. Are certain ethnicities, gender identities, sexual orientations, educational-levels, disabilities, or ages more impacted by the effects of COVID-19 as seen in their mental health? If so, why?"),
  p("2. Is the percent of depressive or anxiety symptoms different among populations after vaccine distribution? If so, why?"),
  p("3. Which social identities have the largest gap in mental health symptoms between subgroups?"),
)

conclusion_main <- mainPanel(
  h1("Conclusion"),
  tags$img(src = "https://themighty.com/wp-content/uploads/2020/10/GettyImages-1255250589-1280x640.jpg", width = "600px", height = "300px"),
  p("Source:", tags$a(href = "https://themighty.com/2020/10/mental-health-is-physical-health/", "https://themighty.com/2020/10/mental-health-is-physical-health/")),
  h2("Three Specific Takeaways"), 
  h4("Takeaway #1"), 
      p("Being a member of marginalized groups contributes to higher risk for mental health issues. Moreover, the pandemic further exacerbates their vulnerability due to isolations and social injustices."), 
      p("For example, when comparing the Non-Hispanic White racial group to the Non-Hispanic Black racial group in the Race Plot, we find that there is a ~ 3-4% difference in percent mental health from 2020 to 2022. The Black racial group had the higher average percent mental health each year. Additionally, when looking at mental health levels based on sex, we found that females were ~ 6% more likely to have mental health symptoms compared to males in 2021 and 2022. These trends are apparent in almost all of our visualizations, highlighting the major differences in mental health symptoms between different social identities. Thus, these disparities in mental health seen in our visualizations tell us that certain ethnicities, gender identities, sexual orientations, educational-levels, disabilities, and ages are much more impacted by the effects of COVID-19 as seen in their mental health."),
      h4("Takeaway #2"), 
          p("The percentage of reported depressive symptoms was the highest in 2020, which is shown in Age Plot and Race Plot. For instance, the reported depressive symptoms in the 18 to 29 age group was 44.36% in 2020 and was  41.04 % in 2022. It also displayed an overall decreasing trend over time, partly thanks to the distribution of vaccines. Due to the availability of vaccines and reduction in death and case numbers, the government lifted lockdown mandates and people feel more safe to socialize with friends and others."),
          h4("Takeaway #3"),
              p("Disability status and gender identity are the top two social identity categories with the largest gap in mental health symptoms between subgroups. These social identities are analyzed in the Social Identities Plot. Approximately 56% of people with disabilities had depressive symptoms and ~ 22% of people without disabilities had depressive symptoms in 2021, which means there was a 34% gap. The percentages for 2022 were not much different (~ 55% and ~ 23% respectively). Additionally, when comparing mental health based on gender identity, we found that transgender individuals (58%) reported 32% higher in depressive symptoms than cisgender male (23%). This is an incredibly large gap, and it is important to close this gap in order to promote health equity and better health outcomes among underserved populations."),
      h2("Most Important Insight"), 
      p("The most important insight from our analysis was that minority groups (for example, people with disabilities, females, people in the lowest educational-level, transgender individuals, Black people, and bisexual people) have higher average percent mental health symptoms compared to non-minority groups (for example, people without disabilities, males, people in the highest educational-level, cis-gender individuals, White people, and straight people). This is seen in all social groups represented in our data visualizations, although educational level does not seem to strongly correlate with mental health status during the COVID-19 pandemic. Additionally, we found that younger generations are likely to have higher percentages of mental health symptoms when analyzing the Age Plot. It is saddening to see that younger generations have higher symptoms of mental health disorders compared to other age groups. Knowing this allows for further research questions to come into play such as \"Why do younger generations tend to exhibit higher levels of mental health symptoms?\""),
      h2("Broader Implications"), 
      p("Some broader implications of our insight could be giving minority groups more resources to increase healthcare accessibility in order to improve mental health equity across all groups. This could include insurance coverage for mental health services, access to the low cost and relatively effective mental health services, and have people be more mindful of prejudice in the healthcare field in general. This would ultimately improve the mental health of these minority groups and close the mental health gap that stems from inequitable barriers in healthcare."),
  tags$img(src = "https://www.evidentlycochrane.net/wp-content/uploads/2020/02/Featured-image-visual-blog-Depressed-Girl-Blog-Header-01-1-e1580916482139.jpg", width = "600px", height = "300px"),
  p("Source:", tags$a(href = "https://www.evidentlycochrane.net/picturing-mental-health/", "https://www.evidentlycochrane.net/picturing-mental-health/")), 
  
)

conclusion_tab <- tabPanel(
  "Conclusion",
  sidebarLayout(
    conclusion_sidebar,
    conclusion_main
  )
)

ui <- navbarPage(
  "COVID-19 Mental Health",
  intro_tab,
  plot1_tab, 
  plot2_tab, 
  plot3_tab, 
  conclusion_tab
)
