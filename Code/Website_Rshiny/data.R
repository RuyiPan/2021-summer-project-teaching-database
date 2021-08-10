# my.dir <- readline(prompt="working directory")
# # /Users/dimu/Dropbox/PanMu2021/Code/Shiny_Descriptive_Analysis
# setwd(my.dir)
# # getwd()

######### read the csv #########
require(readxl)
source('dependencies.R')
intro = read.csv('intro.csv')
data = read_excel('data/merged_data_NR_Aug3.xlsx')



###################

data1 = data %>%
  filter(!is.na(Publication_Date)) %>%
  filter(!is.na(`Quality (NR) (1 best)`))%>%
  filter(!is.na(`Statistical Sophistication (1 best)`))%>%
  dplyr::select(-c('Scenario', 'Result', 'Conclusion','...10','...11'))%>%
  separate_rows(General_Concept, sep = ",")%>%
  mutate(General_Concept = ifelse(General_Concept == 'Probability ', 'Probability', 
                                  ifelse(General_Concept == ' Probability',  'Probability', General_Concept)))%>%
  mutate(General_Concept = ifelse(General_Concept == 'Regression ', 'Regression', 
                                  ifelse(General_Concept == ' Regression',  'Regression', General_Concept)))%>%
  mutate(General_Concept = ifelse(General_Concept == 'Statistics ', 'Statistics', 
                                  ifelse(General_Concept == ' Statistics',  'Statistics', General_Concept))) %>%
  mutate(Publication_Year = lubridate::year(Publication_Date)) %>%
  mutate(Course_Year = lubridate::year(Course_Date)) 



d3 = data1 %>%
  ungroup()%>%
  # select(Id, Course_Date, Course_Year, 
  #        Publication_Date,Publication_Year,
  #        Scientific_Field)%>%
  group_by(Id) %>%  slice(1)%>%
  arrange(Id)%>%
  mutate(group = ifelse(Publication_Year < 2006, 1, 
                        ifelse(Publication_Year < 2011, 2,
                               ifelse(Publication_Year < 2016, 3, 4)
                        )
  )
  )%>%
  mutate(label= ifelse(Publication_Year < 2006, '<= 2005', 
                       ifelse(Publication_Year < 2011,'2006-2010',
                              ifelse(Publication_Year < 2016, '2011-2015',
                                     '2015+')
                       )
  )
  )%>%
  mutate(label2 = ifelse(Course_Year < 2006, '<= 2005', 
                         ifelse(Course_Year  < 2011,'2006-2010',
                                ifelse(Course_Year  < 2016, '2011-2015',
                                       '2015+')
                         )
  )
  )


