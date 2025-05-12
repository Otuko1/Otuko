library(googlesheets4)

gs4_auth()

url <- ("https://docs.google.com/spreadsheets/d/180t3kXSIfpKp90i5-11yIAZpzca2me8tyCnakW0Mp4w/edit?gid=0#gid=0")

data_historical <- read_sheet(url)

colnames(data_historical)

library(tidyverse)

data <- data_historical %>% select("Country","Program","Gender","Age","Year")

table(data)

unique(data$Country)
unique(data$Program)
unique(data$Country)

data_mutate <- data %>% mutate(Program = 
                  case_when(Program=="Community Leadership"~"Community leadership",
                                    Program=="Digital Business"~"Digital Business",
                                    Program=="Digital Jobs Level 1"~"Digital Jobs L1",
                                    Program=="Digital Jobs Level 2"~"Digital Jobs L2",
                                    Program=="Digital Jobs Level 1 & 2"~"Digital Jobs L1&L2",
                                    Program=="Social Entreprise"~"Social Entrepreneurship",
                                    Program=="Social Enterprise MVP"~"Social Entrepreneurship",
                                    Program=="Social Enterprise Prototype"~"Social Entrepreneurship",
                                    Program=="Street Teams"~"Street Teams",
                            Program=="YLAB/Street team"~"Street Teams",
                            Program=="YLAB"~"YLAB"), 
                  Gender = case_when(Gender =="Male"~"Male",Gender=="MALE"~"Male", Gender=="male"~"Male",
                                     Gender=="Female"~"Female", Gender=="FEMALE"~"Female",Gender=="female"~"Female",
                                     Gender=="Prefer not to say"~"Female"))






unique(data_mutate$Program)

table(data_mutate$Program)
count(Program)

unique(data_mutate$Gender)


data_mutate %>% group_by(Program, Gender) %>% summarise(count = n()) %>%
  pivot_wider(names_from = Gender, values_from = count, values_fill = 0)

data_mutate %>% group_by(Country,Program, Gender) %>% summarise(count = n()) %>%
  pivot_wider(names_from = Gender, values_from = count, values_fill = 0)

data_mutate %>% group_by(Program, Gender) %>% summarise(Average_age = mean(Age)) %>%
  pivot_wider(names_from = Gender, values_from = Average_age, values_fill = 0)

data <-data_mutate %>% group_by(Year,Country, Program, Gender) %>% summarise(count = n())




