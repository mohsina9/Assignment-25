#load packages
library(tidyverse)
library(readxl)
library(openxlsx)

#load data
data<-read_excel("raw_data/AMR_KAP_Data (2).xlsx")


# section separation

demographic <- data |> 
  select(1 : 11) |> 
  #converting data into factor
  mutate(across(1:11, as.factor)) 



#checking data structure
glimpse(demographic)  

#knowledge
#selecting knowledge questions from data


knowledge <- data |> 
  select(12 : 23) 

knowledge <- knowledge |> 
  mutate(across(c(1,2,3,6,8,9,10,11,12) , ~ case_when(
    . == "Don't Know" ~ 0,
    . == "Yes" ~ 1,   
    . == "No" ~ 0,
    TRUE ~ NA_real_
  ))) |>  
  mutate(across(c(4, 5, 7) , ~ case_when(
    . == "Don't Know" ~ 0,
    . == "Yes" ~ 0,   
    . == "No" ~ 1,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |>  # calculating a person's total knowledge score (in percentage)
  mutate(Knowledge_Score = mean(c_across(1 : 12)*100, na.rm = TRUE
  )) |> 
  mutate(Knowledge_Level = case_when(    #Grading a person's knowledge level
    Knowledge_Score <= 49 ~ "Poor", 
    Knowledge_Score > 49 & Knowledge_Score <80 ~ "Moderate",
    Knowledge_Score >= 80 ~ "Good"
  ))   


unique(knowledge$`Antibiotic kills the bacteria(Yes)`)  #checking options of values
glimpse(knowledge)  #checking data structure



#attitude

attitude <- data |> 
  select(24 : 33)


attitude <- attitude |>  # re-coding 
  select(1 : 10) |> 
  mutate(across(1 : 10, ~ case_when(
    . == "Disagree"  ~ 1,
    . == "Agree"  ~ 0,   
    . == "Neutral"  ~ 0,
    TRUE ~ NA_real_
  ))) |> 
  rowwise() |>  # calculating a person's total attitude score (in percentage)
  mutate(Attitude_Score = mean(c_across(1 : 10)*100, na.rm = TRUE
  ))|> 
  mutate(Attitude_Level = case_when(    #Grading a person's attitude level
    Attitude_Score <= 49 ~ "Negative", 
    Attitude_Score > 49 & Attitude_Score <80 ~ "Uncertain",
    Attitude_Score >= 80 ~ "Positive"
  ))

unique(attitude$`A child with cold is given antibiotics(Disagree)`)  #checking options of values
glimpse(attitude)  #checking data structure



#practice

practice <- data |> 
  select(34 : 39)


practice <- practice |>  # re-coding 
  select(1 : 6) |> 
  mutate(across(c(1, 2, 6), ~ case_when(
    . == "Yes" ~ 1,   
    . == "No" ~ 0,
    TRUE ~ NA_real_
  ))) |> 
  mutate(across( 3 : 5, ~ case_when(
    . == "Yes" ~ 1,   
    . == "No" ~ 0,
    TRUE ~ NA_real_))) |> 
  rowwise() |>  # calculating a person's total practice score (in percentage)
  mutate(Practice_Score = mean(c_across(1 : 6)*100, na.rm = TRUE
  ))|> 
  mutate(Practice_Level = case_when(
    Practice_Score <= 79 ~ "Misuse",  #Grading a person's practice level
    Practice_Score >= 80 ~ "Good"
  ))

unique(practice$" My child should complete a given dose, even he improve after 2 dose(Yes)")  #checking options of values
glimpse(practice)  #checking data structure



#source
source <- data |> 
  select(40 : 49)

glimpse(source)  #checking data structure


# combine all sections
clean_data <- cbind(demographic,knowledge, attitude, practice, source)

clean_data <- clean_data |> 
  mutate(across(c(25, 37, 45, 46), as.factor))  # converting character data into factor

glimpse(clean_data)  #checking data structure


# export the data as csv
write.csv(clean_data, "clean_data/clean_data.csv", row.names = FALSE)

# export the data as excel
write.xlsx(clean_data, "clean_data/clean_data.xlsx", rowNames = FALSE)

