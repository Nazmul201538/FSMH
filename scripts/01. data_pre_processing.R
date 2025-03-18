#load packages

library(tidyverse)
library(readxl)
library(dplyr)


#load Data
data <- read_excel("raw_data/FSMH_raw.xlsx")

#changing Column Names
sections <- data |> select(16:42)
colnames(sections) <- paste0("Q", 16:42)

#Determining the Effects of Financial Stress on Mental Health 

library(stringr)
sections <- sections |> mutate(across(Q16:Q24, ~ str_trim(.)))

effects <- sections |> select(Q16:Q24) |> mutate(across(Q16:Q24, ~ case_when(
  . == "Agree" ~ 1,
  . == "Disagree" ~ 0,

  TRUE ~ NA_real_
))) |>
  
  mutate(effects_mean= mean(c_across(Q16:Q24), na.rm = TRUE))

unique(sections$Q16)

#Determining Stress Level with PERCEIVED STRESS SCALE

stress <- stress %>%
  mutate(across(Q25:Q34, ~ trimws(tolower(.))))


unique(sections$Q25)

  stress <- sections |> 
     select(Q25:Q34) |> 
   mutate(across(Q25:Q34, ~ case_when(
         . == "Never; কখনো না" ~ 4,
         . == "Almost Never; খুবই অল্প সময়" ~ 3,
         . == "Sometimes; মাঝে মাঝে" ~ 2,
         . == "Fairly Often; প্রায়ই" ~ 1,
         . == "Very Often; খুব বেশি" ~ 0,
         TRUE ~ NA_real_
       ))) |> 
    rowwise() |>  
     mutate(PSS_Score = sum(c_across(Q25:Q34), na.rm = TRUE))
  
  mutate(Stress_status = case_when(
    PSS_Score (1-13) ~ "Low",
    PSS_Score (14-26) ~ "Moderate",
    PSS_Score (27-40) ~ "High",
  ))
     
     
