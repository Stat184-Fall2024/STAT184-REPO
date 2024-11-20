install.packages("tidyverse")
library(tidyverse)
install.packages("tidyr")
install.packages("readxl")
library(readxl)
library(rvest)

install.packages('googlesheets4') 
library(googlesheets4)

gs4_deauth()
US_Armed_Forces1 <- read_sheet(
  ss = 'https://docs.google.com/spreadsheets/d/1cn4i0-ymB1ZytWXCwsJiq6fZ9PhGLUvbMBHlzqG4bwo/edit?gid=597536282#gid=597536282'
)

US_Armed_Forces1 <- US_Armed_Forces1[-c(12, 18, 29:31), ]

US_Armed_Forces1 <- US_Armed_Forces1 %>%
  dplyr::select(-c(4, 7, 10, 13, 16:19)) %>%
  rename (
    PayGrade = "Active-Duty Personnel by Service Branch, Sex, and Pay Grade",
    Army.Male = "...2",
    Army.Female = "...3",
    Navy.Male = "...5",
    Navy.Female = "...6",
    Marines.Male = "...8",
    Marines.Female = "...9",
    AirForce.Male = "...11",
    AirForce.Female = "...12",
    SpaceForce.Male = "...14",
    SpaceForce.Female = "...15",
  )

US_Armed_Forces1 <- US_Armed_Forces1[-c(1:2), ]

Cleaned_Armed_Forces1 <- US_Armed_Forces1 %>%
  pivot_longer(
    cols = (Army.Male:SpaceForce.Female),
    names_to = ("BranchGender"),
    values_to = "count"
  ) %>%
  separate_wider_delim(
    cols = ("BranchGender"),
    delim = ".",
    names = c("Branch", "Gender")
  )


namesRaw <- read_html("https://neilhatfield.github.io/Stat184_PayGradeRanks.html") %>%
  html_elements(css = "table") %>%
  html_table()

names <- namesRaw[[1]]

names <- names[-c(26), ]

namesCleaned <- names %>%
  rename(
    junk = 1,
    Army = 3,
    Navy = 4,
    Marines = 5,
    AirForce = 6,
    SpaceForce = 7,
  ) %>%
  dplyr::select(
    c(2:7)
  ) %>%
  pivot_longer(
    cols = c(Army:SpaceForce),
    names_to = "Branch",
    values_to = "Title"
  )

namesCleanaed <- namesCleaned[-c(1:5), ]



Merged_Armed_Forces <- Cleaned_Armed_Forces1 %>%
  left_join(
    namesCleaned,
    join_by("PayGrade" == "Pay Grade", Branch == Branch),
  ) 

mergedArmedForcesNoNa <- Merged_Armed_Forces %>%
  filter(!is.na(count))

mergedArmedForcesNoNa %>%
  mutate(
    count = parse_number(count)
  )

armedForcesIndividual <- uncount(data = mergedArmedForcesNoNa, weights = count, .remove = TRUE)




Cleaned_Armed_Forces1$coefficients %>%
  kable() %>%
  kableExtra::kable_classic()
  
