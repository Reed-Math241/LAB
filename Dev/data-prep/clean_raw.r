library(tidyverse)

wsb_dd_submissions <- read_csv("Data/raw_merged.csv")


clean <- wsb_dd_submissions %>%
  mutate(created_utc = strptime(created_utc, format="%s")) 


write_csv(clean, "Data/wsb_dd_submissions.csv")

