library(tidyverse)

wsb_dd_submissions <- read_csv("Dev/Scraping/wsb_dd_submissions.csv")


clean <- wsb_dd_submissions %>%
  unique() %>%
  subset(select=-c(index)) %>%
  mutate(created_utc = strptime(created_utc, format="%s")) 


write_csv(clean, "Data/wsb_dd_submissions.csv")