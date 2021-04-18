#devtools::install_github("hadley/emo")
library(tidyverse)
library(emo)

wsb <- read_csv("data/wsb_dd_submissions.csv") %>%
  mutate(emojis = unique(ji_extract_all(title)))

ji_extract_all(wsb$title[275])


