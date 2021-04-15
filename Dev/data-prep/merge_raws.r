library(tidyverse)

path = "data/raw-historical"

files_listed <- list.files(path)

print(files_listed)

combined <- read_csv(paste(path, files_listed[1], sep = "/")) %>%
  mutate(created_utc = as.character(created_utc),
         score = as.character(score),
         num_comments = as.character(num_comments))



files_listed <-files_listed[-1] #removes the first file

for (file in files_listed){
  opened_file <- read_csv(paste(path, file, sep = "/")) %>%
    mutate(created_utc = as.character(created_utc),
           score = as.character(score),
           num_comments = as.character(num_comments))
  combined <- bind_rows(combined, opened_file)
}

combined <- unique(combined)

write_csv(combined, "data/raw_merged.csv")