library(tidyverse)

path = "data/raw-historical"

files_listed <- list.files(path)

print(files_listed)

combined <- read_csv(paste(path, files_listed[1], sep = "/")) %>%
  mutate(created_utc = as.integer(created_utc),
         score = as.integer(score),
         num_comments = as.integer(num_comments))

if("upvote_ratio" %in% names(combined)){
  combined <- combined %>%
    subset(select=-(upvote_ratio))
}


files_listed <-files_listed[-1] #removes the first file

for (file in files_listed){
  print(file)
  opened_file <- read_csv(paste(path, file, sep = "/")) %>%
    mutate(created_utc = as.integer(created_utc),
           score = as.integer(score),
           num_comments = as.integer(num_comments))
  
  if("upvote_ratio" %in% names(opened_file)){
    opened_file <- opened_file %>%
      subset(select=-(upvote_ratio))
  }
  combined <- bind_rows(combined, opened_file)
}

combined <- unique(combined) %>%
  mutate(created_utc = strptime(created_utc, format="%s")) %>%
  subset(select=-c("all_awardings", "score"))

write_csv(combined, "data/wsb_dd_submissions.csv")