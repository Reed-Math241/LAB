#devtools::install_github("hadley/emo")

library(tidyverse)
library(emo)
library(tidytext)
library(ggplot2)



wsb <- read_csv("data/wsb_dd_submissions.csv")
nyse_common <- read_csv("Dev/data-prep/nyse_common.csv") %>%
  mutate(Symbol = tolower(Symbol))



sentiments <- get_sentiments("afinn")
calc_sentiment <- function(string){
  split <- unlist(strsplit(string, "[[:punct:] ]"))
  indexed <- match(split, sentiments$word)
  vals <- sentiments$value[indexed]
  return(sum(vals, na.rm=TRUE))
}


named_stocks <- function(string){
  split <- unlist(strsplit(tolower(string), "[[:punct:] ]"))
  indexed <- match(split, nyse_common$Symbol)
  vals <- toupper(nyse_common$Symbol[indexed])
  clean <- vals[!is.na(vals)]
  return (paste(clean, collapse = ' '))
}




wsb$title_sentiment <- map(wsb$title, calc_sentiment)%>%
  unlist()

wsb$post_sentiment <- map(wsb$selftext, calc_sentiment)%>%
  unlist()

wsb$title_stocks <- map(wsb$title, named_stocks) %>%
  unlist()
wsb$post_stocks <- map(wsb$selftext, named_stocks) %>%
  unlist()


write_csv(wsb, "data/wsb_dd_submissions.csv")







meh <- wsb %>%
   mutate(emojis = ji_extract_all(title))
