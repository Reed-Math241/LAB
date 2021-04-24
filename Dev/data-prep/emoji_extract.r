#devtools::install_github("hadley/emo")

library(tidyverse)
library(emo)
library(tidytext)
library(ggplot2)



sentiments <- get_sentiments("afinn")
calc_sentiment <- function(string){
  split <- unlist(strsplit(string, "[[:punct:] ]"))
  indexed <- match(split, sentiments$word)
  vals <- sentiments$value[indexed]
  return(sum(vals, na.rm=TRUE))
}

wsb <- read_csv("data/wsb_dd_submissions.csv")

wsb$title_sentiment <- map(wsb$title, calc_sentiment)%>%
  unlist()

wsb$post_sentiment <- map(wsb$selftext, calc_sentiment)%>%
  unlist()


wsb %>%
  ggplot(aes(post_sentiment)) +
  geom_histogram()


wsb <- wsb %>%
  mutate(emojis = ji_extract_all(title))

ji_extract_all(wsb$title[275])


