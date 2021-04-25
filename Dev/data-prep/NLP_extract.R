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


named_stocks <- function(string){ # Finds stocks named
  split <- unlist(strsplit(tolower(string), "[[:punct:] ]"))
  indexed <- match(split, nyse_common$Symbol)
  vals <- toupper(nyse_common$Symbol[indexed])
  clean <- vals[!is.na(vals)]
  return (paste(clean, collapse = ' '))
}

clean_awards <- function(string){ # Takes the all awardings column and returns three cols: count awards, coin coint, award names
  if (is.na(string) || string=="[]"){
    return(list("count_awards" = NA, "coin_awards" = NA, "award_names" = NA))
  }
  split_awards <- str_extract_all(awardings[5], "(?<=\\{).+?(?=\\})")[[1]]
  only_awards <- split_awards[str_detect(split_awards, "award_sub_type")]
  
  count_awards <- str_extract_all(only_awards,"(?<=count': ).+(?=, 'days_of_drip_extension)")
  count_awards <- sum(as.numeric(unlist(count_awards)))
  
  coin_awards <- str_extract_all(only_awards,"(?<=coin_price': ).+(?=, 'coin_reward)")
  coin_awards <- sum(as.numeric(unlist(coin_awards)))
  
  award_names <- str_extract_all(only_awards,"(?<=name': ').+(?=', 'penny_donate)")
  award_names <- paste(unlist(award_names))
  
  return(list("count_awards" = count_awards, "coin_awards" = coin_awards, "award_names" = award_names))
}

awards <- map(wsb$all_awardings, clean_awards)

awards <- as.data.frame(do.call(rbind,
                      awards)) %>%
  flatten()

awards[] <- lapply(awards, unlist)

wsb <- cbind(wsb, awards)

wsb <- subset(wsb, select = -c(all_awardings))


wsb$title_sentiment <- map(wsb$title, calc_sentiment)%>%
  unlist()

wsb$post_sentiment <- map(wsb$selftext, calc_sentiment)%>%
  unlist()

wsb$title_stocks <- map(wsb$title, named_stocks) %>%
  unlist()
wsb$post_stocks <- map(wsb$selftext, named_stocks) %>%
  unlist()



# wsb <- wsb %>%
#   mutate(created_utc = )



write_csv(wsb, "data/wsb_dd_submissions.csv")