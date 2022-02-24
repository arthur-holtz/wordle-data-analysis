##### libraries #####
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyr)

setwd("~/wordle-data-analysis/")

##### download game script #####
url = "https://www.powerlanguage.co.uk/wordle/main.e65ce0a5.js"
wordle_script_text = GET(url) %>%
  content(as = "text", encoding = "UTF-8")

##### parse word list #####

# I couldn't figure out how to extract this programmatically
# so instead I'm extracting from the beginning of the first word
# through the end of the last word
word_list = substr(
  wordle_script_text,
  # cigar is the first word
  str_locate(wordle_script_text, "cigar")[,"start"],
  # shave is the last word
  str_locate(wordle_script_text, "shave")[,"end"]) %>%
  # remove quotation marks
  str_remove_all("\"") %>%
  # split at commas
  str_split(",") %>%
  # convert to DF
  data.frame() %>%
  # rename column
  select(word = 1) %>%
  mutate(word = toupper(word)) %>%
  head(50) %>%
  separate(word,
           sep = "",
           # for some reason, R puts a blank column in the beginning
           # so I add a 6th column and then remove it in the next step
           into = as.character(1:6),
           remove = FALSE) %>%
  select(-2,
         "l_1" = 3,
         "l_2" = 4,
         "l_3" = 5,
         "l_4" = 6,
         "l_5" = 7)

cross_joined = merge(word_list,
                     word_list,
                     by = NULL,
                     suffixes = c("_guess","_target")) %>%
  filter(word_guess != word_target) %>%
  mutate(
    l_1_score = case_when(
      l_1_guess == l_1_target ~ 3,
      l_1_guess == l_2_target ~ 1,
      l_1_guess == l_3_target ~ 1,
      l_1_guess == l_4_target ~ 1,
      l_1_guess == l_5_target ~ 1,
      TRUE ~ 0),
    l_2_score = case_when(
      l_2_guess == l_2_target ~ 3,
      l_2_guess == l_1_target ~ 1,
      l_2_guess == l_3_target ~ 1,
      l_2_guess == l_4_target ~ 1,
      l_2_guess == l_5_target ~ 1,
      TRUE ~ 0),
    l_3_score = case_when(
      l_3_guess == l_3_target ~ 3,
      l_3_guess == l_1_target ~ 1,
      l_3_guess == l_2_target ~ 1,
      l_3_guess == l_4_target ~ 1,
      l_3_guess == l_5_target ~ 1,
      TRUE ~ 0),
    l_4_score = case_when(
      l_4_guess == l_4_target ~ 3,
      l_4_guess == l_1_target ~ 1,
      l_4_guess == l_2_target ~ 1,
      l_4_guess == l_3_target ~ 1,
      l_4_guess == l_5_target ~ 1,
      TRUE ~ 0),
    l_5_score = case_when(
      l_5_guess == l_5_target ~ 3,
      l_5_guess == l_1_target ~ 1,
      l_5_guess == l_2_target ~ 1,
      l_5_guess == l_3_target ~ 1,
      l_5_guess == l_4_target ~ 1,
      TRUE ~ 0),
    total_score = l_1_score + l_2_score + l_3_score + l_4_score + l_5_score)

%>%
  select(word_guess, word_target, contains("score")) %>%
  group_by(word_guess) %>%
  summarize(avg_score = mean(total_score))


