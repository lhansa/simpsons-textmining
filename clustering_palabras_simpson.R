rm(list=ls()); gc()

library(tidyverse)
library(subtools)
library(tidytext)

df <- read.subtitles.serie(dir = "./The Simpsons/") %>%
  subDataFrame() %>%
  as_tibble() %>%
  na.omit()

data(stop_words)

df <- df %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words) %>% 
  filter(is.na(as.numeric(word))) %>% 
  filter(word != "simpson")

## Contamos el número de veces que aparece cada palabra en cada episodio

df <- df %>% 
  mutate(season_episode = str_c(str_pad(season_num, width = 2, pad = "0"), 
                                str_pad(episode_num, width = 2, pad = "0"), 
                                sep = "x")) %>% 
  group_by(season_episode) %>%
  count(word)

df$word %>% unique %>% length
df$word %>% unique %>% sort %>% range

## Filtramos palabras que empiecen por dígitos

df <- df %>% 
  ungroup() %>% 
  filter(!grepl("^[[:digit:]]",df$word))

## Pivotamos para tener formato documento-término

df_pivot <- df %>% 
  spread(word, n)

for (var in names(df_pivot)) {
  df_pivot[[var]][rlang::are_na(df_pivot[[var]])] <- 0
}

df_pivot


