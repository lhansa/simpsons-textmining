rm(list=ls()); gc()

library(tidyverse)
library(subtools)
library(tidytext)
library(glue)
library(wordcloud)

## Datos ----------------------------------------------

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

# df$word %>% unique %>% length
# df$word %>% unique %>% sort %>% range

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



## Componentes principales ----------------------------------------------------


gc()
componentes <- prcomp(x = select(df_pivot,-season_episode), scale.=TRUE)
summary(componentes)

df_componentes <- componentes$x %>% 
  as_tibble() %>% 
  select(PC1:PC30)

rm(componentes); gc()


## k-means ---------------------------------------------------------

nombres_componentes <- names(df_componentes)

cortes <- function(varr){
  quantile(varr, probs = seq(0,1, by = 0.2))
}

dots <- map(nombres_componentes, function(var){
    as.formula(glue("~findInterval({var}, vec = cortes({var}))"))
  })

set.seed(31818)
clobjeto <- df_componentes %>% 
  mutate_(.dots = set_names(dots, nombres_componentes)) %>% 
  kmeans(centers = 4)

## Exploración de resultados ---------------------------------------

df_pivot %>% 
  mutate(cluster = clobjeto$cluster) %>% 
  count(cluster)

df <- df_pivot %>% 
  mutate(cluster = clobjeto$cluster) %>% 
  gather("word","conteo",-season_episode, -cluster)

df %>% 
  arrange(cluster, desc(conteo))

## Wordcloud --------------------------------------------------------

aux <- df %>% 
  filter(cluster == 1)
  

wordcloud(aux$word, aux$conteo)
