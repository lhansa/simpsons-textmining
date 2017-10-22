rm(list=ls()); gc()

library(tidyverse)
library(stringr)
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

df_episodio_cluster <- df_pivot %>% 
  select(season_episode) %>% 
  mutate(cluster = clobjeto$cluster)


ggplot(df_episodio_cluster, aes(x = factor(cluster))) + 
  geom_bar()

# df <- df_pivot %>% 
#   gather("word","conteo",-season_episode, -cluster)

df <- df %>% 
  left_join(df_episodio_cluster, by = "season_episode")


## Distribución de palabras por cluster -----------------------------------

df %>% 
  group_by(cluster, word) %>% 
  summarise(conteo = sum(n)) %>% 
  arrange(cluster, desc(conteo)) %>% 
  summarise(n(), sum(conteo))

library(manipulate)

manipulate(
  df %>% 
    group_by(cluster, word) %>% 
    summarise(conteo = sum(n)) %>% 
    arrange(cluster, desc(conteo)) %>% 
    do(head(.,50)) %>% 
    filter(cluster == cl) %>% 
    ggplot(aes(x = reorder(word, desc(conteo)), y = conteo)) + 
    geom_col() +
    theme(axis.text.x = element_text(angle = 90)), 
  cl = slider(1,4)
)





## Wordcloud --------------------------------------------------------

# genera_wordcloud <- function(n_cluster){
#   aux <- df %>% 
#     filter(cluster == n_cluster)
# 
#   wordcloud(aux$word, aux$conteo)
# }
# 

