rm(list=ls()); gc()

library(tidyverse)
library(stringr)
library(subtools)
library(tidytext)
library(glue)
library(wordcloud)

source("src/palabras_por_cluster.R")

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
  group_by(season_num, season_episode) %>%
  count(word)

# df$word %>% unique %>% length
# df$word %>% unique %>% sort %>% range

## Filtramos palabras que empiecen por dígitos

df <- df %>% 
  ungroup() %>% 
  filter(!grepl("^[[:digit:]]",df$word))


## Nos quedamos solo con las palabras que aparecen cierto número de veces. 
## Con el resultado calculamos el tf_idf y pivotamos para tener la 
## matriz documento término

limite_inferior <- 2

df %>% 
  count(word) %>% 
  filter(nn > 2) %>%
  ggplot(aes(x = nn)) +
  geom_histogram(binwidth = 5) + 
  coord_cartesian(xlim = c(0, 20))
# df <- df %>% 
#   add_count(word) %>% 
#   filter(nn > limite_inferior)

df %>% 
  add_count(word) %>% 
  filter(nn >= quantile(nn, probs = 0.9)) %>% 
  distinct(word)

df_filtrado <- df %>% 
  add_count(word) %>% 
  filter(nn >= quantile(nn, probs = 0.9))

df_pivot <- df_filtrado %>% 
  bind_tf_idf(word, season_episode, n) %>% 
  arrange(-tf_idf) %>% 
  select(season_episode, word, tf_idf) %>% 
  spread(word, tf_idf,fill = 0)

# for (var in names(df_pivot)) {
#   df_pivot[[var]][rlang::are_na(df_pivot[[var]])] <- 0
# }

cor(df_pivot %>% select(sample(1:80, 20)))


## Componentes principales ----------------------------------------------------

df_pivot %>% 
  select_if(.predicate = function(x) any(x > 1)) %>% 
  names()

gc()
componentes <- prcomp(x = select(df_pivot,-season_episode), scale.=TRUE)
summary(componentes)

df_componentes <- componentes$x %>% 
  as_tibble() %>% 
  select(PC1:PC38)

rm(componentes); gc()

## k-means ---------------------------------------------------------

nombres_componentes <- names(df_componentes)

cortes <- function(varr){
  quantile(varr, probs = seq(0,1, by = 0.2))
}

dots <- map(nombres_componentes, function(var){
    as.formula(glue("~findInterval({var}, vec = cortes({var}))"))
})

num_clusters <- 3

set.seed(31818)
clobjeto <- df_componentes %>% 
  mutate_(.dots = set_names(dots, nombres_componentes)) %>% 
  kmeans(centers = num_clusters)

## Exploración de resultados ---------------------------------------

df_episodio_cluster <- df_pivot %>% 
  select(season_episode) %>% 
  mutate(cluster = clobjeto$cluster)

ggplot(df_episodio_cluster, aes(x = factor(cluster))) + 
  geom_bar()

# df <- df_pivot %>% 
#   gather("word","conteo",-season_episode, -cluster)

df <- df %>% 
  select(season_num:n) %>% 
  left_join(df_episodio_cluster, by = "season_episode")


## Distribución de palabras por cluster -----------------------------------

palabras_por_cluster()


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
    ggplot(aes(x = reorder(word, conteo), y = conteo)) + 
    geom_col() +
    coord_flip() + 
    labs(title = glue("Cluster {cl}"), x = "Palabra", y = "Frecuencia") +
    theme(axis.text.x = element_text(angle = 0)),
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

