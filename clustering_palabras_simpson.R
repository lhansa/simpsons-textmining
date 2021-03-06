rm(list=ls()); gc()

library(tidyverse)
library(stringr)
library(subtools)
library(tidytext)
library(glue)
library(wordcloud)
library(gridExtra)

source("src/palabras_por_cluster.R")
palabras_molestas <- c("hey", "uh", "ah", "hmm", "huh", "yeah")

simpson_family <- c('homer', 'bart', 'lisa', 'maggie', 'marge', 'patty','selma')
other_characters <-  c('moe', 'ned', 'barney', 'modd', 'itchy', 'scratchy', 'krusty', 'burns', 'lenny', 'carl', 'edna', 'nelson', 'apu', 'milhouse', 'ralph', 'skinner', 'bob')


filtra_molestas <- function(.data){
  .data %>% 
    filter(!word %in% palabras_molestas) %>% 
    filter(!word %in% simpson_family) %>% 
    return()
}

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

## Filtramos palabras que empiecen por dígitos y alguna que molesta



df <- df %>% 
  ungroup() %>% 
  filter(!grepl("^[[:digit:]]",df$word)) %>% 
  filtra_molestas() 


## Nos quedamos solo con las palabras que aparecen cierto número de veces. 
## Con el resultado calculamos el tf_idf y pivotamos para tener la 
## matriz documento término

# limite_inferior <- 2
# 
# df %>% 
#   count(word) %>% 
#   filter(nn > 2) %>%
#   ggplot(aes(x = nn)) +
#   geom_histogram(binwidth = 5) + 
#   coord_cartesian(xlim = c(0, 20))
# df <- df %>% 
#   add_count(word) %>% 
#   filter(nn > limite_inferior)

# df %>% 
#   add_count(word) %>% 
#   filter(nn >= quantile(nn, probs = 0.9)) %>% 
#   distinct(word, nn) %>% 
#   arrange(desc(nn)) %>% 
#   print(n = 50)

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

num_clusters <- 5

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

df <- df %>% 
  select(season_num:n) %>% 
  left_join(df_episodio_cluster, by = "season_episode")

## Distribución de palabras por cluster -----------------------------------

grid.arrange(grobs = palabras_por_cluster(20), ncol = num_clusters)

df %>% 
  group_by(cluster, word) %>% 
  summarise(conteo = sum(n)) %>% 
  arrange(cluster, desc(conteo)) %>% 
  summarise(n(), sum(conteo))








## Wordcloud --------------------------------------------------------

# genera_wordcloud <- function(n_cluster){
#   aux <- df %>% 
#     filter(cluster == n_cluster)
# 
#   wordcloud(aux$word, aux$conteo)
# }
# 

