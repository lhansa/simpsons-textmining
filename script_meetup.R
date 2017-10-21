rm(list=ls()); gc()

library(tidyverse)
library(subtools)
library(tm)
library(wordcloud)
library(tidytext)
library(igraph)
library(ggraph)
library(stringr)

## TM ---------------------------------------------------------

c <- read.subtitles.serie(dir = "./The Simpsons/") %>% 
  tmCorpus() %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stripWhitespace)


TDM <- c %>% 
  TermDocumentMatrix() %>% 
  as.matrix()

vec.season <- c(rep(x = 1,13), rep(2, 22), rep(3,24), 
                rep(4,22), rep(5,22), rep(6,25), 
                rep(7,25),rep(8,25), rep(9,25)) #episodios por temp

TDM.season <- t(apply(TDM, 1, function(x) tapply(x, vec.season, sum)))
colnames(TDM.season) <- paste0("S_", unique(vec.season))
head(TDM.season)


set.seed(100)
comparison.cloud(TDM.season, title.size = 1, max.words = 200, random.order = T)


## Ahora con tidytext -------------------------------------------------

df <- read.subtitles.serie(dir = "./The Simpsons/") %>%
  subDataFrame() %>%
  as_tibble() %>%
  na.omit()

data(stop_words)

tidy_df <- df %>%
  unnest_tokens(word, Text) %>%
  dplyr::anti_join(stop_words)

tidy_df <- tidy_df %>% 
  filter(is.na(as.numeric(word))) %>% 
  filter(word != "simpson")

library(ggplot2)

tidy_df %>% group_by(season) %>%
  count(word, sort = FALSE) %>%
  top_n(15) %>%
  ggplot(aes(reorder(word,n), n, fill = season)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~season, scales = "free_y") +
  labs(x = NULL) +
  guides(fill = FALSE) +
  scale_fill_brewer(palette = "Set1")

# require(plotly)
tidy_tf <- tidy_df %>% 
  group_by(season) %>%
  count(word, sort = TRUE)

simpson_family <- c('homer', 'bart', 'lisa', 'maggie', 'marge', 'patty','selma')
other_characters <-  c('moe', 'ned', 'barney', 'modd', 'itchy', 'scratchy', 'krusty', 'burns', 'lenny', 'carl', 'edna', 'nelson', 'apu', 'milhouse', 'ralph', 'skinner', 'bob')

  
tidy_tf %>% 
  filter(word %in% simpson_family) %>% 
  ggplot(aes(x=season, y=n, group=word)) +
  geom_line(aes(color=word), size=1.25)+
  geom_point(aes(color=word))

tidy_tf %>% 
  filter(word %in% other_characters) %>% 
  ggplot(aes(x=season, y=n, group=word)) +
  geom_line(aes(color=word), size=0.75)+
  geom_point(aes(color=word))

## Bigramas --------------------------------------------------

bigram_graph <- df %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  group_by(season) %>%
  count(word1, word2, sort = TRUE) %>%
  select(word1, word2, season, n) %>%
  filter(n > 7) %>%
  graph_from_data_frame()

glimpse(bigram_graph)


set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

## TF-idf --------------------------------------------------------------------

tf_idf_df <- tidy_df %>% 
  count(season, word, sort = TRUE) %>%
  bind_tf_idf(word, season, n) %>% 
  arrange(-tf_idf)

tf_idf_df %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = season)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

tf_idf_df %>% 
  group_by(season) %>% 
  top_n(8) %>% 
  ungroup %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = season)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~season, ncol = 2, scales = "free") +
  coord_flip()

## Análisis de sentimiento --------------------------------------------------


head(get_sentiments("afinn"), 3)

head(get_sentiments("bing"), 3)

head(get_sentiments("nrc"), 3)

tidy_df <- tidy_df %>% 
  mutate(season_episode = paste0('S', season_num,"XE", str_pad(episode_num,width = 2,pad = "0")))


simpson_sentiment <- tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(season_episode, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(simpson_sentiment,3)

simpson_sentiment <- simpson_sentiment %>% 
  mutate(season = str_sub(season_episode,start = 1, end = 2),
         season = str_replace(season, "S", "Season "), 
         episode = str_sub(season_episode,start = 4, end = 6))

ggplot(simpson_sentiment, aes(episode, sentiment, fill = season)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season, ncol = 2, scales = "free_x")

afinn <- tidy_df %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(season_episode) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

afinn <- afinn %>% 
  mutate(season = str_sub(season_episode,start = 1, end = 2),
         season = str_replace(season, "S", "Season "),
         episode= str_sub(season_episode,start = 4, end = 6))

ggplot(afinn, aes(episode, sentiment, fill = season)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~season, ncol = 2, scales = "free_x")


resplandior <- tidy_df %>% 
  filter(season_num==6, episode_num==6)

afinn_r <- resplandior %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(season_episode) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc_r <- bind_rows(resplandior %>% 
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing et al."),
                            resplandior %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                           filter(sentiment %in% c("positive", 
                                                                   "negative"))) %>%
                              mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

comparativa <- afinn_r %>% 
  select(sentiment, method) %>% 
  bind_rows(bing_and_nrc_r %>% 
              select(sentiment, method))
  
ggplot(data=comparativa, aes(x=method, y=sentiment)) +
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal()
