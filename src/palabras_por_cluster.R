palabras_por_cluster <- function(top = 50){
  
  map(1:max(num_clusters), function(cl){
    df %>% 
      filtra_molestas() %>% 
      group_by(cluster, word) %>% 
      summarise(conteo = sum(n)) %>% 
      arrange(cluster, desc(conteo)) %>% 
      do(head(.,top)) %>% 
      filter(cluster == cl) %>% 
      ggplot(aes(x = reorder(word, conteo), y = conteo)) + 
      geom_col() +
      coord_flip() + 
      labs(
        title = glue("Cluster {cl}"), 
        x = "",
        y = ""
      ) +
      theme(axis.text.x = element_text(angle = 0))
  }) %>% 
    return()
  
}

