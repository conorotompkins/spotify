library(tidyverse)
library(geniusr)

geniusr::genius_token(force = TRUE)

geniusr::search_artist("Queens of the Stone Age")
geniusr::search_artist("Desert Sessions")

qotsa <- geniusr::get_artist(artist_id = 25320)

qotsa_songs <- geniusr::get_artist_songs_df(25320)

qotsa_songs

qotsa_songs %>% 
  slice(2) %>% 
  pull(song_id) %>% 
  map(~get_song(song_id = .x)) %>% 
  map("content") %>% 
  map("writer_artists") %>% 
  flatten() %>% 
  map_chr("name") %>% 
  paste(collapse = ", ")

qotsa_songs %>% 
  slice(3) %>% 
  pull(song_id) %>% 
  map(~get_song(song_id = .x)) %>% 
  map(c("content", "writer_artists")) %>% 
  flatten() %>% 
  map_chr("name") %>% 
  paste(collapse = ", ")

get_writer_artists <- function(var_song_id){
  
  text <- map(var_song_id, get_song) %>% 
    map(c("content", "writer_artists")) %>% 
    flatten() %>% 
    map_chr("name") %>% 
    paste(collapse = ", ")
  
  if (text == "") {
    return(NA)
  }
  
  else{
    return(text)
  }
  
}

get_writer_artists(118898)

qotsa_artists <- qotsa_songs %>% 
  #slice(1:10) %>% 
  mutate(writer_artists = map(song_id, get_writer_artists)) %>% 
  separate_rows(writer_artists, sep = ", ")

qotsa_artists %>% 
  count(song_name) %>% 
  mutate(song_name = str_squish(song_name) %>% str_to_lower(.)) %>% 
  ggplot(aes(n)) +
  geom_histogram()

qotsa_artists %>% 
  count(writer_artists, sort = TRUE)

library(tidygraph)
library(ggraph)
qotsa_artists %>% 
  select(song_name, writer_artists) %>% 
  widyr::pairwise_count(writer_artists, song_name, diag = FALSE, upper = FALSE) %>% 
  as_tbl_graph() %>% 
  activate(edges) %>% 
  #filter(n > 1) %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated()) %>% 
  ggraph() +
  geom_edge_fan(aes(edge_width = n, edge_alpha = n)) +
  geom_node_label(aes(label = name), repel = TRUE) +
  theme_void()
