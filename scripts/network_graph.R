library(tidyverse)
library(geniusr)

#edit r environment file here
#usethis::edit_r_environ()

geniusr::genius_token()

geniusr::search_artist("Queens of the Stone Age")
#geniusr::search_artist("Desert Sessions")

qotsa <- geniusr::get_artist(artist_id = 25320)



qotsa_songs <- geniusr::get_artist_songs_df(25320)
qotsa_songs






qotsa_songs %>% 
  slice(3) %>% 
  pull(song_id) %>% 
  map(~get_song(song_id = .x)) %>% 
  map(c("content", "writer_artists")) %>% 
  flatten() %>% 
  map_chr("name") %>% 
  paste(collapse = ", ")

get_personnel <- function(var_song_id, personnel_type){
  
  text <- map(var_song_id, get_song) %>% 
    map(c("content", personnel_type)) %>% 
    flatten() %>% 
    map_chr("name") %>% 
    paste(collapse = ", ")
  
  if (text == "") {
    return(NA_character_)
  }
  
  else{
    return(text)
  }
  
}

get_personnel(118898, "producer_artists")


get_album_name <- function(var_song_id){
  
  text <- map(var_song_id, get_song) %>% 
    map(c("content", "album")) %>% 
    #flatten() %>% 
    map_chr("name") %>% 
    paste(collapse = ", ")
  
  if (text == "") {
    return(NA_character_)
  }
  
  else{
    return(text)
  }
  
}

get_album_name(118898) %>% str()


qotsa_artists <- qotsa_songs %>% 
  #slice(1:10) %>% 
  mutate(writer = map(song_id, get_personnel, personnel_type = "writer_artists"),
         producer = map(song_id, get_personnel, personnel_type = "producer_artists")) %>% 
  mutate(producer = str_replace_all(producer, "The Fififf Teeners", "Josh Homme, Chris Goss")) %>% 
  separate_rows(writer, sep = ", ") %>% 
  separate_rows(producer, sep = ", ") %>% 
  pivot_longer(cols = c(writer, producer), names_to = "personnel_type", values_to = "personnel_name")

qotsa_artists

qotsa_artists %>% 
  filter(personnel_name == "The Fififf Teeners")



qotsa_artists %>% 
  distinct(song_name, personnel_name) %>% 
  count(song_name) %>% 
  mutate(song_name = str_squish(song_name) %>% str_to_lower(.)) %>% 
  ggplot(aes(n)) +
  geom_histogram()

qotsa_artists %>% 
  filter(personnel_type == "producer") %>% 
  count(personnel_name, sort = TRUE)

library(tidygraph)
library(ggraph)

qotsa_artists %>%
  distinct(song_name, personnel_type, personnel_name) %>% 
  filter(!is.na(personnel_name)) %>% 
  group_by(personnel_name) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(song_name, personnel_name) %>% 
  widyr::pairwise_count(personnel_name, song_name, diag = FALSE, upper = FALSE) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  activate(edges) %>% 
  #filter(n > 1) %>% 
  activate(nodes) %>% 
  #filter(!node_is_isolated()) %>% 
  ggraph() +
  geom_edge_fan(aes(edge_width = n, edge_alpha = n), color = "white") +
  geom_node_point(size = 4, color = "red") +
  geom_node_label(aes(label = name), repel = TRUE) +
  labs(title = "Queens of the Stone Age personnel network",
       edge_width = "Distinct connections",
       edge_alpha = "Distinct connections") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        title = element_text(color = "white"),
        legend.text = element_text(color = "white"))
