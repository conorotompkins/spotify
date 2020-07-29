library(tidyverse)
library(geniusr)

qotsa_songs <- geniusr::get_artist_songs_df(25320)

#writer_artists
#producer_artists
#album name
##album id, name

#custom_performances
##mixing
###name
##mastered by
###name 
##recorded by
###name
##instrument performances

test <- qotsa_songs %>% 
  slice(2) %>% 
  pull(song_id) %>% 
  map(~get_song(song_id = .x))

str(test)


qotsa_songs %>% 
  slice(2) %>% 
  pull(song_id) %>% 
  map(~get_song(song_id = .x))

test_flat <- test %>% 
  map(c("content", "custom_performances"))# %>% 
#flatten()

test_flat %>% 
  map(c(10, 1))

test_flat %>% 
  pluck(1, "label")

test_flat %>% 
  map_chr(c(1, 2, 1, 7))

test_flat %>% 
  map(c(2, 1, 7))

test_flat %>% 
  map(c("artists")) %>% 
  flatten() %>% 
  pluck("name")

tibble(role = map_chr(test_flat, "label"))
object = test_flat)
# personnel = map_chr(test_flat, c(1, 2, 1, 7)))

list(type = map_chr(test_flat, "label"),
     name = map(test_flat, "artists") %>% flatten %>% map_chr("name"))
