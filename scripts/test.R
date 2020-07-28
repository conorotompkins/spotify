library(tidyverse)
library(spotifyr)



access_token <- get_spotify_access_token()


search_spotify("Tool", type = "artist") %>% 
  arrange(desc(popularity)) %>% 
  View()

get_artist_albums("2yEwvVSSSUkcLeSTNyHKh8") %>% 
  glimpse()

get_album_data(artist = "TOOL", albums = "Fear Innoculum")
get_album_data("Wild child", "Expectations")

get_song("")

library(geniusr)

search_artist("Queens of the Stone Age") %>% 
  pull(artist_id) %>% 
  map(get_artist)

geniusr::get_artist_songs_df(25320)
geniusr::get_artist(25320)
geniusr::get_artist(artist_id = 25320)

get_artist_albums(id = "25320", include_groups = "album")
get_artist_albums()

test_song <- get_song(song_id = 3039923, access_token = genius_token())

test_song$primary_artist %>% str()
test_song$content$writer_artists %>% map_chr("name") %>% paste(collapse = ", ")
test_song$content$producer_artists %>% map_chr("name") %>% paste(collapse = ", ")
test_song$content$writer_artists %>% map_chr("name") %>% paste(collapse = ", ")

get_personnel


get_song_df(3039923) %>% 
  glimpse()


str(test_song)
glimpse(test_song)

get_song(song_id = 90479)

get_
?genius_token
