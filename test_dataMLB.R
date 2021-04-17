library(rvest)
library(tidyverse)

# Tabla de las fotos

MLB <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/dataMLB/main/dataMLB.csv") 

# Descargamos la tabla de baseball-reference standard-fielding

reference_fielding <- read_html ("https://www.baseball-reference.com/leagues/MLB/2021-standard-fielding.shtml") %>% 
  html_nodes(xpath = '//comment()') %>%
  html_text() %>%
  paste(collapse='') %>%
  read_html() %>% 
  html_node("#players_players_standard_fielding_fielding") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  mutate(name = str_squish(name)) %>% #quitamos los espacios en blanco para que coincidan bien los nombres
  mutate_at(vars(rk,age,g:rf_g), as.numeric) %>% 
  filter(name != "Name" & name !="LgAvg")


# Unimos las tablas

reference_fielding_add <- reference_fielding %>% left_join(dataMLB, by = c("name"="espn_player_name"))


#comprobamos los NA

reference_na <- reference_fielding_add %>% filter(is.na(cabezas))

# De esta lista salen 124 jugadores sin foto que no están en principio en la fecha de la ejecucion del código en ninguna plantilla