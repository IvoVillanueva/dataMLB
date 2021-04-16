library(rvest)
library(tidyverse)


# Scrap la lista de la url base de los equipos ----------------------------------------

url_equipos <- read_html("https://espndeportes.espn.com/beisbol/mlb/equipos") %>% 
                html_nodes(".pl3 > a") %>% 
                html_attr("href") %>% tibble() %>% 
                set_names(c("equipos")) %>% 
mutate(equipourl = str_extract(equipos, "[^/]+$"), #esto saca la última parte de la url con los nombres de los equipos
       equipoabr = str_split(equipos, "/", simplify = TRUE)[ , 7]) #esto saca lo que hay en el hueco numero siete entre barras

#datapasta::vector_paste_vertical(url_equipos$equipourl)
#-------------------------------------------------------el código de arriba nos escribe con las comillas y las comas un dataframe solo le tenemos que dar el nombre "teams" en este caso
teams <- c("chicago-white-sox",
            "cleveland-indians",
            "detroit-tigers",
            "kansas-city-royals",
            "minnesota-twins",
            "chicago-cubs",
            "cincinnati-reds",
            "milwaukee-brewers",
            "pittsburgh-pirates",
            "st-louis-cardinals",
            "baltimore-orioles",
            "boston-red-sox",
            "new-york-yankees",
            "tampa-bay-rays",
            "toronto-blue-jays",
            "atlanta-braves",
            "miami-marlins",
            "new-york-mets",
            "philadelphia-phillies",
            "washington-nationals",
            "houston-astros",
            "los-angeles-angels",
            "oakland-athletics",
            "seattle-mariners",
            "texas-rangers",
            "arizona-diamondbacks",
            "colorado-rockies",
            "los-angeles-dodgers",
            "san-diego-padres",
            "san-francisco-giants")

#datapasta::vector_paste_vertical(url_equipos$equipoabr)
#-------------------------------------------------------y este nos da el dataframe con el codigo corto
shortsnames <- c("chw",
                 "cle",
                 "det",
                 "kc",
                 "min",
                 "chc",
                 "cin",
                 "mil",
                 "pit",
                 "stl",
                 "bal",
                 "bos",
                 "nyy",
                 "tb",
                 "tor",
                 "atl",
                 "mia",
                 "nym",
                 "phi",
                 "wsh",
                 "hou",
                 "laa",
                 "oak",
                 "sea",
                 "tex",
                 "ari",
                 "col",
                 "lad",
                 "sd",
                 "sf")

# Hacemos un data frame con los dos data frames para usarlo en las funciones  

cabezas <- tibble(shortsnames, teams) %>%
  unnest(cols = c(shortsnames, teams))

# Function para scrape las fotos de los jugadores  ----

cabezas_foto_scrape <- function(shortsnames, teams) {
  Sys.sleep(3) #Suspende la ejecución  durante el intervalo que indiquemos
  url <- glue::glue("https://espndeportes.espn.com/beisbol/mlb/equipo/plantel/_/nombre/{shortsnames}/{teams}")
  read_html(url) %>%
    html_nodes(".headshot img") %>%
    html_attr("alt") %>% tibble()
}

foto_scrape_final <- cabezas  %>%
  mutate(data = map2(shortsnames, teams, ~ cabezas_foto_scrape(.x,.y))) #la funcion "map2" es para mapear dos variables como en este caso, si no seria "map"

# ## limpiamos y desanidamos los datos en una tabla -----------------------

foto_scrape_df <- foto_scrape_final %>% unnest() %>%
  set_names(c("shortsnames", "teams" , "espn_foto")) %>%
  mutate(id_player = str_extract(espn_foto, "[0-9]+"), #sacamos el id del jugador
         numero = row_number()) #esta columna es para la union con la tabla de los nombres

# Function para scrape los nombres de los jugadores  ----

nombres_scrape <- function(shortsnames, teams) {
  Sys.sleep(3) #3 segundos porque no hay que ser demasiado macarra raspando datos
  url <- glue::glue("https://espndeportes.espn.com/beisbol/mlb/equipo/plantel/_/nombre/{shortsnames}/{teams}")
  read_html(url) %>% 
    html_nodes("td:nth-of-type(2) a") %>%
    html_text("title") %>% tibble() 
  
}
# al ejecutar esta parte tardara un rato porque le hemos puesto 3 segundos entre url
nombres_scrape_final <- cabezas %>%
  mutate(data = map2(shortsnames, teams, ~ nombres_scrape(.x,.y)))


# ## limpiamos y desanidamos los datos en una tabla -----------------------

nombres_scrape_df <- nombres_scrape_final %>% unnest() %>%
  set_names(c("shortsnames", "teams" , "mlb_names")) %>%
  mutate(numero = row_number()) #esta columna es para la union con la tabla de las fotos


# ## unimos los nombres y las fotos ---------------------------------------

mlb_csv1 <- foto_scrape_df %>% left_join(nombres_scrape_df, by = c("numero") ) %>%
  select(PlayerIdEspn = id_player, EspnName = mlb_names, Cabezas = espn_foto, UrlNameTeams= teams.x, TeamAbr = shortsnames.x ) 

######### guardmos la tabla

write.csv(mlb_csv1, "mlb_csv1.csv", row.names = FALSE)
mlb_csv1 <- read.csv("mlb_csv1.csv")

#  Scrape Logos Mlb -------------------------------------------------------

mlb_logos <- read_html("https://www.mlb.com/team") %>% 
  html_nodes("img.p-forge-logo") %>% 
  html_attr("data-src")%>% tibble() %>% 
  set_names(c("mlb_logos")) %>% 
  mutate(id_team = str_extract(mlb_logos, "[0-9]+")) #extraemos el id de los equipos de la url de la pagina de la mlb


# Scrape mlb_names --------------------------------------------------------

  
mlb_names <- read_html("https://www.mlb.com/team") %>% 
  html_nodes("div.u-text-h4") %>% 
  html_text("class")%>% tibble() %>% 
  set_names(c("mlb_names"))


# Unimos los nombres y los logos ------------------------------------------

mlb_table <- tibble(mlb_names, mlb_logos)


# Creamos la columna "team_abr" que nos servirá de union con la tabla jugadores ----------------------

#datapasta::vector_paste_vertical(mlb_teams$teamnames) con este código nos da la lista de los equipos

mlb_table <- mlb_table %>% mutate(
  team_abr = case_when(
    mlb_names == "Baltimore Orioles" ~ "bal",
    mlb_names == "Boston Red Sox" ~ "bos",
    mlb_names == "Chicago White Sox" ~ "chw",
    mlb_names == "Cleveland Indians" ~ "cle",
    mlb_names == "Detroit Tigers" ~ "det",
    mlb_names == "Houston Astros" ~ "hou",
    mlb_names == "Kansas City Royals" ~ "kc",
    mlb_names == "Los Angeles Angels" ~ "laa",
    mlb_names == "Minnesota Twins" ~ "min",
    mlb_names == "New York Yankees" ~ "nyy",
    mlb_names == "Oakland Athletics" ~ "oak",
    mlb_names == "Seattle Mariners" ~ "sea",
    mlb_names == "Tampa Bay Rays" ~ "tb",
    mlb_names == "Texas Rangers" ~ "tex",
    mlb_names == "Toronto Blue Jays" ~ "tor",
    mlb_names == "Arizona Diamondbacks" ~ "ari",
    mlb_names == "Atlanta Braves" ~ "atl",
    mlb_names == "Chicago Cubs" ~ "chc",
    mlb_names == "Cincinnati Reds" ~ "cin",
    mlb_names == "Colorado Rockies" ~ "col",
    mlb_names == "Los Angeles Dodgers"~ "lad",
    mlb_names == "Miami Marlins" ~ "mia",
    mlb_names == "Milwaukee Brewers" ~ "mil",
    mlb_names == "New York Mets" ~ "nym",
    mlb_names == "Philadelphia Phillies"~ "phi",
    mlb_names == "Pittsburgh Pirates" ~ "pit",
    mlb_names == "San Diego Padres" ~ "sd",
    mlb_names == "San Francisco Giants" ~ "sf",
    mlb_names == "St. Louis Cardinals" ~ "stl",
    mlb_names == "Washington Nationals"~ "wsh",
    TRUE ~ mlb_names))

# Guardamos en csv --------------------------------------------------------

write.csv(mlb_table, "mlb_table.csv", row.names = FALSE)
mlb_teams <- read.csv("mlb_table.csv", stringsAsFactors = FALSE)


# Cargamos el csv de los jugadores para unirlo con la tabla de los o no si ya está cargado. Por si acaso lo pongo--------


mlb_csv1 <- read.csv("mlb_csv1.csv", stringsAsFactors = FALSE)


# Unimos las tablas -------------------------------------------------------

dataMLB <- mlb_csv1 %>% left_join(mlb_teams, by =c("TeamAbr"= "team_abr")) %>% 
  janitor::clean_names() %>% select(player_id_espn,
                                    espn_player_name = espn_name,
                                    cabezas,
                                    mlb_team_name = mlb_names,
                                    mlb_logos,
                                    id_team,
                                    url_name_teams) %>% mutate( mlb_logos =
                                                                  str_remove(mlb_logos, "//")) #al principio de las url de los equipos, nos sale esas dos barras asi que las quitamos

# Guardamos en csv --------------------------------------------------------



write.csv(dataMLB, "dataMLB.csv", row.names = FALSE)
dataMLB <- read.csv("dataMLB.csv")


# chequeo sanitario ------------------------------------------------------------

MLB <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/dataMLB/main/dataMLB.csv")
