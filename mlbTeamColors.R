library(rvest)
library(tidyverse)



colores_equipos <- read_html("https://teamcolorcodes.com/mlb-color-codes") %>% 
  html_nodes("p:nth-of-type(1) a.team-button") %>% 
  html_attr("href") %>% tibble() %>% 
  set_names(c("equipos")) %>% 

mutate( equipocol = str_split(equipos, "/", simplify = TRUE)[ , 4])

nombres_equipos <- read_html("https://teamcolorcodes.com/mlb-color-codes") %>% 
  html_nodes("p:nth-of-type(1) a.team-button") %>% 
  html_text("style") %>% tibble() %>% 
  set_names(c("nombr_equipos"))


colo <- c("arizona-diamondbacks-color-codes",
          "atlanta-braves-color-codes",
          "baltimore-orioles-color-codes",
          "boston-red-sox-color-codes",
          "chicago-cubs-color-codes",
          "chicago-white-sox-color-codes",
          "cincinnati-reds-color-codes",
          "cleveland-indians-color-codes",
          "colorado-rockies-color-codes",
          "detroit-tigers-color-codes",
          "houston-astros-color-codes",
          "kansas-city-royals-color-codes",
          "los-angeles-angels-of-anaheim-color-codes",
          "los-angeles-dodgers-color-codes",
          "miami-marlins-color-codes",
          "milwaukee-brewers-color-codes",
          "minnesota-twins-color-codes",
          "new-york-mets-color-codes",
          "new-york-yankees-color-codes",
          "oakland-as-color-codes",
          "philadelphia-phillies-color-codes",
          "pittsburgh-pirates-color-codes",
          "st-louis-cardinals-color-codes",
          "san-diego-padres-color-codes",
          "san-francisco-giants-color-codes",
          "seattle-mariners-color-codes",
          "tampa-bay-rays-color-codes",
          "texas-rangers-color-codes",
          "toronto-blue-jays-color-codes",
          "washington-nationals-color-codes")

lista <- c("primary",
           "secondary",
           "tertiary",
          "quaternary")

equipo_tabla <- tibble(team = nombres_equipos$nombr_equipos, colo)

colores <- tibble(colo)%>%
unnest(cols = c(colo))


colores_scrape <- function(colo) {
  Sys.sleep(3)
  url <- glue::glue("https://teamcolorcodes.com/{colo}/")
  read_html(url) %>%
  html_nodes("div.colorblock") %>%
  html_text("style") %>% tibble() %>% 
  slice(1:4)
}
colores_scrape_final <- colores %>%
  mutate(data = map(colo, ~ colores_scrape (.x)))

colores_scrape_df <- colores_scrape_final %>% unnest(cols = c(data)) %>% 
  set_names(c("colo", "hex")) %>% 
  mutate(hex1 = str_extract(hex, "#\\w{6}"))%>% 
  select( colo, hex1) 
        
coloresw <- colores_scrape_df %>% group_by(colo) %>% 
  mutate(row= row_number()) %>% 
  pivot_wider(values_from = hex1, names_from =colo) %>% 
  select(-row) %>% unnest(cols = c()) 

colores <- colores  %>% pivot_longer(!lista,  names_to = "equipos", names_transform = list(colores = as.integer)) %>% 
  select(equipos, lista, value)
  colores <- colores  %>% arrange(equipos) %>% 
  left_join(equipo_tabla, by =c("equipos" = "colo")) 
  
  
colores <- colores   %>% 
    pivot_wider(team, names_from = lista) 
  
  write.csv(colores, "coloresMLB.csv", row.names = FALSE)
  coloresMLB<- read.csv("coloresMLB.csv")
  
  
  
  
  
  
  
  
