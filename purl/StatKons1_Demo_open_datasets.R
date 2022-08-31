library(tidyverse)

data() # erzeugt eine Liste mit den Datensätzen, welche in R verfügbaren sind
head(chickwts)
str(chickwts)

# Load packages and data
data_911 <- read_delim(here("data","911.csv"), delim = ",")
str(data_911)

# Beachtet dabei, dass ihr die URL zum originalen (raw) Datensatz habt 
star_wars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-05-14/week7_starwars.csv", locale = readr::locale(encoding = "latin1")) #not working yet 

# lade die Datei "Häufigste Sprachen"
urlfile = "https://data.stadt-zuerich.ch/dataset/bfs_ste_bev_hauptsprachen_top50_od3011/download/BEV301OD3011.csv"

dat_lang <- read_delim(url(urlfile), delim = ",", col_names = T)
head(dat_lang)
