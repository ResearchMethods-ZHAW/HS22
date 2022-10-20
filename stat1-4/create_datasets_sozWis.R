# Small script for creating datasets for the excercises

# read data
nova <- read_delim(file = "https://zenodo.org/record/3890931/files/2017_ZHAW_aggregated_menu_sales_NOVANIMAL.csv?download=1", 
                   delim = ";")
nova_survey <- read_delim(file = "https://zenodo.org/record/3554884/files/2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv?download=1",
                          delim = ";")

nova_ind <- read_delim()


##1.2. t-Test

df_ttest <- nova %>%
  group_by(week, condit) %>%  
  summarise(tot_sold = n()) %>% 
  ungroup()

write_csv2(df_ttest, "stat1-4/Statistik1.2_Uebung.csv")

##2.2 Anova
df_anova <- nova # klone den originaler Datensatz

# fasst die vier Inhalte der Gerichte zu drei Inhalten zusammen.
df_anova %<>%
  # Geflügel & Fisch zu fleischgerichte zählen
  mutate(label_content = str_replace(label_content, "Geflügel|Fisch", "Fleisch")) %>% 
  # achtung reihenfolge spielt eine rolle, wegen des + (plus)
  mutate(label_content = str_replace(label_content, "Pflanzlich[+]|Pflanzlich", "Vegetarisch"))

# gruppiert Daten nach Menü-Inhalt und Woche
df_anova %<>%
  group_by(label_content, week) %>% 
  summarise(tot_sold = n()) %>%
  drop_na() %>% 
  ungroup() # lasst die unbekannten Menü-Inhalte weg

write_csv2(df_anova, "stat1-4/Statistik2.1_Uebung.csv")

##2.3 mehrfaktorielle Anova
# klone den originaler Datensatz
df_manova <- nova_ind 

# Daten vorbereiten
df_manova %>% # schaut euch das Package "magrittr" an
  # ersetze Local mit einem leeren String
  mutate(article_description = str_replace(article_description, "Local ", "")) %>% 
  filter(article_description != "Hot and Cold") %>% # lasse Buffet Gerichte weg
  filter(member != "Spezialkarten") %>% # Spezialkarten können vernachlässigt werden
  #  fasse die zwei Menülinien "World & Favorite" zusammen
  mutate(article_description = str_replace_all(article_description, "Favorite|World",
                                               "Fav_World"))  
# gruppiere Daten nach Menülinie, Geschlecht und Hochschulzugehörigkeit
df_manova %<>%
  group_by(article_description, member, week) %>% 
  summarise(tot_sold = n()) %>%
  ungroup() %>% 
  drop_na()  # lasst die unbekannten Menü-Inhalte weg

write_csv2(df_manova, "stat1-4/Statistik2.1_Uebung.csv")
