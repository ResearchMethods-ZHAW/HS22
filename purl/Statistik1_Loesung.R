<<<<<<< HEAD
# so kannst du die Daten direkt von der Webpage laden
nova <- read_delim(file = "https://zenodo.org/record/3890931/files/2017_ZHAW_aggregated_menu_sales_NOVANIMAL.csv?download=1", delim = ";")
nova_survey <- read_delim(file = "https://zenodo.org/record/3554884/files/2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv?download=1", delim = ";")
=======
options(knitr.duplicate.label = "allow")
knitr::purl("stat1-4/Statistik1_Loesung.qmd", "purl/Statistik1_Loesung.R", documentation = 0)
>>>>>>> bc745010dd48842e320bd76811fb7d43f3c8f76e



## ladet die nötigen Packete und die novanimal.csv Datei in R

library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)


## definiert mytheme für ggplot2 (verwendet dabei theme_classic())
mytheme <- 
  theme_classic() + 
  theme(
    axis.line = element_line(color = "black"), 
    axis.text = element_text(size = 12, color = "black"), 
    axis.title = element_text(size = 12, color = "black"), 
    axis.ticks = element_line(size = .75, color = "black"), 
    axis.ticks.length = unit(.5, "cm")
    )



# Als eine Möglichkeit, die Aufgabe 1.1 zu bearbeiten, nehmen wir hier den 
# Datensatz  der Gästebefragung NOVANIMAL und gehen der folgenden Frage nach: 
# Gibt es einen Zusammenhang zwischen Geschlecht und dem wahrgenommenen 
# Milchkonsum (viel vs. wenig Milch/-produkte)

# die Variable wahrgenommener Milchkonsum muss 
# noch in 2 Kategorien zusammengefasst werden: geringer vs. hoher Milchkonsum

# Variable  milk == wahrgenommener Milchkonsum 
# alles kleiner als 4 (3 inklusive) == geringer wahrgenommener Milchkonsum, 
#alles grösser als 3 (4 inklusive) == hoher wahrgenommener Milchkonsum
nova_survey <- read_delim("datasets/statistik/2019_ZHAW_vonRickenbach_cleaned_recorded_survey_dataset_NOVANIMAL_anonym.csv") |> 
  dplyr::select(gender, milk)

#check die Daten zB.
head(nova_survey, 5)
str(nova_survey)

nova2 <- nova_survey |>
  filter(gender != "x") |> # x aus der Variable Geschlecht entfernen 
  mutate(milkcon = if_else(milk <= 3, "wenig", "viel")) |> 
  select(gender, milkcon) |> 
  drop_na() # alle Missings können gestrichen werden
 
# mal anschauen
table(nova2)

#achtung chi_squre erwartet matrix
nova_mtx <- xtabs(~ gender + milkcon ,data = nova2) 
# da es in diesem fall keine kriteriumsvariable gibt, fehlt das y sozusagen

#Chi-squared Test
chi_sq <- chisq.test(nova_mtx)


#visualisierung
OP <- par(mfrow=c(1,2), "mar"=c(1,1,3,1))
mosaicplot(chi_sq$observed, cex.axis =1 , main = "Observed counts")
mosaicplot(chi_sq$expected, cex.axis =1 , main = "Expected counts\n(wenn geschlecht keinen einfluss hat)")
par(OP)

#Fisher's Test nur mit 2X2 Kontingenztabelle möglich
fisher.test(nova_mtx)

table <- nova2 %>%
  group_by(gender, milkcon) |>
  summarise(tot = n()) |>
  mutate(`wahr. Milchkonsum (%)` = round(tot / sum(tot) * 100, 1)) |>
  rename(Geschlecht = gender, `wahr. Milchkonsum` = milkcon, `absolute Werte`= tot)

knitr::kable(table, caption = "Wahrgenommener Milchkonsum nach Geschlecht")

#lade Daten
df <- readr::read_csv2("datasets/statistik/Datensatz_novanimal_Uebung_Statistik1.2.csv")


# überprüft die Voraussetzungen für einen t-Test
ggplot2::ggplot(df, aes(x = condit, y= tot_sold)) + # achtung 0 Punkt fehlt
    geom_boxplot(fill = "white", color = "black", size = 1) + 
    labs(x="\nBedingungen", y="Durchschnittlich verkaufte Gerichte pro Woche\n") + 
    mytheme

# Auf den ersten Blick scheint es keine starken Abweichungen zu einer 
#Normalverteilung zu geben resp. es sind keine extremen schiefen Verteilungen
# ersichtlich (vgl. Skript Statistik 2)

# führt einen t-Tests durch; 
# es wird angenommen, dass die Verkaufszahlen zwischen den Bedingungen 
# unabhängig sind

t_test <- t.test(tot_sold ~ condit, data=df, var.equl = T)
t_test

#alternative Formulierung
t.test(df[df$condit == "Basis", ]$tot_sold, 
                 df[df$condit == "Intervention", ]$tot_sold) 
