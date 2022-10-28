#.################################################################################################
# Einfluss von COVID19 auf die Besucherzahlen im WPZ waehrend der dunklen Tageszeit ####
# Modul Research Methods, HS22. Adrian Hochreutener ####
#.################################################################################################

#.################################################################################################
# METADATA UND DEFINITIONEN ####
#.################################################################################################

# save and load workspace
save.image(file = "fallstudie_s/results/my_work_space.RData")
load(file = "fallstudie_s/results/my_work_space.RData")

# Datenherkunft ####
# Saemtliche verwendeten Zaehdaten sind Eigentum des Wildnispark Zuerich und duerfen nur im Rahmen 
# des Moduls verwendet werden. Sie sind vertraulich zu behandeln.
# Die Meteodaten sind Eigentum von MeteoSchweiz.

# Verwendete Meteodaten
# Lufttemperatur 2 m UEber Boden; Tagmaximum (6 UTC blis 18 UTC), tre200jx [°C ]
# Niederschlag; Halbtagessumme 6 UTC - 18 UTC, rre150j0 [mm]


# Ordnerstruktur ####
# Im Ordner in dem das R-Projekt abgelegt ist muessen folgende Unterordner bestehen:
# - data (Rohdaten hier ablegen)
# - results
# - scripts

# Benoetigte Bibliotheken ####
library(tidyverse) # Data wrangling und piping
library(lubridate) # Arbeiten mit Datumsformaten
library(suncalc)   # berechne Tageszeiten abhaengig vom Sonnenstand
library(ggpubr)    # to arrange multiple plots in one graph
library(PerformanceAnalytics) # Plotte Korrelationsmatrix
library(MuMIn)     # Multi-Model Inference
library(AICcmodavg)# Modellaverageing
library(fitdistrplus)# Prueft die Verteilung in Daten
library(lme4)      # Multivariate Modelle
library(blmeco)    # Bayesian data analysis using linear models
library(sjPlot)    # Plotten von Modellergebnissen (tab_model)
library(lattice)   # einfaches plotten von Zusammenhängen zwischen Variablen


# definiere ein farbset zur wiedervewendung
mycolors <- c("orangered","gold", "mediumvioletred", "darkblue")

# Start und Ende ####
# Untersuchungszeitraum, ich waehle hier das Jahr 2019 bis und mit Sommer 2021
depo_start <- as.Date("2017-01-01")
depo_end <- as.Date("2022-7-31")

# Start und Ende Lockdown
# definieren, wichtig fuer die spaeteren Auswertungen
lock_1_start_2020 <- as.Date("2020-03-16")
lock_1_end_2020 <- as.Date("2020-05-11")

lock_2_start_2021 <- as.Date("2020-12-22")
lock_2_end_2021 <- as.Date("2021-03-01")

# Ebenfalls muessen die erste und letzte Kalenderwoche der Untersuchungsfrist definiert werden
# Diese werden bei Wochenweisen Analysen ebenfalls ausgeklammert da sie i.d.R. unvollstaendig sind
KW_start <- isoweek(depo_start)
KW_end <- isoweek(depo_end)

# Erster und letzter Tag der Ferien
# je nach Untersuchungsdauer muessen hier weitere oder andere Ferienzeiten ergaenzt werden
# (https://www.schulferien.org/schweiz/ferien/2020/)

Winterferien_2016_start <- as.Date("2017-01-01") 
Winterferien_2016_ende <- as.Date("2017-01-08")

Fruehlingsferien_2017_start <- as.Date("2017-04-15") 
Fruehlingsferien_2017_ende <- as.Date("2017-04-30") 
Sommerferien_2017_start <- as.Date("2017-07-15") 
Sommerferien_2017_ende <- as.Date("2017-08-20") 
Herbstferien_2017_start <- as.Date("2017-10-07") 
Herbstferien_2017_ende <- as.Date("2017-10-22") 
Winterferien_2017_start <- as.Date("2017-12-23") 
Winterferien_2017_ende <- as.Date("2018-01-07") 

Fruehlingsferien_2018_start <- as.Date("2018-04-21") 
Fruehlingsferien_2018_ende <- as.Date("2018-05-06") 
Sommerferien_2018_start <- as.Date("2018-07-14") 
Sommerferien_2018_ende <- as.Date("2018-08-19") 
Herbstferien_2018_start <- as.Date("2018-10-06") 
Herbstferien_2018_ende <- as.Date("2018-10-21") 
Winterferien_2018_start <- as.Date("2018-12-22") 
Winterferien_2018_ende <- as.Date("2019-01-06") 

Fruehlingsferien_2019_start <- as.Date("2019-04-20") 
Fruehlingsferien_2019_ende <- as.Date("2019-05-05") 
Sommerferien_2019_start <- as.Date("2019-07-13") 
Sommerferien_2019_ende <- as.Date("2019-08-18") 
Herbstferien_2019_start <- as.Date("2019-10-05") 
Herbstferien_2019_ende <- as.Date("2019-10-20") 
Winterferien_2019_start <- as.Date("2019-12-21") 
Winterferien_2019_ende <- as.Date("2020-01-05")

Fruehlingsferien_2020_start <- as.Date("2020-04-11")
Fruehlingsferien_2020_ende <- as.Date("2020-04-26")
Sommerferien_2020_start <- as.Date("2020-07-11")
Sommerferien_2020_ende <- as.Date("2020-08-16")
Herbstferien_2020_start <- as.Date("2020-10-03")
Herbstferien_2020_ende <- as.Date("2020-10-18")
Winterferien_2020_start <- as.Date("2020-12-19")
Winterferien_2020_ende <- as.Date("2021-01-03")

Fruehlingsferien_2021_start <- as.Date("2021-04-24")
Fruehlingsferien_2021_ende <- as.Date("2021-05-09")
Sommerferien_2021_start <- as.Date("2021-07-17")
Sommerferien_2021_ende <- as.Date("2021-08-22")
Herbstferien_2021_start <- as.Date("2021-10-09")
Herbstferien_2021_ende <- as.Date("2021-10-24")
Winterferien_2021_start <- as.Date("2021-12-18")
Winterferien_2021_ende <- as.Date("2022-01-02")

Fruehlingsferien_2022_start <- as.Date("2022-04-16")
Fruehlingsferien_2022_ende <- as.Date("2022-05-01")
Sommerferien_2022_start <- as.Date("2022-07-16")
Sommerferien_2022_ende <- as.Date("2022-08-21")
Herbstferien_2022_start <- as.Date("2022-10-08")
Herbstferien_2022_ende <- as.Date("2022-10-23")
Winterferien_2022_start <- as.Date("2022-12-24")
Winterferien_2022_ende <- as.Date("2023-01-08")

#.################################################################################################
# 1. DATENIMPORT #####
#.################################################################################################

# Beim Daten einlesen koennen sogleich die Datentypen und erste Bereinigungen vorgenommen werden

# 1.1 Zaehldaten ####
# Die Zaehldaten des Wildnispark wurden vorgaengig bereinigt. z.B. wurden Stundenwerte 
# entfernt, an denen am Zaehler Wartungsarbeiten stattgefunden haben.

# lese die Daten ein 
# Je nach Bedarf muss der Speicherort sowie der Dateiname angepasst werden
depo <- read.csv("./data/211_sihlwaldstrasse_2017_2022.csv", sep = ";")

# Hinweis zu den Daten:
# In hourly analysis format, the data at 11:00 am corresponds to the counts saved between 
# 11:00 am and 12:00 am.

# Anpassen der Datentypen und erstes Sichten
str(depo)

depo <- depo |>
  mutate(Datum = as.character(Datum)) |>
    mutate(Datum = as.Date(Datum, format = "%Y%m%d"))
  # Schneide das df auf den gewuenschten Zeitraum zu
  # filter(Datum >= depo_start, Datum <=  depo_end) # das Komma hat die gleiche Funktion wie ein &

# In dieser Auswertung werden nur Personen zu Fuss betrachtet!
# it select werden spalten ausgewaehlt oder eben fallengelassen
depo <- depo |> dplyr::select(-c(Velo_IN, Velo_OUT))

# Berechnen des Totals, da dieses in den Daten nicht vorhanden ist
depo <- depo|>
  mutate(Total = Fuss_IN + Fuss_OUT)

# Entferne die NA's in dem df.
depo <- na.omit(depo)

# 1.2 Meteodaten ####
# Einlesen
meteo <- read.csv("./data/order_105742_data.txt", sep = ";")

# Datentypen setzen
# Das Datum wird als Integer erkannt. Zuerst muss es in Text umgewaldelt werden aus dem dann
# das eigentliche Datum herausgelesen werden kann
meteo <- transform(meteo, time = as.Date(as.character(time), "%Y%m%d"))

# Die eigentlichen Messwerte sind alle nummerisch
meteo <- meteo|>
  mutate(tre200jx = as.numeric(tre200jx))|>
  mutate(rre150j0 = as.numeric(rre150j0))|>
  mutate(sremaxdv = as.numeric(sremaxdv)) |> 
  filter(time >= depo_start, time <=  depo_end) # schneide dann auf Untersuchungsdauer

# Was ist eigentlich Niederschlag:
# https://www.meteoschweiz.admin.ch/home/wetter/wetterbegriffe/niederschlag.html

# Filtere Werte mit NA
meteo <- meteo |>
  filter(!is.na(stn)) |>
  filter(!is.na(time))|>
  filter(!is.na(tre200jx))|>
  filter(!is.na(rre150j0))|>
  filter(!is.na(sremaxdv))
# Pruefe ob alles funktioniert hat
str(meteo)
sum(is.na(meteo)) # zeigt die Anzahl NA's im data.frame an

#.################################################################################################
# 2. VORBEREITUNG DER DATEN #####
#.################################################################################################

# 2.1 Convinience Variablen ####
# fuege dem Dataframe (df) die Wochentage hinzu
depo <- depo |> 
  mutate(Wochentag = weekdays(Datum)) |> 
  # R sortiert die Levels aplhabetisch. Da das in unserem Fall aber sehr unpraktisch ist,
  # muessen die Levels manuell manuell bestimmt werden
  mutate(Wochentag = base::factor(Wochentag, 
                            levels = c("Montag", "Dienstag", "Mittwoch", 
                                       "Donnerstag", "Freitag", "Samstag", "Sonntag"))) |> 
  # Werktag oder Wochenende hinzufuegen
  mutate(Wochenende = if_else(Wochentag == "Montag" | Wochentag == "Dienstag" | 
                           Wochentag == "Mittwoch" | Wochentag == "Donnerstag" | 
                           Wochentag == "Freitag", "Werktag", "Wochenende"))|>
  #Kalenderwoche hinzufuegen
  mutate(KW= isoweek(Datum))|>
  # monat und Jahr
  mutate(Monat = month(Datum)) |> 
  mutate(Jahr = year(Datum))

#Lockdown 
# Hinweis: ich mache das nachgelagert, da ich die Erfahrung hatte, dass zu viele 
# Operationen in einem Schritt auch schon mal durcheinander erzeugen koennen.
# Hinweis II: Wir packen alle Phasen (normal, die beiden Lockdowns und Covid aber ohne Lockdown)
# in eine Spalte --> long ist schoener als wide
depo <- depo |>
  mutate(Phase = if_else(Datum >= lock_1_start_2020 & Datum <= lock_1_end_2020,
                         "Lockdown_1",
                         if_else(Datum >= lock_2_start_2021 & Datum <= lock_2_end_2021,
                                 "Lockdown_2",
                                 if_else(Datum>= (lock_1_start_2020 - years(1)) & Datum < lock_1_start_2020,
                                         "Normal", 
                                         if_else(Datum > lock_2_end_2021,
                                                 "Post", "Pre")))))

# hat das gepklappt?!
unique(depo$Phase)

# aendere die Datentypen
depo <- depo |> 
  mutate(Wochenende = as.factor(Wochenende)) |> 
  mutate(KW = factor(KW)) |> 
  # mit factor() koennen die levels direkt einfach selbst definiert werden.
  # wichtig: speizfizieren, dass aus R base, ansonsten kommt es zu einem 
  # mix-up mit anderen packages
  mutate(Phase = base::factor(Phase, levels = c("Pre", "Normal", "Lockdown_1", "Lockdown_2", "Post")))

str(depo)
  
# Fuer einige Auswertungen muss auf die Stunden als nummerischer Wert zurueckgegriffen werden
depo$Stunde <- as.numeric(format(as.POSIXct(depo$Zeit,format="%H:%M:%S"),"%H"))

# ersetze 0 Uhr mit 24 Uhr (damit wir besser rechnen können)
depo$Stunde[depo$Stunde == 0] <- 24
unique(depo$Stunde)
typeof(depo$Stunde)

# Die Daten wurden kalibriert. Wir runden sie fuer unserer Analysen auf Ganzzahlen
depo$Total <- round(depo$Total, digits = 0)
depo$Fuss_IN <- round(depo$Fuss_IN, digits = 0)
depo$Fuss_OUT <- round(depo$Fuss_OUT, digits = 0)

# 2.2 Tageszeit hinzufuegen ####

# Einteilung Standort Zuerich
Latitude <- 47.38598
Longitude <- 8.50806

# Zur Berechnung der Tageslaege muessen wir zuerst den Start und das Ende der Sommer-
# zeit definieren
# https://www.schulferien.org/schweiz/zeit/zeitumstellung/

So_start_2017 <- as.Date("2017-03-26") 
So_end_2017 <- as.Date("2017-10-29") 
So_start_2018 <- as.Date("2018-03-25") 
So_end_2018 <- as.Date("2018-10-28") 
So_start_2019 <- as.Date("2019-03-31") 
So_end_2019 <- as.Date("2019-10-27") 
So_start_2020 <- as.Date("2020-03-29")
So_end_2020 <- as.Date("2020-10-25")
So_start_2021 <- as.Date("2021-03-28")
So_end_2021 <- as.Date("2021-10-31")
So_start_2022 <- as.Date("2022-03-27")
So_end_2022 <- as.Date("2022-10-30")

# Welche Zeitzone haben wir eigentlich?
# Switzerland uses Central European Time (CET) during the winter as standard time, 
# which is one hour ahead of Coordinated Universal Time (UTC+01:00), and 
# Central European Summer Time (CEST) during the summer as daylight saving time, 
# which is two hours ahead of Coordinated Universal Time (UTC+02:00).
# https://en.wikipedia.org/wiki/Time_in_Switzerland

# Was sind Astronomische Dämmerung und Golden Hour ueberhaupt?
# https://sunrisesunset.de/sonne/schweiz/zurich-kreis-1-city/
# https://www.rdocumentation.org/packages/suncalc/versions/0.5.0/topics/getSunlightTimes

# Wir arbeiten mit folgenden Variablen:
# "nightEnd" : night ends (morning astronomical twilight starts)
# "goldenHourEnd" : morning golden hour (soft light, best time for photography) ends
# "goldenHour" : evening golden hour starts
# "night" : night starts (dark enough for astronomical observations)

lumidata <-
  getSunlightTimes(
    date = seq.Date(depo_start, depo_end, by = 1),
    keep = c("nightEnd", "goldenHourEnd", "goldenHour", "night"),
    lat = Latitude,
    lon = Longitude,
    tz = "CET")

lumidata <- lumidata |> 
  mutate(Jahreszeit = ifelse(date >= So_start_2017 & date <=  So_end_2017 |
                               date >= So_start_2018 & date <=  So_end_2018 |
                               date >= So_start_2019 & date <=  So_end_2019 |
                               date >= So_start_2020 & date <= So_end_2020 |
                               date >= So_start_2021 & date <= So_end_2021 |
                               date >= So_start_2022 & date <= So_end_2022, 
                               "Sommerzeit", "Winterzeit"))

# CH ist im Im Sommer CET + 1. 
# Darum auf alle relevanten Spalten eine Stunde addieren
# hinweis: ich verzichte hier auf ifelse, da es einfacher und nachvollziehbarer scheint,
# hier mit einem filter die betreffenden Spalten zu waehlen
lumidata_So <- lumidata |> 
  filter(Jahreszeit=="Sommerzeit") |> 
  mutate(nightEnd = nightEnd + hours(1),
         goldenHourEnd =  goldenHourEnd + hours(1),
         goldenHour = goldenHour + hours(1),
         night = night + hours(1))

lumidata_Wi <- lumidata |> 
  filter(Jahreszeit=="Winterzeit") 
# verbinde sommer- und winterzeit wieder
lumidata <- rbind(lumidata_So, lumidata_Wi) |> 
  arrange(date)

# change data type
lumidata$date <- as.Date(lumidata$date, format= "%Y-%m-%d")

# drop unnecessary cols
lumidata <- lumidata |> dplyr::select(-lat, -lon)

# jetzt haben wir alle noetigen Angaben zu Sonnenaufgang, Tageslaenge usw. 
# diese Angaben koennen wir nun mit unseren Zaehldaten verbinden:
depo <- left_join(depo,lumidata, by = c("Datum" ="date"))

# aendere alle Zeit- und Datumsangaben so, dass sie gleich sind und miteinander verrechnet werden können.
depo <- depo |> 
  mutate(datetime = paste(Datum, Zeit)) |> 
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d  %H:%M:%S"))|> 
  mutate(nightEnd = as.POSIXct(nightEnd)) |> 
  mutate(goldenHourEnd = as.POSIXct(goldenHourEnd)) |> 
  mutate(goldenHourEnd = goldenHourEnd + hours(1)) |> 
  mutate(goldenHour = as.POSIXct(goldenHour)) |> 
  mutate(goldenHour = goldenHour - hours(1)) |> 
  mutate(night = as.POSIXct(night))

# im naechsten Schritt weise ich den Stunden die Tageszeiten Morgen, Tag, Abend und Nacht zu.
# diese Zuweisung basiert auf der Einteilung gem. suncalc und eigener Definition.
depo <- depo|>
  mutate(Tageszeit = if_else(datetime >= nightEnd & datetime <= goldenHourEnd, "Morgen",
                             ifelse(datetime > goldenHourEnd & datetime < goldenHour, "Tag",
                                    ifelse(datetime >= goldenHour & datetime <= night,
                                           "Abend",
                                           "Nacht")))) |>
  mutate(Tageszeit = factor(Tageszeit, levels = c(
    "Morgen", "Tag", "Abend", "Nacht")))

# # behalte die relevanten Var
depo <- depo |> dplyr::select(-nightEnd, -goldenHourEnd, -goldenHour, -night)

#Plotte zum pruefn ob das funktioniert hat
p <- ggplot(depo, aes(y = Datum, color = Tageszeit, x = Stunde))+
  geom_jitter()+
  scale_color_manual(values=mycolors)

plotly::ggplotly(p)

# bei mir hat der Zusatz der Tageszeit noch zu einigen NA-Wertren gefueht. 
# Diese loesche ich einfach:
depo <- na.omit(depo)
# hat das funktioniert?
sum(is.na(depo))

# # 2.3 Pruefen auf Ausreisser (OPTIONAL) ####
# # Grundsaetzlich ist es schwierig Ausreisser in einem df zu finden. Gerade die Extremwerte 
# # koennen entweder falsche Werte, oder aber die wichtigsten Werte im ganzen df sein.
# # Ausreisser koennen in einem ersten Schritt optisch relativ einfach gefunden werden.
# # Dazu werden die Zaehlmengen auf der y-Achse und die einzelne Datenpunkte auf der x-Achse
# # geplottet. Wenn einzelne Werte nun die umliegenden bei weiten ueberragen, sollten diese
# # genauer angeschaut werden. Sind sie an einem Wochenende? Ist die Tageszeit realisitsch?
# # Wie mit Ausreissern umgegangen wird, muss von Fall zu Fall individuell entschieden werden.
# 
# # Verteilung mittels Histogram pruefen
# hist(depo$Total[!depo$Total==0] , breaks = 100) 
# # hier schliesse ich die Nuller aus der Visualisierung aus
# 
# # Verteilung mittels Scatterplot pruefen
# plot(x=depo$Datum, y=depo$Total)
# # Dem Scatterplot kann nun eine horizontale Linie hinzugefuegt werden, die bei 95 % der
# # Werte liegt. Berchnet wird die Linie mittels dem 95 % Quartil
# qts <- quantile(depo$Total,probs=c(0,.95))
# abline(h=qts[2], col="red")
# 
# # Werte ueber 95 % auflisten
# # Nun koennen diese optisch identifizierten Werte aufgelistet werden. Jeder einzele Wert
# # sollte nun auf die Plausibilitaet geprueft werden. Dies sowohl bei Total als auch in
# # den einzelnen Richtungen (IN, OUT)
# (
#   Ausreisser <- depo |>
#     filter(Total > qts[2])|> 
#     arrange(desc(Total))
# )
# 
# # Werte ausschliessen; Aufgrund manueller Inspektion
# # Die Werte, welche als Ausreisser identifiziert wurden, werden nun aufgrund der Zeilennummer 
# # ausgeschlossen. Das muss individuell angepasst werden.
# # depo <- depo[-c(Zeilennummer),] # ersetze "Zeilennummer" mit Zahlen, Kommagetrennt
# 
# # Da der WPZ die Daten aber bereits bereinigte, koennen wir uns diesen Schritt eigentlich sparen... ;)
# 
# # pruefe das df
# str(depo)
# head(depo)


# 2.4 Aggregierung der Stundendaten zu ganzen Tagen ####
# Zur Berechnung von Kennwerten ist es hilfreich, wenn neben den Stundendaten auch auf Ganztagesdaten
# zurueckgegriffen werden kann
# hier werden also pro Nutzergruppe und Richtung die Stundenwerte pro Tag aufsummiert
depo_d <- depo |> 
  group_by(Datum, Wochentag, Wochenende, KW, Monat, Jahr, Phase) |> 
  summarise(Total = sum(Fuss_IN + Fuss_OUT), 
            Fuss_IN = sum(Fuss_IN),
            Fuss_OUT = sum(Fuss_OUT)) 
# Wenn man die Convinience Variablen als grouping variable einspeisst, dann werden sie in 
# das neue df uebernommen und muessen nicht nochmals hinzugefuegt werden
# pruefe das df
head(depo_d)

# nun gruppieren wir nicht nur nach Tag sondern auch noch nach Tageszeit
depo_daytime <- depo |> 
  group_by(Datum, Wochentag, Wochenende, KW, Monat, Jahr, Phase, Tageszeit) |> 
  summarise(Total = sum(Fuss_IN + Fuss_OUT), 
            Fuss_IN = sum(Fuss_IN),
            Fuss_OUT = sum(Fuss_OUT)) 


# Gruppiere die Werte nach Monat
depo_m <- depo |> 
  group_by(Jahr, Monat) |> 
  summarise(Total = sum(Total)) 
# sortiere das df aufsteigend (nur das es sicher stimmt)
depo_m <- as.data.frame(depo_m)
depo_m[
  with(depo_m, order(Jahr, Monat)),]
depo_m <- depo_m |> 
  mutate(Ym = paste(Jahr, Monat)) |> # und mache eine neue Spalte, in der Jahr und
  mutate(Ym= lubridate::ym(Ym)) # formatiere als Datum

# Gruppiere die Werte nach Monat und TAGESZEIT
depo_m_daytime <- depo |> 
  group_by(Jahr, Monat, Tageszeit) |> 
  summarise(Total = sum(Total)) 
# sortiere das df aufsteigend (nur das es sicher stimmt)
depo_m_daytime <- as.data.frame(depo_m_daytime)
depo_m_daytime[
  with(depo_m_daytime, order(Jahr, Monat)),]
depo_m_daytime <- depo_m_daytime |> 
  mutate(Ym = paste(Jahr, Monat)) |> # und mache eine neue Spalte, in der Jahr und
  mutate(Ym= lubridate::ym(Ym)) # formatiere als Datum

#.################################################################################################
# 3. DESKRIPTIVE ANALYSE UND VISUALISIERUNG #####
#.################################################################################################

# 3.1 Verlauf der Besuchszahlen / m ####
# Monatliche Summen am Standort

# wann beginnt die Datenreihe schon wieder?
first(depo_m$Ym)
# und wann ist die fertig?
last(depo_m$Ym)

# Plotte
ggplot(depo_m, mapping = aes(Ym, Total, group = 1))+ # group = 1 braucht R, dass aus den Einzelpunkten ein Zusammenhang hergestellt wird
  #zeichne Lockdown 1
  geom_rect(mapping = aes(xmin = ym("2020-3"), xmax = ym("2020-5"),
                          ymin = 0, ymax = max(Total+(Total/100*10))),
            fill = "lightskyblue", alpha = 0.2, colour = NA)+
  #zeichne Lockdown 2
  geom_rect(mapping = aes(xmin = ym("2020-12"), xmax = ym("2021-3"), 
                          ymin = 0, ymax = max(Total+(Total/100*10))), 
            fill = "lightskyblue", alpha = 0.2, colour = NA)+
  geom_line(alpha = 0.6, size = 1.5)+
  labs(title= "", y="Fussgänger:innen pro Monat", x = "Jahr")+
  theme_linedraw(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("Entwicklung_Zaehlstelle.png", width=20, height=10, units="cm", dpi=1000, 
       path = "fallstudie_s/results/") 

# # mit TAGESZEIT
# ggplot(depo_m_daytime, mapping = aes(Ym, Total, group = Tageszeit, color = Tageszeit))+
#   #zeichne Lockdown 1
#   geom_rect(mapping = aes(xmin = ym("2020-3"), xmax = ym("2020-5"),
#                           ymin = 0, ymax = max(Total+(Total/100*10))),
#             fill = "lightskyblue", alpha = 0.2, colour = NA)+
#   #zeichne Lockdown 2
#   geom_rect(mapping = aes(xmin = ym("2020-12"), xmax = ym("2021-3"), 
#                           ymin = 0, ymax = max(Total+(Total/100*10))), 
#             fill = "lightskyblue", alpha = 0.2, colour = NA)+
#   geom_line(alpha = 0.8, size = 1.5)+
#   labs(title= "", y="Fussgänger:innen pro Monat", x = "Jahr")+
#   scale_color_manual(values = mycolors)+
#   # scale_y_continuous(trans="log10")+
#   theme_linedraw(base_size = 15)+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# mache einen prozentuellen areaplot
ggplot(depo_m_daytime, aes(Ym, Total, fill = Tageszeit)) + 
  geom_area(position = "fill")+
  scale_fill_manual(values = mycolors)+
  theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(title= "", y="Verteilung Fussgänger:innen / Monat [%]", x = "Jahr")
  
ggsave("Proz_Entwicklung_Zaehlstelle.png", width=20, height=10, units="cm", dpi=1000, 
       path = "fallstudie_s/results/")  


# lineplot nur Dunkel und FACET
ggplot(subset(depo_m_daytime, !(Tageszeit %in% "Tag")), 
       mapping = aes(Monat, Total, group = Tageszeit, color = Tageszeit))+
  geom_line(alpha = 0.8, size = 1.5)+
  labs(title= "", y="Fussgänger:innen pro Monat", x = "Monat")+
  scale_color_manual(values = mycolors)+
  facet_grid(rows = vars(Jahr), scales = "free_x")+
  # scale_y_continuous(trans="log10")+
  theme_linedraw(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# # lineplot nur DUNKEL
# ggplot(subset(depo_m_daytime, !(Tageszeit %in% "Tag")), mapping = aes(Ym, Total, group = Tageszeit, color = Tageszeit))+
#   #zeichne Lockdown 1
#   geom_rect(mapping = aes(xmin = ym("2020-3"), xmax = ym("2020-5"),
#                           ymin = 0, ymax = max(Total+(Total/100*10))),
#             fill = "lightskyblue", alpha = 0.2, colour = NA)+
#   #zeichne Lockdown 2
#   geom_rect(mapping = aes(xmin = ym("2020-12"), xmax = ym("2021-3"), 
#                           ymin = 0, ymax = max(Total+(Total/100*10))), 
#             fill = "lightskyblue", alpha = 0.2, colour = NA)+
#   geom_line(alpha = 0.4, size = 1.5)+
#   geom_smooth(method = "lm", se = F, lwd = 2)+
#   # scale_x_discrete(breaks = c("2019 1", "2019 7","2019 1","2020 1","2020 7","2021 1","2021 7"),
#   #                  labels = c("2019 1", "2019 7","2019 1","2020 1","2020 7","2021 1","2021 7"))+
#   labs(title= "", y="Fussgänger:innen pro Monat", x = "Jahr")+
#   scale_color_manual(values = mycolors)+
#   # scale_y_continuous(trans="log10")+
#   theme_linedraw(base_size = 15)+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# berechne Regression für die Besuchszahlen in den Tageszeiten
summary(lm(Total~Jahr + Tageszeit, data = depo))

# berechne ctree
library(partykit)

ct <- ctree(Total ~ Jahr + Phase + Tageszeit, 
            data = depo, maxdepth = 5)

# we can inspect the results via a print method
ct
st <- as.simpleparty(ct)

# https://stackoverflow.com/questions/13751962/how-to-plot-a-large-ctree-to-avoid-overlapping-nodes
# ?plot.party

# WICHTIG: um ueberlappungen zu vermeiden, plotte Bild, oeffne im separaten Fenster, amche Screenshot und speichere unter ctrees.png ab.

plot(st, gp = gpar(fontsize = 10),
     inner_panel=node_inner,
     ep_args = list(justmin = 10),
     ip_args=list(
       abbreviate = FALSE,
       id = FALSE))

# calendar heat chart (not working)
# library(viridis)
# ggplot(depo, aes(Datum, Stunde, fill=Total))+
#   geom_tile(color= "white",size=0.1) +
#   scale_fill_viridis(name="Passagen",option ="C")+
#   facet_grid(cols = vars(Monat), rows = vars(Jahr), scales = "free", space = "free")+
#   scale_y_continuous(trans = "reverse", breaks = unique(depo$Stunde)) +
#   theme_minimal(base_size = 15) +
#   labs(title= "", x="Datum", y="Uhrzeit [h]")+
#   theme(axis.text.x = element_blank(),
#         axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

# 3.2 Wochengang ####

# mean / d / phase
mean_phase_wd <- depo_d |> 
  group_by(Wochentag, Phase) |> 
  summarise(Total = mean(Total))

write.csv(mean_phase_wd, "fallstudie_s/results/mean_phase_wd.csv")

#plot
ggplot(data = depo_d)+
  geom_boxplot(mapping = aes(x= Wochentag, y = Total, fill = Phase))+
  labs(title="", y= "Fussgänger:innen pro Tag")+
  scale_fill_manual(values = c("lightgray", "royalblue", "red4", "orangered", "gold2"))+
  theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank())

ggsave("Wochengang_Lockdown.png", width=15, height=15, units="cm", dpi=1000, 
       path = "fallstudie_s/results/")


# Wochengang, unterteilt nach TAGESZEIT

# ggplot(data = depo_daytime)+
#   geom_boxplot(mapping = aes(x= Wochentag, y = Total, fill = Phase))+
#   labs(title="", y= "Fussgänger:innen pro Tag")+
#   scale_fill_manual(values = c("lightgray", "royalblue", "red4", "orangered", "gold2"))+
#   facet_grid(rows = vars(Tageszeit), scales = "free")+
#   theme_classic(base_size = 15)+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
#         legend.title = element_blank())


# Statistik: Unterschied WE und WO während Lockdown 1
t.test(depo_d$Total [depo_d$Phase == "Lockdown_1" & depo_d$Wochenende=="Werktag"], 
       depo_d$Total [depo_d$Phase == "Lockdown_1" & depo_d$Wochenende=="Wochenende"])


# 3.3 Tagesgang ####
# Bei diesen Berechnungen wird jeweils der Mittelwert pro Stunde berechnet. 
# wiederum nutzen wir dafuer "pipes"
Mean_h <- depo |> 
  group_by(Wochentag, Stunde, Phase) |> 
  summarise(Total = mean(Total)) 

# Plotte den Tagesgang, unterteilt nach Wochentagen

ggplot(Mean_h, aes(x = Stunde, y = Total, colour = Wochentag, linetype = Wochentag))+
  geom_line(size = 2)+
  scale_colour_viridis_d()+
  scale_linetype_manual(values = c(rep("solid", 5),  "twodash", "twodash"))+
  scale_x_continuous(breaks = c(seq(0, 23, by = 2)), labels = c(seq(0, 23, by = 2)))+
  facet_grid(rows = vars(Phase))+
  labs(x="Uhrzeit [h]", y= "∅ Fussganger_Innen / h", title = "")+
  lims(y = c(0,25))+
  theme_linedraw(base_size = 15)

ggsave("Tagesgang.png", width=25, height=25, units="cm", dpi=1000,
       path = "fallstudie_s/results/")

# 3.4 Kennzahlen ####
total_phase <- depo_daytime |> 
  # gruppiere nach Phasen inkl. Normal. Diese Levels haben wir bereits definiert
  group_by(Phase, Tageszeit) |> 
  summarise(Total = sum(Total),
            IN = sum(Fuss_IN),
            OUT = sum(Fuss_OUT))

write.csv(total_phase, "fallstudie_s/results/total_phase.csv")

# mean besser Vergleichbar, da Zeitreihen unterschiedlich lange
mean_phase_d <- depo_daytime |> 
  group_by(Phase, Tageszeit) |> 
  summarise(Total = mean(Total),
            IN = mean(Fuss_IN),
            OUT = mean(Fuss_OUT))
# berechne prozentuale Richtungsverteilung
mean_phase_d <- mean_phase_d |> 
  mutate(Proz_IN = round(100/Total*IN, 1)) |> # berechnen und auf eine Nachkommastelle runden
  mutate(Proz_OUT = round(100/Total*OUT,1))

write.csv(mean_phase_d, "fallstudie_s/results/mean_phase_d.csv")

# plotte die Verteilung der Fussgänger nach Tageszeit abhängig von der Phase
ggplot(mean_phase_d, mapping = aes(Phase, Total, fill=Tageszeit)) + 
  geom_col(position = "fill")+
  scale_fill_manual(values = mycolors)+
  theme_classic(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(title= "", y="Verteilung Fussgänger:innen nach Tageszeit [%]", x = "Phase")

ggsave("Proz_Entwicklung_Zaehlstelle_Phase.png", width=20, height=15, units="cm", dpi=1000, 
       path = "fallstudie_s/results/")  

# # selektiere absolute Zahlen
# # behalte rel. Spalten (nur die relativen Prozentangaben)
# mean_phase_d_abs <- mean_phase_d |> dplyr::select(-c(Total, Proz_IN, Proz_OUT))
# 
# # transformiere fuer Plotting
# mean_phase_d_abs <- pivot_longer(mean_phase_d_abs, cols = c("IN","OUT"), 
#              names_to = "Gruppe", values_to = "Durchschnitt")
# 
# # selektiere relative Zahlen
# # behalte rel. Spalten (nur die relativen Prozentangaben)
# mean_phase_d_proz <- mean_phase_d |> dplyr::select(-c(Total:OUT))
# 
# # transformiere fuer Plotting
# mean_phase_d_proz <- pivot_longer(mean_phase_d_proz, cols = c("Proz_IN","Proz_OUT"), 
#                                   names_to = "Gruppe", values_to = "Durchschnitt")
# 
# # Visualisierung abs
# abs <- ggplot(data = mean_phase_d_abs, mapping = aes(x = Gruppe, y = Durchschnitt, fill = Phase))+
#   geom_col(position = "dodge", width = 0.8)+
#   scale_fill_manual(values = c("lightgray","royalblue", "red4", "orangered", "gold2"), name = "Phase")+
#   scale_x_discrete(labels = c("IN", "OUT"))+
#   labs(y = "Durchschnitt [mean]", x= "Bewegungsrichtung")+
#   theme_classic(base_size = 15)+
#   theme(legend.position = "bottom")
# 
# # Visualisierung %
# proz <- ggplot(data = mean_phase_d_proz, mapping = aes(x = Gruppe, y = Durchschnitt, fill = Phase))+
#   geom_col(position = "dodge", width = 0.8)+
#   scale_fill_manual(values = c("lightgray","royalblue", "red4", "orangered", "gold2"), name = "Phase")+
#   scale_x_discrete(labels = c("IN", "OUT"))+
#   labs(y = "Durchschnitt [%]", x= "Bewegungsrichtung")+
#   theme_classic(base_size = 15)+
#   theme(legend.position = "bottom")
# 
# # Arrange und Export Verteilung
# ggarrange(abs,            # plot 1 aufrufen
#           proz,            # plot 2 aufrufen
#           ncol = 2, nrow = 1,    # definieren, wie die plots angeordnet werden
#           heights = c(1),        # beide sind bleich hoch
#           widths = c(1,0.95),    # plot 2 ist aufgrund der fehlenden y-achsenbesch. etwas schmaler
#           labels = c("a) Absolute Verteilung", "b) Relative Verteilung"),
#           label.x = 0,        # wo stehen die labels
#           label.y = 1.0,
#           common.legend = TRUE, legend = "bottom") # wir brauchen nur eine Legende, unten
# 
# ggsave("Verteilung.png", width=20, height=15, units="cm", dpi=1000,
#        path = "fallstudie_s/results/")


#.################################################################################################
# 4. MULTIFAKTORIELLE ANALYSE UND VISUALISIERUNG #####
#.################################################################################################

# 4.1 Einflussfaktoren Besucherzahl ####
# Erstelle ein df indem die taeglichen Zaehldaten und Meteodaten vereint sind
umwelt <- inner_join(depo_d, meteo, by = c("Datum" = "time"))

# Wir muessen unserem Daten noch zuweisen, ob Ferienzeit oder nicht. Das machen wir mit einer Funktion
# erstelle zuerst ein dataframe zur Zuweisung der Ferien # credits Melina Grether

Start <- c(Winterferien_2016_start, Fruehlingsferien_2017_start, Sommerferien_2017_start, Herbstferien_2017_start, 
           Winterferien_2017_start, Fruehlingsferien_2018_start, Sommerferien_2018_start, Herbstferien_2018_start,
           Winterferien_2019_start, Fruehlingsferien_2019_start, Sommerferien_2019_start, Herbstferien_2019_start,
           Winterferien_2020_start, Fruehlingsferien_2020_start, Sommerferien_2020_start, Herbstferien_2020_start,
           Winterferien_2021_start, Fruehlingsferien_2021_start, Sommerferien_2021_start, Herbstferien_2021_start,
           Winterferien_2022_start, Fruehlingsferien_2022_start, Sommerferien_2022_start, Herbstferien_2022_start)
End <- c(Winterferien_2016_ende, Fruehlingsferien_2017_ende, Sommerferien_2017_ende, Herbstferien_2017_ende, 
         Winterferien_2017_ende, Fruehlingsferien_2018_ende, Sommerferien_2018_ende, Herbstferien_2018_ende,
         Winterferien_2019_ende, Fruehlingsferien_2019_ende, Sommerferien_2019_ende, Herbstferien_2019_ende,
         Winterferien_2020_ende, Fruehlingsferien_2020_ende, Sommerferien_2020_ende, Herbstferien_2020_ende,
         Winterferien_2021_ende, Fruehlingsferien_2021_ende, Sommerferien_2021_ende, Herbstferien_2021_ende,
         Winterferien_2022_ende, Fruehlingsferien_2022_ende, Sommerferien_2022_ende, Herbstferien_2022_ende)

# verbinde das zu einem df
ferien <- data.frame(Start, End)

# schreibe nun eine Funktion zur zuweisung Ferien. WENN groesser als start UND kleiner als
# ende, DANN schreibe ein 1
for (i in 1:nrow(ferien)){
  umwelt$Ferien[umwelt$Datum >= ferien[i,"Start"] & umwelt$Datum <= ferien[i,"End"]] <- 1
}
umwelt$Ferien[is.na(umwelt$Ferien)] <- 0

# hat das funktioniert? zaehle die anzahl Ferientage
sum(umwelt$Ferien)

# Faktor und integer
# Im GLMM wird das Jahr als random factor definiert. Dazu muss es als
# Faktor vorliegen. Monat und KW koennen die Besuchszahlen auch erklaeren.
# auch sie muessen faktoren sein
umwelt <- umwelt |> 
  mutate(Jahr = as.factor(Jahr)) |> 
  mutate(KW = as.factor(KW)) |>
  mutate(Monat = as.factor(Monat)) |>
  # zudem muessen die die nummerischen Wetterdaten auch als solche abgespeichert sein
  mutate(tre200nx = as.numeric(tre200nx))|>
  mutate(tre200jx = as.numeric(tre200jx))|>
  mutate(rre150j0 = as.numeric(rre150j0))|>
  mutate(rre150n0 = as.numeric(rre150n0))|>
  mutate(sremaxdv = as.numeric(sremaxdv))

# falls das noch zu NA's gefuehrt hat, muessen diese entfernt werden
sum(is.na(umwelt))
umwelt <- na.omit(umwelt)
summary(umwelt)
str(umwelt)

# Unser Modell kann nur mit ganzen Zahlen umgehen. Zum Glueck habe wir die Zaehldaten
# bereits gerundet.

# unser Datensatz muss ein df sein, damit scale funktioniert
umwelt <- as.data.frame(umwelt)

#  Variablen skalieren
# Skalieren der Variablen, damit ihr Einfluss vergleichbar wird 
# (Problem verschiedene Skalen der Variablen (bspw. Temperatur in Grad Celsius, 
# Niederschlag in Millimeter und Sonnenscheindauer in Minuten)
umwelt <- umwelt |> 
  mutate(tre200jx_scaled = scale(tre200jx), 
         tre200nx_scaled = scale(tre200nx),
         rre150j0_scaled = scale(rre150j0), 
         rre150n0_scaled = scale(rre150n0), 
         sremaxdv_scaled = scale(sremaxdv))

# 4.2 Variablenselektion ####
# Korrelierende Variablen koennen das Modelergebnis verfaelschen. Daher muss vor der
# Modelldefinition auf Korrelation getestet werden.

# Erklaerende Variablen definieren
# Hier wird die Korrelation zwischen den (nummerischen) erklaerenden Variablen berechnet
cor <-  cor(umwelt[,12:16]) # in den [] waehle ich die skalierten Spalten.
# Mit dem folgenden Code kann eine simple Korrelationsmatrix aufgebaut werden
# hier kann auch die Schwelle für die Korrelation gesetzt werden, 
# 0.7 ist liberal / 0.5 konservativ
# https://researchbasics.education.uconn.edu/r_critical_value_table/
cor[abs(cor)<0.7] <-  0 #Setzt alle Werte kleiner 0.7 auf 0 (diese sind dann ok, alles groesser ist problematisch!)
cor

# Korrelationsmatrix erstellen
# Zur Visualisierung kann ein einfacher Plot erstellt werden:
chart.Correlation(umwelt[,12:16], histogram=TRUE, pch=19)

# ich schliesse die Temperatur bei Nacht in den Modellen aufgrund der Korelation aus, 
# da ich davon ausgehe, dass die Temperatur bei Tag das Besuchsaufkommen besser erklaert

# Automatisierte Variablenselektion (achtung, RECHENINTENSIV)
# fuehre die dredge-Funktion und ein Modelaveraging durch
# Hier wird die Formel für die dredge-Funktion vorbereitet
f <- Total ~ Wochentag + Ferien + Phase + Monat + 
  tre200jx_scaled + rre150j0_scaled + rre150n0_scaled + 
  sremaxdv_scaled
# Jetzt kommt der Random-Factor hinzu und es wird eine Formel daraus gemacht
f_dredge <- paste(c(f, "+ (1|Jahr)"), collapse = " ") |>
  as.formula()
# Das Modell mit dieser Formel ausführen
m <- glmer.nb(f_dredge, data = umwelt, na.action = "na.fail")
# Das Modell in die dredge-Funktion einfügen (siehe auch ?dredge)
all_m <- dredge(m)
# suche das beste Modell
print(all_m)
# Importance values der Variablen
# hier wird die wichtigkeit der Variablen in den verschiedenen Modellen abgelesen
MuMIn::sw(all_m)

# Schliesslich wird ein Modelaverage durchgeführt
# Schwellenwert für das delta-AIC = 2
avgmodel <- model.avg(all_m, rank = "AICc", subset = delta < 2)
summary(avgmodel)

# 4.3 Pruefe Verteilung ####
# pruefe zuerst nochmals, ob wir NA im df haben:
sum(is.na(umwelt$Total))

f1<-fitdist(umwelt$Total,"norm")  # Normalverteilung
f1_1<-fitdist((umwelt$Total + 1),"lnorm")  # log-Normalvert (beachte, dass ich +1 rechne. 
# log muss positiv sein; allerdings kann man die
# Verteilungen dann nicht mehr miteinander vergleichen). 
f2<-fitdist(umwelt$Total,"pois")  # Poisson
f3<-fitdist(umwelt$Total,"nbinom")  # negativ binomial
f4<-fitdist(umwelt$Total,"exp")  # exponentiell
# f5<-fitdist(umwelt$Total,"gamma")  # gamma (berechnung mit meinen Daten nicht möglich)
f6<-fitdist(umwelt$Total,"logis")  # logistisch
f7<-fitdist(umwelt$Total,"geom")  # geometrisch
# f8<-fitdist(umwelt$Total,"weibull")  # Weibull (berechnung mit meinen Daten nicht möglich)

gofstat(list(f1,f2,f3,f4,f6,f7), 
        fitnames = c("Normalverteilung", "Poisson",
                     "negativ binomial","exponentiell", "logistisch",
                     "geometrisch"))

# die 2 besten (gemaess Akaike's Information Criterion) als Plot + normalverteilt, 
plot.legend <- c("Normalverteilung", "exponentiell", "negativ binomial")
# vergleicht mehrere theoretische Verteilungen mit den empirischen Daten
cdfcomp(list(f1, f4, f3), legendtext = plot.legend)

# --> Verteilung ist gemäss AICc exponentiell. negativ binomial ist auch nicht schlecht.
# --> ich entscheide mich für diese beide und probiere mit beiden Modelle aus.

# 4.4 Berechne verschiedene Modelle ####

# Hinweise zu GLMM: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# Ich verwende hier die Funktion glmer aus der Bibliothek lme4. 
# Die Totale Besucheranzahl soll durch verschiedene Parameter erklaert werden. 
# Die verschiedenen Jahre sollen hierbei nicht beachtet werden, 
# sie wird als random Faktor bestimmt --> Wir betrachten jedes Jahr für sich und nicht
# den allgemeinen Trend

# Einfacher Start
# Auch wenn wir gerade herausgefunden haben, dass die Verteilung negativ binomial ist,
# berechne ich für den Vergleich zuerst ein einfaches Modell der Familie poisson.
Tages_Model <- glmer(Total ~ Wochentag + Ferien + Phase + Monat + 
                       tre200jx_scaled + rre150j0_scaled + rre150n0_scaled + 
                       sremaxdv_scaled +
                       (1|Jahr), family = poisson, data = umwelt)

summary(Tages_Model)
# Inspektionsplots
plot(Tages_Model, type = c("p", "smooth"))
qqmath(Tages_Model)
# pruefe auf Overdispersion
dispersion_glmer(Tages_Model) #it shouldn't be over 1.4
# wir gut erklaert das Modell?
r.squaredGLMM(Tages_Model) 
# check for multicollinearity
# https://rforpoliticalscience.com/2020/08/03/check-for-multicollinearity-with-the-car-package-in-r/
car::vif(Tages_Model) # VIF für beide predictors = 1, d.h. voneinander unabhängig (kritisch wird es ab einem Wert von >4-5)

# Berechne ein negativ binomiales Modell
# gemäss AICc die zweitbeste Verteilung
Tages_Model_nb <- glmer.nb(Total ~ Wochentag + Ferien + Phase + Monat + 
                             tre200jx_scaled + rre150j0_scaled + rre150n0_scaled + 
                             sremaxdv_scaled +
                             (1|Jahr), data = umwelt)

summary(Tages_Model_nb)
plot(Tages_Model_nb, type = c("p", "smooth"))
qqmath(Tages_Model_nb)
dispersion_glmer(Tages_Model_nb)
r.squaredGLMM(Tages_Model_nb) 
car::vif(Tages_Model_nb)

# auf quadratischen Term testen ("es gehen weniger Leute in den Wald, wenn es zu heiss ist")
Tages_Model_nb_quad <- glmer.nb(Total ~ Wochentag + Ferien + Phase + Monat + 
                                  tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled + 
                                  sremaxdv_scaled +
                                  (1|Jahr), data = umwelt)

summary(Tages_Model_nb_quad)
plot(Tages_Model_nb_quad, type = c("p", "smooth"))
qqmath(Tages_Model_nb_quad)
dispersion_glmer(Tages_Model_nb_quad)
r.squaredGLMM(Tages_Model_nb_quad) 
car::vif(Tages_Model_nb_quad)

# Interaktion testen, da Ferien und / oder Wochentage einen Einfluss auf
# die Besuchszahlen waehrend des Lockown haben koennen!
# (Achtung: Rechenintensiv!)
# Tages_Model_nb_int <- glmer.nb(Anzahl_Total ~  Wochentag  * Ferien + Phase +
#                                  tre200jx_scaled + I(tre200jx_scaled^2) * 
#                                  rre150j0_scaled + sremaxdv_scaled +
#                                  (1|KW) + (1|Jahr), data = umwelt)
# 
# summary(Tages_Model_nb_int)
# plot(Tages_Model_nb_int, type = c("p", "smooth"))
# qqmath(Tages_Model_nb_int)
# dispersion_glmer(Tages_Model_nb_int)
# r.squaredGLMM(Tages_Model_nb_int) 


# Vergleich der Modellguete mittels AICc
cand.models<-list()
cand.models[[1]] <- Tages_Model
cand.models[[2]] <- Tages_Model_nb
cand.models[[3]] <- Tages_Model_nb_quad

Modnames<-c("Tages_Model","Tages_Model_nb", 
            "Tages_Model_nb_quad")
aictab(cand.set=cand.models,modnames=Modnames)
#K = Anzahl geschaetzter Parameter (2 Funktionsparameter und die Varianz)
#Delta_AICc <2 = Statistisch gleichwertig
#AICcWt =  Akaike weight in %

# --> Ich entscheide mich bei diesen drei Modellen für das Tages_Model_nb_quad
# Warum: statistisch das beste und ich denke die Quadratur macht Sinn!
# zudem wissen wir gem. Test der Verteilungen, dass negativ binomial Sinn macht.
# PROBLEM: alle drei Modelle erfüllen gem. der Modelldiagnostik die VOrausetzungen 
# nicht komplett.

# Berechne ein Modell mit exponentieller Verteilung:
# gemäss AICc der Verteilung die zweitbeste
# https://stats.stackexchange.com/questions/240455/fitting-exponential-regression-model-by-mle
Tages_Model_exp <- glmer((Total+1) ~ Wochentag + Ferien + Phase + Monat + 
                           tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled + 
                           sremaxdv_scaled + (1|Jahr), family = Gamma(link="log"), data = umwelt)

summary(Tages_Model_exp, dispersion=1)
plot(Tages_Model_exp, type = c("p", "smooth"))
qqmath(Tages_Model_exp)
dispersion_glmer(Tages_Model_exp) #it shouldn't be over 1.4
r.squaredGLMM(Tages_Model_exp) 
car::vif(Tages_Model_nb_quad)

# --> Die zweitbeste Verteilung (exp) führt auch nicht dazu, dass die Modellvoraussetzungen besser
# erfüllt werden


# 4.5 Transformationen ####
# Die Modellvoraussetzungen waren überall mehr oder weniger verletzt.
# Das ist ein Problem, allerdings auch nicht ein so grosses.
# (man sollte es aber trotzdem ernst nehmen)
# Schielzeth et al. Robustness of linear mixed‐effects models to violations of distributional assumptions
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13434
# Lo and Andrews, To transform or not to transform: using generalized linear mixed models to analyse reaction time data
# https://www.frontiersin.org/articles/10.3389/fpsyg.2015.01171/full

# die Lösung ist nun, die Daten zu transformieren:
# mehr unter: https://www.datanovia.com/en/lessons/transform-data-to-normal-distribution-in-r/

# berechne skewness coefficient 
library(moments)
skewness(umwelt$Total)
# A positive value means the distribution is positively skewed (rechtsschief).
# The most frequent values are low; tail is toward the high values (on the right-hand side)

# log 10, da stark rechtsschief
Tages_Model_quad_Jahr_log10 <- lmer(log10(Total+1) ~ Wochentag + Ferien + Phase + Monat + 
                                      tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled + 
                                      sremaxdv_scaled + (1|Jahr), data = umwelt)
summary(Tages_Model_quad_Jahr_log10)
plot(Tages_Model_quad_Jahr_log10, type = c("p", "smooth"))
qqmath(Tages_Model_quad_Jahr_log10)
dispersion_glmer(Tages_Model_quad_Jahr_log10)
r.squaredGLMM(Tages_Model_quad_Jahr_log10) 
car::vif(Tages_Model_nb_quad)
# lmer zeigt keine p-Werte, da diese schwer zu berechnen sind. Alternative Packages berechnen diese
# anhand der Teststatistik. Achtung: die Werte sind wahrscheinlich nicht präzise!
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000904.html
tab_model(Tages_Model_quad_Jahr_log10, transform = NULL, show.se = TRUE)


# natural log, da stark rechtsschief
Tages_Model_quad_Jahr_ln <- lmer(log(Total+1) ~ Wochentag + Ferien + Phase + Monat + 
                                   tre200jx_scaled + I(tre200jx_scaled^2) + rre150j0_scaled + rre150n0_scaled + 
                                   sremaxdv_scaled + (1|Jahr), data = umwelt)
summary(Tages_Model_quad_Jahr_ln)
plot(Tages_Model_quad_Jahr_ln, type = c("p", "smooth"))
qqmath(Tages_Model_quad_Jahr_ln)
dispersion_glmer(Tages_Model_quad_Jahr_ln)
r.squaredGLMM(Tages_Model_quad_Jahr_ln) 
car::vif(Tages_Model_nb_quad)

# --> Die Modellvoraussetzungen sind nicht deutlich besser erfüllt jetzt wo wir Transformationen 
# benutzt haben. log10 und ln performen beide etwa gleich.

# Zusatz: ACHTUNG - Ruecktransformierte Regressionskoeffizienten zu erlangen (fuer die Interpretation, das Plotten), 
# ist zudem nicht moeglich (Regressionskoeffizienten sind nur im transformierten Raum linear). 
# Ein ruecktransformierter Regressionskoeffiziente haette eine nicht-lineare Beziehung mit der 
# abhaengigen Variable.


# 4.6 Exportiere die Modellresultate ####
# (des besten Modells)
tab_model(Tages_Model_nb_quad, transform = NULL, show.se = TRUE)
# The marginal R squared values are those associated with your fixed effects, 
# the conditional ones are those of your fixed effects plus the random effects. 
# Usually we will be interested in the marginal effects.


# 4.7 Visualisiere Modellresultate ####

# Hintergrundinfo interaction plot:
# https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html

# definiere eine Funktion zum Plotten der Modellergebnisse
# Credits function to Sabrina Harsch

# original
# rescale_plot <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, x_scaling, x_nk) {
#   
#   plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title="")
#   labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks+1)*x_scaling, x_nk)
#   
#   custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var)-min(scaled_var))/num_breaks))
#   custom_limits <- c(min(scaled_var), max(scaled_var))
#   
#   plot_id <- plot_id +
#     scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x=x_lab)) +
#     scale_y_continuous(name=NULL, labels = scales::percent_format(accuracy = 5L), limits = c(0,1),position = "left") +
#     theme_classic()
#   
#   return(plot_id)
# }


rescale_plot_num <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title="")
  labels <- round(seq(floor(min(unscaled_var)), ceiling(max(unscaled_var)), length.out = num_breaks+1)*x_scaling, x_nk)
  
  custom_breaks <- seq(min(scaled_var), max(scaled_var), by = ((max(scaled_var)-min(scaled_var))/num_breaks))
  custom_limits <- c(min(scaled_var), max(scaled_var))
  
  plot_id <- plot_id +
    scale_x_continuous(breaks = custom_breaks, limits = custom_limits, labels = c(labels), labs(x=x_lab)) +
    scale_y_continuous(labs(y=y_lab), limits = c(0,50)) +
    theme_classic(base_size = 20)
  
  return(plot_id)
}

## Tagesmaximaltemperatur
input_df     <-  Tages_Model_nb_quad
input_term   <- "tre200jx_scaled [all]"
unscaled_var <- umwelt$tre200jx
scaled_var   <- umwelt$tre200jx_scaled
num_breaks   <- 10
x_lab        <- "Temperatur [°C]"
y_lab        <- "Fussgänger:innen pro Tag"
x_scaling    <- 1 # in prozent
x_nk         <- 0   # x round nachkommastellen    


p_temp <- rescale_plot_num(input_df, input_term, unscaled_var, scaled_var, num_breaks, 
                         x_lab, y_lab, x_scaling, x_nk)
p_temp

ggsave("temp.png", width=15, height=15, units="cm", dpi=1000, 
       path = "fallstudie_s/results/") 


## Wochentag

rescale_plot_fac <- function(input_df, input_term, unscaled_var, scaled_var, num_breaks, x_lab, y_lab, x_scaling, x_nk) {
  
  plot_id <- plot_model(input_df, type = "pred", terms = input_term, axis.title = "", title="")

  plot_id <- plot_id +
    scale_y_continuous(labs(y=y_lab), limits = c(0,50)) +
    theme_classic(base_size = 20)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  return(plot_id)
}




input_df     <-  Tages_Model_nb_quad
input_term   <- "Wochentag [all]"
unscaled_var <- umwelt$Wochentag
scaled_var   <- umwelt$Wochentag
num_breaks   <- 10
x_lab        <- "Wochentag"
y_lab        <- "Fussgänger:innen pro Tag"
x_scaling    <- 1 # in prozent
x_nk         <- 0   # x round nachkommastellen    


p_wd <- rescale_plot_fac(input_df, input_term, unscaled_var, scaled_var, num_breaks, 
                              x_lab, y_lab, x_scaling, x_nk)
p_wd

ggsave("wd.png", width=15, height=15, units="cm", dpi=1000, 
       path = "fallstudie_s/results/") 















# ZUSATZ: Wir haben die Wetterparameter skaliert. 
# Fuer die Plots muss das beruecksichtigt werden: wir stellen nicht die wirklichen Werte
# dar sondern die skalierten. Mit folgendem Befehl kann man die Skalierung nachvollziehen:
# attributes(umwelt$tre200jx_scaled)
# Die Skalierung kann rueckgaengig gemacht werden, indem man die Skalierten werte mit
# dem scaling factor multipliziert und dann den Durchschnitt addiert:
# Bsp.: d$s.x * attr(d$s.x, 'scaled:scale') + attr(d$s.x, 'scaled:center')
# mehr dazu: https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
# --> wir bleiben aber bei den skalierten Werten, leben damit und sind uns dessen bewusst.

# Auch beim Plotten der Modellresultate gilt: 
# visualisiere nur die Parameter welche nach der Modellselektion uebig bleiben
# und signifikant sind!
# plot_model / type = "pred" sagt die Werte "voraus"
# achte auf gleiche Skalierung der y-Achse (Vergleichbarkeit)

# 
# # Temperatur
# t <- plot_model(Tages_Model_quad_Jahr_log10, type = "pred", terms = 
#                   "tre200jx_scaled [all]", # [all] = Model contains polynomial or cubic / 
#                 #quadratic terms. Consider using `terms="tre200jx_scaled [all]"` 
#                 # to get smooth plots. See also package-vignette 
#                 # 'Marginal Effects at Specific Values'.
#                 title = "", axis.title = c("Tagesmaximaltemperatur [°C]", 
#                                            "Fussgaenger:innen pro Tag [log]"))
# # fuege die Achsenbeschriftung hinzu. Hier wird auf die unskalierten Werte zugegriffen.
# labels <- round(seq(floor(min(umwelt$tre200jx)), ceiling(max(umwelt$tre200jx)),
#                     # length.out = ___ --> Anpassen gemaess breaks auf dem Plot
#                     length.out = 5), 0) 
# (Tempplot <- t + 
#     scale_x_continuous(breaks = c(-2,-1,0,1,2), 
#                        labels = c(labels))+
#     # fuege die y- Achsenbeschriftung hinzu. Hier transformieren wir die Werte zurueck
#     scale_y_continuous(breaks = c(0,0.5,1,1.5,2),
#                        labels = round(c(10^0, 10^0.5, 10^1, 10^1.5, 10^2),0),
#                        limits = c(0, 2))+ 
#     theme_classic(base_size = 20))


