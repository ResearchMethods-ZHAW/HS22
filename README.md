

## Abstract

Im Kurs Research Methods verwenden wir seit einigen Jahren RMarkdown um die R Unterlagen für die Studenten bereit zu stellen. 

Seit HS2020 arbeiten wir mit Quarto, der Nachfolger von Rmarkdown. Am besten Nils und Dominik machen mit euch eine kleine Einführung dazu. Die Slides sind hier zu finden: <https://researchmethods-zhaw.github.io/Intro-for-Authors/>





## Anleitung 1: Software Aufsetzen

### R, RStudio und Git installieren

*(wer dies bereits gemacht hat oder auf dem RStudio Server arbeitet, kann diesen Schritt überspringen)*

Wer Lokal auf seinem eingenen PC arbeiten will, muss eine aktuell version von R, RStudio und Git installieren. Siehe dazu folgende Anleitungen:

-   [happygitwithr: Install or upgrade R and RStudio](https://happygitwithr.com/install-r-rstudio.html)
-   [happygitwithr: Install Git](https://happygitwithr.com/install-git.html)

### RStudio Konfigurieren

Ich empfehle folgende Konfiguration in RStudio (`Global Options`):

-   R Markdown
    -   Show document outline by default: checked *(Stellt ein Inhaltsverzeichnis rechts von .Rmd files dar)*
    -   Soft-wrap R Markdown files: checken *(macht autmatische Zeilenumbrüche bei .Rmd files)*
    -   Show in document outline: Sections Only *(zeigt nur "Sections" im Inhaltsverzeichnis)*
    -   Show output preview in: Window *(beim kopilieren von Rmd Files wird im Anschluss ein Popup mit dem Resultat dargestellt)*
    -   Show equation an image previews: In a popup
    -   Evaluate chunks in directory: Document (**\<- wichtig !**)
-   Code \> Tab "Saving"
    -   Default Text Encoding: UTF-8 (**\<- wichtig !**)

### Git konfigurieren

*(wer dies bereits gemacht hat, kann diesen Schritt überspringen)*

Nach der Installation muss git konfiguriert werden. Siehe dazu folgende Kapitel:

-   [happygitwithr: Introduce yourself to Git](https://happygitwithr.com/hello-git.html)
-   [happygitwithr: Cache credentials for HTTPS](https://happygitwithr.com/credential-caching.html)

## Anleitung 2: Projekt aufsetzen

### Repo Klonen

Um die ganzen \*.Rmd Files lokal bearbeiten zu können, muss das Repository geklont werden. Mit RStudio ist dies sehr einfach, siehe dazu nachstehende Anleitung. Als Repo-URL folgendes einfügen: `https://github.com/ResearchMethods-ZHAW/HS22.git`

-   [happygitwithr: New RStudio Project via git clone](https://happygitwithr.com/new-github-first.html#new-rstudio-project-via-git)

### "Upstream" setzen

Um das Github repo als standart "upstream" zu setzen muss man im Terminal nachstehenden Befehl eingeben. Danach RStudio neu starten und das entsprechende Projekt laden. Nun sollte im "Git" fenster der "Push" button nicht mehr inaktiv sein.

    git branch -u origin/main


## Anleitung 3: Inhalte Editieren und veröffentlichen


### Qmd erstellen

Die meisten Inhalte exisitieren bereits und ihr müsst sie nur noch anpassen. Falls ihr aber ein neues .Qmd File erstellen möchtet, müsst ihr einen Unterordner in einem der Ordner erstellen. 


### Qmd editieren

Um Inhalte zu editieren, öffnet ihr das entsprechende .Rmd file in einem der Ordner `prepro`, `infovis`, `rauman` usw.. Ihr könnt dieses File wie ein reguläres, eigenständiges .Qmd File handhaben. **Wichtig**: Alle Pfade im Dokument sind relativ zum Project zu verstehen: **Das Working directory ist der Project folder!!**.

### Qmd Kompilieren / Vorschau

```
quarto preview
```

oder 

```
quarto.com preview
```


### Änderungen veröffentlichen

Um die Änderungen zu veröffentlichen (für die Studenten sichtbar zu machen) müsst ihr diese via git auf das Repository "pushen". Vorher aber müsst ihr die Änderungen `stage`-en und `commit`-en. Ich empfehle, dass ihr zumindest zu beginn mit dem RStudio "Git" Fenster arbeitet.

-   `stage`: Setzen eines Häckchens bei "Staged" (im Terminal mit `git add .`)
-   `commit`: Klick auf den Button "commit" (im Terminal mit `git commit -m "deine message"`)
-   `pull`: Klick auf den Button "Pull" (im Terminal mit `git pull`)
-   `push`: Click auf den button "Push" (im Terminal mit `git push`)


## Todo's

- library(tidyverse) vermeiden
- Funktion dpkg vermeinden, wenn das geht
- case_when statt verschachtelte ifelse
- schulferien ersetzen mit: https://data.stadt-zuerich.ch/dataset/ssd_schulferien/download/schulferien.csv (https://opendata.swiss/de/dataset/ferien-und-schulfreie-tage-der-volksschule-der-stadt-zurich-schuljahre-2020-2021-bis-2024-2025/resource/9a085e52-0d7b-4565-b839-5155c2e6a50c)
- multiple mutate in einem befehl
- sf, raster, tmap möglichst einheitlich und wenn möglich nach rauman1-5
- implement submodule datasets
