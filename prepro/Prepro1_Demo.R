x <- 10.3

x

typeof(x)

y = 7.3

y

z <- 42
typeof(z)
is.integer(z)
is.numeric(z)
is.double(z)

a <- as.integer(z)
is.numeric(a)
is.integer(a)

c <- 8L
is.numeric(c)
is.integer(c)

typeof(a)

is.numeric(a)
is.integer(a)

vector <- c(10,20,33,42,54,66,77)
vector
vector[5]
vector[2:4]

vector2 <- vector[2:4]

a <- as.integer(7)
b <- as.integer(3.14)
a
b
typeof(a)
typeof(b)
is.integer(a)
is.integer(b)

c <- as.integer("3.14")
c
typeof(c)

e <- 3
f <- 6
g <- e > f
e
f
g
typeof(g)

sonnig <- TRUE
trocken <- FALSE

sonnig & !trocken

u <- TRUE
v <- !u 
v

s <- as.character(3.14)
s
typeof(s)

fname <- "Hans"
lname <- "Muster"
paste(fname,lname)

fname2 <- "hans"
fname == fname2

wochentage <- c("Donnerstag","Freitag","Samstag","Sonntag","Montag","Dienstag","Mittwoch",
                "Donnerstag","Freitag","Samstag","Sonntag", "Montag","Dienstag","Mittwoch")

typeof(wochentage)

wochentage_fac <- as.factor(wochentage)

wochentage
wochentage_fac

factor(wochentage, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"), ordered = TRUE)

datum <- "2017-10-01 13:45:10"

# konvertiert character in POSIXct:
as.POSIXct(datum) 

datum <- "01.10.2017 13:45"

# konvertiert character in POSIXct:
as.POSIXct(datum,format = "%d.%m.%Y %H:%M")

datum <- as.POSIXct(datum,format = "%d.%m.%Y %H:%M")

strftime(datum, format = "%m") # extrahiert den Monat als Zahl
strftime(datum, format = "%b") # extrahiert den Monat mit Namen (abgekürzt)
strftime(datum, format = "%B") # extrahiert den Monat mit Namen (ausgeschrieben)

library(lubridate)

month(datum)                             # extrahiert den Monat als Zahl
month(datum, label = TRUE, abbr = TRUE)  # extrahiert den Monat mit Namen (abgekürzt)
month(datum, label = TRUE, abbr = FALSE) # extrahiert den Monat mit Namen (ausgeschrieben)

df <- data.frame(
  Stadt = c("Zürich","Genf","Basel","Bern","Lausanne"),
  Einwohner = c(396027,194565,175131,140634,135629),
  Ankunft = c("1.1.2017 10:00","1.1.2017 14:00",
              "1.1.2017 13:00","1.1.2017 18:00","1.1.2017 21:00")
)

str(df)

df$Einwohner <- as.integer(df$Einwohner)

df$Einwohner

df$Ankunft <- as.POSIXct(df$Ankunft, format = "%d.%m.%Y %H:%M")

df$Ankunft

df$Groesse[df$Einwohner > 300000] <- "gross"
df$Groesse[df$Einwohner <= 300000 & df$Einwohner > 150000] <- "mittel"
df$Groesse[df$Einwohner <= 150000] <- "klein"

df$Groesse

df$Ankunft_stunde <- hour(df$Ankunft)

df$Ankunft_stunde
