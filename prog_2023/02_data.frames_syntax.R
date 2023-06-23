## funktion(objektname1,
##          option1 = sehr_lange_Auswahl_die_sehr_lang_ist,
##          option2 = noch_eine_Auswahl_Option2)

# data.frame ------
studs <- c(19173,5333,15643)    # Studierendenzahlen unter "studs" ablegen 
profs       <- c(322,67,210)    # Prof-Zahlen unter "profs" ablegen
dat1_orig <- data.frame(studs, profs)
dat1_orig

dat1 <- data.frame(studs = c(19173,5333,15643), 
                   profs = c(322,67,210),
                   gegr  = c(1971,1830,1973)) # ohne zwischen-Objekte
dat1    # zeigt den kompletten Datensatz an

dat1$profs 

colnames(dat1) ## Variablen-/Spaltennamen anzeigen
names(dat1) ## Variablen-/Spaltennamen anzeigen
ncol(dat1) ## Anzahl der Spalten/Variablen
nrow(dat1) ## Anzahl der Zeilen/Fälle

# neue Variable erstellen
dat1$stu_prof <- dat1$studs/dat1$profs
## dat1 hat also nun eine Spalte mehr:
ncol(dat1) 
dat1
# auch aus anderen data.frames 
dat1$stu_prof <- dat1_orig$studs/dat1_orig$profs


dat1$uni <- c("Uni Bremen","Uni Vechta", "Uni Oldenburg")
dat1

# Variablentypen ------
class(dat1$profs)
class(dat1$uni)
is.numeric(dat1$profs)
is.character(dat1$profs)

as.character(dat1$profs) ## die "" zeigen an, dass die Variable als character definiert ist

class(dat1$profs)

dat1$profs <- as.character(dat1$profs)

dat1

dat1$profs 
class(dat1$profs)

dat1$profs / 2 

as.numeric(dat1$profs)
as.numeric(dat1$profs) / 2

as.numeric(dat1$uni)

# Navigation in einem data.frame
dat1 # vollständiger Datensatz
dat1[1,1] # erste Zeile, erste Spalte
dat1[1,]  # erste Zeile, alle Spalten
dat1[,1]  # alle Zeilen, erste Spalte (entspricht hier dat1$studs)
dat1[,"studs"] # alle Zeilen, Spalte mit Namen studs -> achtung: ""

dat1[dat1$studs > 10000, ] # Zeilen in denen studs größer 10000, alle Spalten
dat1[dat1$studs > 10000 & dat1$profs < 300, ] # & bedeutet UND

dat1$profs[dat1$studs > 10000] # Nur Prof-Zahl nachsehen: kein Komma 

dat1[dat1$studs > 10000, c("profs","uni")] #  Prof- &  Uni nachsehen

# vektor hinzufügen
werte <- c(12,12,34)
dat1$neue_werte <- werte


## Übung --------

# Pakete ----
#install.packages("Paket") # auf eurem PC nur einmal nötig
library(Paket) # nach jedem Neustart nötig

fdz_install("tidyverse")
library(tidyverse) # nach einmaligem fdz_install("tidyverse")

# Zeilen auswählen mit slice() ----
slice(dat1,1) # erste Zeile
slice(dat1,2:3) # Zeile 2-3
slice(dat1,c(1,3)) # Zeile 1 und 3

# Fälle auswählen mit filter() ----
filter(dat1,uni == "Uni Oldenburg", studs > 1000)

dat1 # unverändert

# ablegen 
ueber_10tsd <- filter(dat1, studs > 10000)
ueber_10tsd

# auswahlhilfen
filter(dat1, studs >= 10000)
filter(dat1, studs <= 10000)
filter(dat1,studs > 10000 | profs < 200) # mehr als 10.000 Studierende *oder* weniger als 200 Professuren
filter(dat1, gegr %in% c(1971,1830)) # gegründet 1971 oder 1830
filter(dat1, between(gegr,1830,1971)) # gegründet zwischen 1971 und 1830 (einschließlich)

?select()

# Variablen auswählen mit select() -----
dat1
select(dat1, studs,profs)

select(dat1, 1:3) # Spalte 1-3
select(dat1, c(1,3)) # Spalte 1&3
select(dat1, !profs) # alles außer profs

dat_ohne_profs <- select(dat1, !profs) 
dat_ohne_profs
dat1 # unverändert

select(dat1,contains("u"))
select(dat1,matches("^u"))

select(filter(dat1,studs < 10000),uni)
# strg shift m
filter(dat1,studs < 10000) %>% select(uni)
## Übung --------

# Pipe -----
dat1 %>% filter(.,studs < 10000) %>% select(.,uni)
dat1 %>% filter(studs < 10000) %>% select(uni)

# sortieren mit arrange -------
dat1 %>% arrange(studs)
dat1 %>% arrange(uni)

# Textvariablen sortieren mit factor & arrange -------
factor(dat1$uni, 
       levels = c("Uni Oldenburg", "Uni Bremen", "Uni Vechta"))

factor(dat1$uni, 
       levels = c("Uni Oldenburg", "Uni Bremen"))

dat1$uni_fct <- factor(dat1$uni,
                       levels = c("Uni Oldenburg", "Uni Bremen"))
dat1


class(dat1$uni_fct)
dat1 %>% arrange(uni_fct)

dat1 %>% arrange(desc(uni_fct))

dat1 %>% arrange(desc(uni_fct), gegr, studs)

## Übung --------

# Datensatz einlesen --> Projekte
# Projekte -----------
rstudioapi::initializeProject(path = "D:/Kurse/R-Kurs")
rstudioapi::openProject(path = "D:/Kurse/R-Kurs")
getwd()

# Stata-Datei einlesen: read_dta()
library(haven)
pend <- read_dta("./orig/PENDDAT_cf_W13.dta") 

class(pend)

head(pend)

nrow(pend)
ncol(pend)

# auswahl mit filter()
pend06 <- pend %>% filter(pintjahr == 2006)
nrow(pend06)

## alter aus pend06
pend06$palter


# saveRDS ----------
pend06$palter
saveRDS(pend06, file = "./data/pend06.RData")
rm(pend06)

pend06_neu <- readRDS(fil = "./data/pend06.RData")
head(pend06) # gibt es nicht mehr
head(pend06_neu) # das gibt es

# Was wenn ich alles speichern will? -----
# alles speichern mit ls()
save(list = ls(),file = "./data/meine_Daten.RData")
load(file = "./data/meine_Daten.RData")

# Hilfe zu Paketen -----
vignette("dplyr")
vignette(package = "dplyr")
vignette("rowwise")
help("dplyr")
help(package = "dplyr")


# Hilfe zu Funktionen
?select()
