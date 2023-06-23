# ------------------- #
# Kapitel 3: Tabellen
# Lösung
# ------------------- #
library(haven)
pend <- read_dta("./orig/PENDDAT_cf_W13.dta")


#  Übung 1------------

# Lassen Sie sich eine Tabelle mit den absoluten Häufigkeiten anzeigen, 
library(tidyverse)
table(pend$famstand)
pend %>% count(famstand)

# Überschreiben Sie Missing-Codes mit NA.
pend$famstand[pend$famstand<0] <- NA

# Hat das Überschreiben der Missings mit NA geklappt? Erstellen Sie die Tabelle erneut.
table(pend$famstand) 

# Lassen Sie sich der relativen Häufigkeiten (Anteile) ausgeben (nutzen sie entweder table() oder xtabs())
table(pend$famstand) %>% prop.table(.)

# labels 

pend$famstand2 <- as_factor(pend$famstand) %>% forcats::fct_drop()
table(pend$famstand2) %>% prop.table(.)

#  Übung 2-------------

# Erstellen Sie eine Kontingenztabelle, indem Sie neben famstand auch das Geschlecht zpsex (2 = Frauen, 1 = Männer) mit einbeziehen
table(pend$famstand,pend$zpsex)
table(as_factor(pend$famstand) %>% fct_drop(),
      as_factor(pend$zpsex) %>% fct_drop())

# Wie viel Prozent der Befragten sind ledige Frauen? Berechnen Sie die relativen Häufigkeiten.
table(pend$famstand,pend$zpsex) %>% prop.table() %>% addmargins()

table(as_factor(pend$famstand) %>% fct_drop(),
      as_factor(pend$zpsex) %>% fct_drop()) %>% prop.table() %>% addmargins()
# Wie viel Prozent der befragten Frauen sind ledig? Nutzen Sie die `margin =`-Option
table(pend$famstand,pend$zpsex) %>% prop.table(margin = 2)
table(as_factor(pend$famstand) %>% fct_drop(),
      as_factor(pend$zpsex) %>% fct_drop()) %>% prop.table(margin = 1)
# Wie viel Prozent der ledigen Befragten sind Frauen? Nutzen Sie die `margin =`-Option
table(as_factor(pend$famstand) %>% fct_drop(),
      as_factor(pend$zpsex) %>% fct_drop()) %>%
  prop.table(margin = 2)

# Übung 3 -------
 #Beschreiben Sie das Alter der Befragten (`palter`) mit `summary` und 
# erstellen Sie selbst einen Überblick mit Hilfe von `summarise()`, der einen Vergleich des Befragtenalters nach Familienstand erlaubt.

# Erstellen Sie einen Überblick mit `summary()`
summary(pend$palter)
pend$palter[pend$palter<0] <- NA
summary(pend$palter)


# Erstellen Sie einen Überblick mit dem Minimum, Median, arith. Mittel, Varianz und Maximum der Alterswerte mit Hilfe von `summarise()`
pend %>% 
  summarise(
            min    = min(palter, na.rm = T),
            mean   = mean(palter, na.rm = T),
            median = median(palter, na.rm = T),
            max    = max(palter, na.rm = T),
            var    = var(palter, na.rm = T)
          )

# Erweitern Sie diesen Überblick dann so, dass sie einen Vergleich der Kennzahlen für die verschiedenen `famstand`-Kategorien ausgegeben bekommen.

# mit group_by() oder .by = 
pend %>%
  group_by(famstand) %>% ## entscheidend: group_by()
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T)
  )

pend %>% 
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T),
    .by = famstand) %>% # .by = ab dplyr 1.1.1
  arrange(famstand)

pend %>% 
  filter(famstand > 0) %>% 
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T),
    .by = famstand) %>% # .by = ab dplyr 1.1.1
  arrange(famstand)



# ohne NA: ------------
pend %>% 
  filter(!is.na(famstand)) %>% 
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T),
    .by = famstand) # .by = ab dplyr 1.1.1


# wie runde ich Werte in der Tabelle? ----------------
pend %>% 
  filter(famstand > 0) %>% 
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T) %>% round(.,0),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T),
    .by = famstand) %>% # .by = ab dplyr 1.1.1
  arrange(famstand) %>% 
  data.frame(.)


übersicht <- 
  pend %>% 
  filter(famstand > 0) %>% 
  data.frame(.) %>% 
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T) %>% round(.,0),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T),
    .by = famstand) %>% # .by = ab dplyr 1.1.1
  arrange(famstand) 
  

pend_gefiltert <- 
  pend %>% 
  filter(famstand > 0) %>% 
  data.frame(.)

min(pend_gefiltert$palter, na.rm = T)
min(pend_gefiltert$palter, na.rm = T)

pend_gefiltert %>% 
  summarise(
    min    = min(palter, na.rm = T),
    mean   = mean(palter, na.rm = T) %>% round(.,0),
    median = median(palter, na.rm = T),
    max    = max(palter, na.rm = T),
    var    = var(palter, na.rm = T),
    .by = famstand) %>% # .by = ab dplyr 1.1.1
  arrange(famstand) 
